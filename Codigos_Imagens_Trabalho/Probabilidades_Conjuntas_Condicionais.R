library(readODS)
library(survival)
library(survminer)
library(dplyr)
library(tidyr)
library(AdequacyModel)
library(ggplot2)
library(car)
library(scales)

################################################################
### Organizando dados e calculando parametros do modelo
################################################################

dados <- read_ods("/dados/unb/TCC-2/Dados/Base_saude.ods")

dados <- dados %>%
  mutate(CARREIRAS = CARREIRAS/3,
         TI_TX = TI_EMPENHADO/TI_PROJ_LEI,
         PIB = as.numeric(scale(log(PIB))))

dados <- dados%>%
  filter(POL_ABV %in% c("pnacac","pnaacto"))

dados <- dados%>%
  select(SIGUF,MUNICÍPIO,POL_ABV,t,ADOPT,TX_PARTC,
         TX_SUCESSO,ANO_ELEITORAL_NAC,IDEOL_PRF,
         CARREIRAS,TI_TX,PIB)


dados <- dados %>%
  pivot_wider(
    id_cols = c(SIGUF, MUNICÍPIO),
    names_from = POL_ABV,
    values_from = -c(POL_ABV,SIGUF, MUNICÍPIO),
    names_glue = "{.value}_{POL_ABV}")

t1<-dados$t_pnacac
t2<-dados$t_pnaacto
cens1<-dados$ADOPT_pnacac
cens2<-dados$ADOPT_pnaacto

cov_11 <- dados$TX_SUCESSO_pnacac
cov_12 <- dados$PIB_pnacac
cov_13 <- dados$ANO_ELEITORAL_NAC_pnacac
cov_14 <- dados$CARREIRAS_pnacac


cov_21 <- dados$TX_PARTC_pnaacto
cov_22 <- dados$TI_TX_pnaacto
cov_23 <- dados$PIB_pnaacto
cov_24 <- dados$IDEOL_PRF_pnaacto


ajust1<-survreg(Surv(t1,cens1)~cov_11 + cov_12 + cov_13 + cov_14, 
                dist='lognormal');summary(ajust1)
ajust2<-survreg(Surv(t2,cens2)~cov_21 + cov_22 + cov_23 + cov_24,
                dist='lognormal');summary(ajust2)

verosi_ln_ln <- function(param) { 
  
  beta01 <- param[1] 
  beta11 <- param[2] 
  beta21 <- param[3]
  beta31 <- param[4]
  beta41 <- param[5]
  
  
  beta02 <- param[6] 
  beta12 <- param[7] 
  beta22 <- param[8]
  beta32 <- param[9]
  beta42 <- param[10]
  
  
  sigma1 <- param[11] 
  sigma2 <- param[12] 
  alpha <- param[13] 
  
  mu1 <- beta01 +
    beta11 * cov_11 +
    beta21 * cov_12 +
    beta31 * cov_13 +
    beta41 * cov_14 
  
  
  
  mu2 <- beta02 +
    beta12 * cov_21 +
    beta22 * cov_22 +
    beta32 * cov_23 +
    beta42 * cov_24 
  
  if (sigma1 <= 0 || 
      sigma2 <= 0 || 
      alpha <= 0) { 
    return(Inf) 
  } 
  
  if (any(t1 <= 0) || 
      any(t2 <= 0)) { 
    return(Inf) } 
  
  z1 <- (mu1 - log(t1)) / sigma1 
  S1 <- pnorm(z1) 
  f1 <- dlnorm( t1, meanlog = mu1, sdlog = sigma1 )
  
  z2 <- (mu2 - log(t2)) / sigma2 
  S2 <- pnorm(z2) 
  f2 <- dlnorm( t2, meanlog = mu2, sdlog = sigma2 ) 
  
  A <- S1^(-alpha) + S2^(-alpha) - 1 
  
  if (any(!is.finite(A)) || 
      any(A <= 0)) { 
    return(Inf) }  
  
  S12 <- A^(-1 / alpha) 
  S121 <- -A^(-1 / alpha - 1) * S1^(-alpha - 1) * f1 
  S122 <- -A^(-1 / alpha - 1) * S2^(-alpha - 1) * f2 
  S1212 <- (1 + alpha) * A^(-1 / alpha - 2) * S1^(-alpha - 1) * S2^(-alpha - 1) * f1 * f2 
  
  loglik <- cens1 * cens2 * log(S1212) + 
    cens1 * (1 - cens2) * log(-S121) + (1 - cens1) * cens2 * log(-S122) + 
    (1 - cens1) * (1 - cens2) * log(S12) 
  
  if (any(!is.finite(loglik))) { 
    return(Inf) } 
  
  return(-sum(loglik)) 
}

# resultado modelo lognormal lognormal

gamma1_inicial <- ajust1$scale
gamma2_inicial <- ajust2$scale

valorin1 <- c( ajust1$coefficients, ajust2$coefficients, gamma1_inicial, gamma2_inicial, 0.4 ) 
estima_ln_ln <- optim( par = valorin1, fn = verosi_ln_ln, hessian = TRUE, method = "BFGS" ) 
estima_ln_ln
parametro_ln_ln <- estima_ln_ln$par


######################################################
#### Calculando probabilidades
######################################################


calcular_probabilidades <- function(dados, t1, t2,
                                    par = parametro_ln_ln) {
  
  # Verificação dos tempos
  if (t1 <= 0 || t2 <= 0) {
    stop("Os tempos t1 e t2 devem ser positivos.")
  }
  
  # Verificação dos parâmetros
  if (length(par) != 13) {
    stop("O vetor de parâmetros deve conter 13 elementos.")
  }
  
  # Parâmetros da margem PNACAC
  beta01 <- par[1]
  beta11 <- par[2]
  beta21 <- par[3]
  beta31 <- par[4]
  beta41 <- par[5]
  
  # Parâmetros da margem PNACTO
  beta02 <- par[6]
  beta12 <- par[7]
  beta22 <- par[8]
  beta32 <- par[9]
  beta42 <- par[10]
  
  # Parâmetros de escala e cópula
  sigma1 <- par[11]
  sigma2 <- par[12]
  alpha <- par[13]
  
  if (sigma1 <= 0 || sigma2 <= 0 || alpha <= 0) {
    stop("Os parâmetros sigma1, sigma2 e alpha devem ser positivos.")
  }
  
  # Preditor linear da PNACAC
  mu1 <- beta01 +
    beta11 * dados$TX_SUCESSO_pnacac +
    beta21 * dados$PIB_pnacac +
    beta31 * dados$ANO_ELEITORAL_NAC_pnacac +
    beta41 * dados$CARREIRAS_pnacac
  
  # Preditor linear da PNACTO
  mu2 <- beta02 +
    beta12 * dados$TX_PARTC_pnaacto +
    beta22 * dados$TI_TX_pnaacto +
    beta32 * dados$PIB_pnaacto +
    beta42 * dados$IDEOL_PRF_pnaacto
  
  # Sobrevivências marginais lognormais
  S1 <- pnorm((mu1 - log(t1)) / sigma1)
  
  S2 <- pnorm((mu2 - log(t2)) / sigma2)
  
  # Sobrevivência conjunta pela cópula de Clayton
  A <- S1^(-alpha) + S2^(-alpha) - 1
  
  S12 <- A^(-1 / alpha)
  
  # Probabilidades marginais de adesão
  F1 <- 1 - S1
  
  F2 <- 1 - S2
  
  # Probabilidade conjunta de adesão às duas políticas
  F12 <- 1 - S1 - S2 + S12
  
  # Proteção contra pequenos erros numéricos
  F12 <- pmax(0, pmin(1, F12))
  
  # Probabilidade conjunta agregada
  prob_conjunta <- mean(F12, na.rm = TRUE)
  
  # Probabilidade condicional:
  # P(PNACTO | PNACAC)
  denominador1 <- sum(F1, na.rm = TRUE)
  
  prob_condicional_12 <- sum(F12, na.rm = TRUE) /
    denominador1
  
  # Probabilidade condicional:
  # P(PNACAC | PNACTO)
  denominador2 <- sum(F2, na.rm = TRUE)
  
  prob_condicional_21 <- sum(F12, na.rm = TRUE) /
    denominador2
  
  # Resultados
  list(
    t1 = t1,
    t2 = t2,
    prob_conjunta = prob_conjunta,
    prob_condicional_12 = prob_condicional_12,
    prob_condicional_21 = prob_condicional_21,
    resultados_municipais = data.frame(
      F1 = F1,
      F2 = F2,
      F12 = F12)
  )
}


# Tempos em dias
periodos <- c(1, 3, 5, 10) * 365

# Cálculo para os diferentes períodos
resultados <- lapply(periodos, function(t) {
  
  calcular_probabilidades(
    dados = dados,
    t1 = t,
    t2 = t
  )
})

# Cálculo para o final do período
resultado_2022 <- calcular_probabilidades(
  dados = dados,
  t1 = 6773,
  t2 = 6528
)

# Organização da tabela
tabela_resultados <- data.frame(
  
  Periodo = c(
    "365 dias",
    "1095 dias",
    "1825 dias",
    "3650 dias",
    "Final do período"
  ),
  
  P12 = c(
    sapply(resultados, function(x) x$prob_conjunta),
    resultado_2022$prob_conjunta
  ),
  
  P21_cond_12 = c(
    sapply(resultados, function(x) x$prob_condicional_12),
    resultado_2022$prob_condicional_12
  ),
  
  P12_cond_21 = c(
    sapply(resultados, function(x) x$prob_condicional_21),
    resultado_2022$prob_condicional_21
  )
)

# Conversão para porcentagem
tabela_resultados[, 2:4] <-
  tabela_resultados[, 2:4] * 100

print(tabela_resultados)
