library(readODS)
library(survival)
library(survminer)
library(dplyr)
library(tidyr)
library(AdequacyModel)
library(ggplot2)
library(car)
library(scales)
library(CopulaCenR)

###########################################
# Preparação dos dados
###########################################

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

################################################
# Ajuste do modelo
################################################

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


gamma1_inicial <- ajust1$scale
gamma2_inicial <- ajust2$scale

valorin1 <- c( ajust1$coefficients, ajust2$coefficients, gamma1_inicial, gamma2_inicial, 0.4 ) 
estima_ln_ln <- optim( par = valorin1, fn = verosi_ln_ln, hessian = TRUE, method = "BFGS" ) 
parametro_ln_ln <- estima_ln_ln$par 

########################################################
# Resíduo CoxSnell da Estimativa do Ajuste
########################################################

param <- parametro_ln_ln

# PNACAC
mip1 <- param[1] +
  param[2] * cov_11 +
  param[3] * cov_12 +
  param[4] * cov_13 +
  param[5] * cov_14

sigma1 <- param[11]
Smod1 <- pnorm((mip1 - log(t1)) / sigma1)
e1 <- -log(Smod1)


KMew<-survfit(Surv(e1,cens1)~1,conf.int=F)
te<-KMew$time 
ste<-KMew$surv
sexp<-exp(-te)

dados_residuos <- data.frame(
  tempo = te,
  KM = ste,
  Exponencial = sexp
)

ggplot(dados_residuos, aes(x = tempo)) +
  geom_step(
    aes(y = KM, color = "Kaplan-Meier"),
    linewidth = 0.8
  ) +
  geom_line(
    aes(y = Exponencial, color = "Exponencial padrão"),
    linetype = "dashed",
    linewidth = 0.5
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Kaplan-Meier" = "black",
      "Exponencial padrão" = "#9D0208"
    )
  ) +
  scale_y_continuous(labels = 
                       number_format(decimal.mark = ",", big.mark = ".")) +
  labs(
    x = "Resíduos de Cox-Snell",
    y = "Sobrevivência estimada"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal() +
  theme(
    legend.position = c(0.80, 0.95),
    legend.background = element_rect(fill = "white", color = "gray"),
    legend.justification = c(1, 1)
  )

# PNAACTO
mip2 <- param[6] +
  param[7] * cov_21 +
  param[8] * cov_22 +
  param[9] * cov_23 +
  param[10] * cov_24

sigma2 <- param[12]

Smod2 <- pnorm((mip2 - log(t2)) / sigma2)
e2 <- -log(Smod2)

KMew2<-survfit(Surv(e2,cens2)~1,conf.int=F)
te2<-KMew2$time 
ste2<-KMew2$surv
sexp2<-exp(-te2)

dados_residuos <- data.frame(
  tempo = te2,
  KM = ste2,
  Exponencial = sexp2
)

ggplot(dados_residuos, aes(x = tempo)) +
  geom_step(
    aes(y = KM, color = "Kaplan-Meier"),
    linewidth = 0.8
  ) +
  geom_line(
    aes(y = Exponencial, color = "Exponencial padrão"),
    linetype = "dashed",
    linewidth = 0.5
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Kaplan-Meier" = "black",
      "Exponencial padrão" = "#9D0208"
    )
  ) +
  scale_y_continuous(labels = 
                       number_format(decimal.mark = ",", big.mark = ".")) +
  labs(
    x = "Resíduos de Cox-Snell",
    y = "Sobrevivência estimada"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal() +
  theme(
    legend.position = c(0.80, 0.95),
    legend.background = element_rect(fill = "white", color = "gray"),
    legend.justification = c(1, 1)
  )

###################################################################
# Avaliação da adequabilidade da Cópula de Clayton para os dados
###################################################################

dados_IR <- data.frame(
  event_time.L = t1,
  status.L = cens1,
  event_time.R = t2,
  status.R = cens2
)

teste <- IRsurv(data = dados_IR,
                censoring = "rc",
                copula = "clayton",
                R =1000)


print(teste)
