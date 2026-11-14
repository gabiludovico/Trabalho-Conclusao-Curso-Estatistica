library(lattice)
library(readODS)
library(survival)
library(survminer)
library(dplyr)
library(tidyr)
library(AdequacyModel)
library(ggplot2)
library(car)
library(scales)
library(lattice)
library(grid)

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


# Parametro do modelo
parametro_ln_ln <- estima_ln_ln$par

# Médias das covariáveis do PNACAC
medias_pnacac <- c(
  0.66326248,  # TX_SUCESSO
  0.06908592,  # PIB
  0.86477987,  # ANO_ELEITORAL_NAC
  0.29874214   # CARREIRAS
)

# Médias das covariáveis do PNAACTO
medias_pnaacto <- c(
  0.9062940,  # TX_PARTC
  0.9519105,  # TI_TX
  0.2020270,  # PIB
  0.1890177   # IDEOL_PRF
)

# Função de sobrevivência conjunta com cópula de Clayton
S_cop_estim <- function(t1, t2,
                        cov11, cov21, cov31, cov41,
                        cov12, cov22, cov32, cov42,
                        b01, b11, b21, b31, b41, sigma1,
                        b02, b12, b22, b32, b42, sigma2,
                        theta) {
  
  eta1 <- b01 + b11*cov11 + b21*cov21 +
    b31*cov31 + b41*cov41
  
  eta2 <- b02 + b12*cov12 + b22*cov22 +
    b32*cov32 + b42*cov42
  
  S1 <- pnorm((eta1 - log(t1))/sigma1)
  S2 <- pnorm((eta2 - log(t2))/sigma2)
  
  (S1^(-theta) + S2^(-theta) - 1)^(-1/theta)
}

f <- function(x, y) {
  S_cop_estim(
    x, y,
    medias_pnacac[1],
    medias_pnacac[2],
    medias_pnacac[3],
    medias_pnacac[4],
    medias_pnaacto[1],
    medias_pnaacto[2],
    medias_pnaacto[3],
    medias_pnaacto[4],
    parametro_ln_ln[1],
    parametro_ln_ln[2],
    parametro_ln_ln[3],
    parametro_ln_ln[4],
    parametro_ln_ln[5],
    parametro_ln_ln[11],
    parametro_ln_ln[6],
    parametro_ln_ln[7],
    parametro_ln_ln[8],
    parametro_ln_ln[9],
    parametro_ln_ln[10],
    parametro_ln_ln[12],
    parametro_ln_ln[13]
  )
}


x <- seq(43,6773,length.out = 30)
y <- seq(400,6528,length.out = 30)

z <- outer(x, y, f)

range(z, na.rm = TRUE)

df_surface <- expand.grid(x = x, y = y)
df_surface$z <- as.vector(z)
colorkey_vals <- pretty(range(df_surface$z), n = 5)

# Formatação dos números
formatar_numero <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",", 
         scientific = FALSE, digits = 2)
}


x_vals <- pretty(df_surface$x)
y_vals <- pretty(df_surface$y)
z_vals <- seq(0, 1, by = 0.2)

# Construção do gráfico
wireframe(z ~ x * y, 
          data = df_surface,
          zoom = 1,
          aspect = c(1, 1, 0.7),
          zlim = c(0,1),
          xlab = NULL,
          ylab = NULL,
          zlab = NULL,
          drape = TRUE,
          
          scpos = list(x =2, y = 5, z = 1),
          
          colorkey = list(
            title = list(
              label = expression(paste("S(", t[1], ",", t[2], ")")),
              cex = 0.8
            ),
            width = 0.8,
            height = 0.6,
            at = colorkey_vals,
            labels = list(
              at = colorkey_vals,
              cex = 0.6,
              labels = formatar_numero(colorkey_vals)
            )
          ),
          scales = list(
            arrows = FALSE,
            cex = 0.6,
            x = list(at = x_vals, labels = formatar_numero(x_vals)),
            y = list(at = y_vals, labels = formatar_numero(y_vals)),
            z = list(at = z_vals, labels = formatar_numero(z_vals))
          ),
          
          screen = list(z =-60, x = -60),
          shade = FALSE,
          par.settings = list(
            axis.line = list(col = "transparent")
          ))

grid.text("Tempos PNACAC",
          x = unit(0.25, "npc"),
          y = unit(0.2, "npc"),
          rot = -50,
          gp = gpar(fontsize = 7))

grid.text("Tempos PNAACTO",
          x = unit(0.57, "npc"),
          y = unit(0.12, "npc"),
          rot = 20,
          gp = gpar(fontsize = 7))

grid.text(expression(S(t[1], t[2])),
          x = unit(0.17, "npc"),
          y = unit(0.60, "npc"),
          rot = 90,
          gp = gpar(fontsize = 8))

