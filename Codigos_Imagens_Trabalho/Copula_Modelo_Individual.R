library(readODS)
library(survival)
library(survminer)
library(dplyr)
library(tidyr)
library(AdequacyModel)
library(ggplot2)
library(car)
library(scales)


###########################################################################
# PNACAC modelo lognormal com variaveis TX_SUCESSO,PIB,
# ANO_ELEITORAL_NAC e CARREIRAS e PNAACTO modelo lognormal com variaveis 
# TX_PARTC,TI_TX,PIB e IDEOL_PRF
##########################################################################


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

head(dados)


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

sd(t1)
sd(t2)
ajust1<-survreg(Surv(t1,cens1)~cov_11 + cov_12 + cov_13 + cov_14, 
                dist='lognormal');summary(ajust1)
ajust2<-survreg(Surv(t2,cens2)~cov_21 + cov_22 + cov_23 + cov_24,
                dist='lognormal');summary(ajust2)



##correlacao
plot(t1,t2,lty=c(2,1),xlab="Tempo1",ylab="Tempo2")
corrr<-cor(t1,t2,method ="kendall")
corrr


# verossimilhanca por caso lognormal lognormal
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
 
vero_ln_ln <- estima_ln_ln$value 
parametro_ln_ln <- estima_ln_ln$par 

hessi_ln_ln <- estima_ln_ln$hessian 
invR_ln_ln <- solve(hessi_ln_ln) 

variancia_ln_ln <- diag(invR_ln_ln) 
epp_ln_ln <- sqrt(variancia_ln_ln) 

estz_ln_ln <- parametro_ln_ln / epp_ln_ln 
pvalor_ln_ln <- 2 * ( 1 - pnorm(abs(estz_ln_ln)) ) 

taucalc_ln_ln <- parametro_ln_ln[13] / (parametro_ln_ln[13] + 2) 
corrr_ln_ln <- cor( y1, y2, method = "kendall" ) 

vero_ln_ln
parametro_ln_ln
epp_ln_ln
estz_ln_ln
pvalor_ln_ln
taucalc_ln_ln
corrr_ln_ln

BIC_ln_ln <- 2 * estima_ln_ln$value +
  length(estima_ln_ln$par) * log(length(y1));BIC_ln_ln
