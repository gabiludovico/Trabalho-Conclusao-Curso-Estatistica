library(readODS)
library(survival)
library(survminer)
library(dplyr)
library(tidyr)
library(corrplot)
library(AdequacyModel)
library(ggplot2)
library(car)
library(survminer)
library(scales)
library(lmtest)

# Organizando os dados
dados <- read_ods("/dados/unb/TCC-2/Dados/Base_saude.ods")

dados <- dados %>%
  mutate(TX_VETO = TX_VETO/100,
         TX_FRAG_PARTD_NEP = as.numeric(scale(TX_FRAG_PARTD_NEP)),
         T_MEDIO_PERMAN_MINISTRO_P_MANDATO = as.numeric(scale(T_MEDIO_PERMAN_MINISTRO_P_MANDATO)),
         CARREIRAS = CARREIRAS/3,
         NUM_ENTIDADES = as.numeric(scale(NUM_ENTIDADES)),
         PROP_SERVIDORES = SERVIDORES_RJU/SERVIDORES,
         TI_TX = TI_EMPENHADO/TI_PROJ_LEI,
         PIB = as.numeric(scale(log(PIB))),
         MRG_PRSD = MRG_PRSD/100) %>%
  filter(POL_ABV %in% c("pnacac","pnaacto"))


dados_pnacac <- dados%>%
  filter(POL_ABV == "pnacac")

dados_pnaacto <- dados%>%
  filter(POL_ABV == "pnaacto")

# Modelo PNACAC univariado escolhido
m_lognormal <- survreg(
  Surv(t, ADOPT) ~ 
    TX_SUCESSO +
    TX_FRAG_PARTD_NEP +
    TI_TX  +
    PIB +
    ANO_ELEITORAL_MUNIC +
    IDEOL_PRF,
  data = dados_pnacac,
  dist = "lognormal"
);summary(m_lognormal)

# Modelo exponencial PNAACTO escolhido
m_exponencial2 <- survreg(
  Surv(t, ADOPT) ~ 
    TX_SUCESSO +
    TX_DISCIP_BASE +
    CARREIRAS +
    PIB +
    IDEOL_PRF,
  data = dados_pnaacto,
  dist = "exponential"
);summary(m_exponencial2)


# Modelo lognormal PNAACTO escolhido
m_lognormal2 <- survreg(
  Surv(t, ADOPT) ~ 
    TX_PARTC+
    TX_FRAG_PARTD_NEP +
    ORC_TOTAL_TX +
    TI_TX+
    PIB+
    IDEOL_PRF,
  data = dados_pnaacto,
  dist = "lognormal"
);summary(m_lognormal2)

###########################################################
#################### RESIDUO COX-SNELL ##################
###########################################################

# PNACAC

#####################################################################
#Log-normal
mip<-m_lognormal$linear.predictors 
Smod<-pnorm((-log(dados_pnacac$t) + mip)/m_lognormal$scale)
ei<-(-log(Smod)) 

KMew<-survfit(Surv(ei,dados_pnacac$ADOPT)~1,conf.int=F)
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

#####################################################################
# Exponencial

mip_exp <- m_exponencial2$linear.predictors
Smod_exp <- exp(-dados_pnaacto$t / exp(mip_exp))
ei_exp <- -log(Smod_exp)
KM_exp <- survfit(Surv(ei_exp, dados_pnaacto$ADOPT) ~ 1,conf.int = FALSE)

te_exp <- KM_exp$time
ste_exp <- KM_exp$surv
sexp_exp <- exp(-te_exp)

dados_residuos <- data.frame(
  tempo = te_exp,
  KM = ste_exp,
  Exponencial = sexp_exp
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
  
  scale_x_continuous(labels = 
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


#######################################################################
#Log-normal2
mip<-m_lognormal2$linear.predictors 
Smod<-pnorm((-log(dados_pnaacto$t) + mip)/m_lognormal2$scale)
ei<-(-log(Smod)) 

KMew<-survfit(Surv(ei,dados_pnaacto$ADOPT)~1,conf.int=F)
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