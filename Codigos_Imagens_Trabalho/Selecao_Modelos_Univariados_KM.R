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

dados <- read_ods("/dados/unb/TCC-2/Dados/Base_saude.ods")

dados_pnacac <- dados%>%
  filter(POL_ABV == "pnacac")

dados_pnaacto <- dados %>%
  filter(POL_ABV == "pnaacto")

#####################################################################################
############################### Política PNACAC #####################################
#####################################################################################


# Kaplen-Meier dos tempos
KMg <- survfit(data = dados_pnacac, Surv(time = t, event = ADOPT) ~ 1, conf.int = FALSE)


# Extrair dados do Kaplan-Meier
km_data <- data.frame(
  time = KMg$time,
  surv = KMg$surv
)

# Ajustes dos modelos paramétricos
ti <- dados_pnacac$t
deltai <- dados_pnacac$ADOPT

# Exponencial
MLE <- survreg(Surv(ti, deltai) ~ 1, dist = 'exponential')
lambda <- 1/exp(MLE$coefficients)

# Weibull
mwe <- survreg(Surv(ti, deltai) ~ 1, dist = 'weibull')
(alfa_weibull <- exp(mwe$coefficients[1]))
(gama_weibull <- 1/mwe$scale)

# Lognormal
mln <- survreg(Surv(ti, deltai) ~ 1, dist = 'lognormal')
mu <- mln$coefficients[1]
sigma <- mln$scale

# Loglogística
mll <- survreg(Surv(ti, deltai) ~ 1, dist = "loglogistic")
alfa_loglog <- exp(mll$coefficients[1])
gama_loglog <- 1/mll$scale

# Criar dados para as curvas teóricas
x_vals <- seq(0, max(ti), length.out = 1000)

curves_data <- data.frame(
  x = x_vals,
  Exponencial = exp(-lambda * x_vals),
  Weibull = exp(-(x_vals/alfa_weibull)^gama_weibull),
  Lognormal = pnorm((-log(x_vals) + mu)/sigma, mean = 0, sd = 1),
  Loglogística = 1/(1 + (x_vals/alfa_loglog)^gama_loglog)
)

km_data_completo <- data.frame(
  time = c(0, km_data$time),
  surv = c(1, km_data$surv)
)

# Gráfico comparativo distribuições e tempo
ggplot() +
  geom_step(data = km_data_completo, aes(x = time, y = surv, color = "Kaplan-Meier"), 
            size = .8) +
  geom_line(data = curves_data, aes(x = x, y = Exponencial, color = "Exponencial"), 
            size = .8) +
  geom_line(data = curves_data, aes(x = x, y = Weibull, color = "Weibull"), 
            size = .8) +
  geom_line(data = curves_data, aes(x = x, y = Loglogística, color = "Log-logística"), 
            size = .8) +
  geom_line(data = curves_data, aes(x = x, y = Lognormal, color = "Log-normal"), 
            size = .8) +
  scale_x_continuous(limits = c(0, max(ti)),
                     labels = number_format(decimal.mark = ",", big.mark = ".")) +
  scale_y_continuous(
    limits = c(0.70, 1),
    breaks = seq(0.70, 1, by = 0.05),
    labels = number_format(decimal.mark = ",", big.mark = ".")
  ) +
  scale_color_manual(
    name = NULL, 
    breaks = c("Kaplan-Meier", "Exponencial", "Weibull", "Log-logística", "Log-normal"),
    values = c(
      "Kaplan-Meier" = "black",
      "Exponencial" = "#2e64ab",
      "Weibull" = "#CC0D0D",
      "Log-logística" = "#2c6552",
      "Log-normal" = "#eccb30"
    )
  ) +
  labs(
    x = "Tempo",
    y = "S(t)"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.21, 0.28),
    legend.background = element_rect(fill = "white", color = "gray"),
    legend.title = element_blank()
  )


# AIC e BIC das distribuições
AIC(MLE); AIC(mwe); AIC(mln); AIC(mll)
BIC(MLE); BIC(mwe); BIC(mln); BIC(mll)


#####################################################################################
############################## Política PNAACTO #####################################
#####################################################################################


# Kaplen-Meier dos tempos
KMg <- survfit(data = dados_pnaacto, Surv(time = t, event = ADOPT) ~ 1, conf.int = FALSE)

# Extrair dados do Kaplan-Meier
km_data <- data.frame(
  time = KMg$time,
  surv = KMg$surv
)

# Ajustes dos modelos paramétricos
ti <- dados_pnaacto$t
deltai <- dados_pnaacto$ADOPT

# Exponencial
MLE <- survreg(Surv(ti, deltai) ~ 1, dist = 'exponential')
lambda <- 1/exp(MLE$coefficients)

# Weibull
mwe <- survreg(Surv(ti, deltai) ~ 1, dist = 'weibull')
(alfa_weibull <- exp(mwe$coefficients[1]))
(gama_weibull <- 1/mwe$scale)

# Lognormal
mln <- survreg(Surv(ti, deltai) ~ 1, dist = 'lognormal')
mu <- mln$coefficients[1]
sigma <- mln$scale

# Loglogística
mll <- survreg(Surv(ti, deltai) ~ 1, dist = "loglogistic")
alfa_loglog <- exp(mll$coefficients[1])
gama_loglog <- 1/mll$scale


# Criar dados para as curvas teóricas
x_vals <- seq(0, max(ti), length.out = 1000)

curves_data <- data.frame(
  x = x_vals,
  Exponencial = exp(-lambda * x_vals),
  Weibull = exp(-(x_vals/alfa_weibull)^gama_weibull),
  Lognormal = pnorm((-log(x_vals) + mu)/sigma, mean = 0, sd = 1),
  Loglogística = 1/(1 + (x_vals/alfa_loglog)^gama_loglog)
)

km_data_completo <- data.frame(
  time = c(0, km_data$time),
  surv = c(1, km_data$surv)
)

# Gráfico comparativo distribuições e tempo
ggplot() +
  geom_step(data = km_data_completo, aes(x = time, y = surv, color = "Kaplan-Meier"), 
            size = .8) +
  geom_line(data = curves_data, aes(x = x, y = Exponencial, color = "Exponencial"), 
            size = .8) +
  geom_line(data = curves_data, aes(x = x, y = Weibull, color = "Weibull"), 
            size = .8) +
  geom_line(data = curves_data, aes(x = x, y = Loglogística, color = "Log-logística"), 
            size = .8) +
  geom_line(data = curves_data, aes(x = x, y = Lognormal, color = "Log-normal"), 
            size = .8) +
  scale_x_continuous(limits = c(0, max(ti)),
                     labels = number_format(decimal.mark = ",", big.mark = ".")) +
  scale_y_continuous(
    limits = c(0.70, 1),
    breaks = seq(0.70, 1, by = 0.05),
    labels = number_format(decimal.mark = ",", big.mark = ".")
  ) +
  scale_color_manual(
    name = NULL,
    breaks = c("Kaplan-Meier", "Exponencial", "Weibull", "Log-logística", "Log-normal"),
    values = c(
      "Kaplan-Meier" = "black",
      "Exponencial" = "#2e64ab",
      "Weibull" = "#CC0D0D",
      "Log-logística" = "#2c6552",
      "Log-normal" = "#eccb30"
    )
  ) +
  labs(
    x = "Tempo",
    y = "S(t)"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.21, 0.28),
    legend.background = element_rect(fill = "white", color = "gray"),
    legend.title = element_blank()
  )


# AIC e BIC das distribuições
AIC(MLE); AIC(mwe); AIC(mln); AIC(mll)
BIC(MLE); BIC(mwe); BIC(mln); BIC(mll)

