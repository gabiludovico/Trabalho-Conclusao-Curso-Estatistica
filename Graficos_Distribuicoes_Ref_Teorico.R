####################################################
### FUNÇÃO DE RISCO ACUMULADA 
####################################################

# Proporção da área do gráfico
par(pin = c(4, 3.6))

# Tempo
t <- seq(0, 23, length.out = 300)

# Funções de risco acumulado
Lambda1 <- 0.0165 * t
Lambda2 <- 0.0006 * t^2
Lambda3 <- 0.09 * sqrt(t)

# Gráfico
plot(t, Lambda1,
     type = "l",
     lty = 1,
     lwd = 1,
     col = "black",
     xlim = c(0, 25),
     ylim = c(0, 0.5),
     xlab = "Tempo",
     ylab = "Taxa de falha acumulada",
     axes = FALSE)

# Eixos
axis(1, at = seq(0, 25, 5))

axis(2,
     at = seq(0, 0.5, 0.1),
     labels = c("0,0", "0,1", "0,2", "0,3", "0,4", "0,5"),
     las = 1)

# Remover moldura superior e direita
box(bty = "n")

# Curvas
lines(t, Lambda2, lty = "44", col = "black")
lines(t, Lambda3, lty = "4414", col = "black")

# Legenda
legend("topleft",
       legend = expression(
         Lambda[1](t),
         Lambda[2](t),
         Lambda[3](t)
       ),
       lty = c(1, 2, 4),
       col = "black",
       lwd = 1,
       bty = "n",
       cex = 1.1)

# ============================================================
# FUNÇÕES DE SOBREVIVÊNCIA E DE RISCO - DISTRIBUIÇÃO EXPONENCIAL
# ============================================================

# Valores do parâmetro de escala
alpha <- c(1,0.7,0.5)


# Cores e tipos de linha
cores <- c("black", "#CC0D0D", "#2c6552", "#2e64ab")
linhas <- c(1, 2, 3)

# Intervalo de tempo
t <- seq(0, 6, length.out = 1000)

# Funções da distribuição exponencial
S <- function(t, alpha) {
  exp(-t / alpha)
}

h <- function(t, alpha) {
  rep(1 / alpha, length(t))
}

# Dois gráficos lado a lado
layout(matrix(c(1, 2), nrow = 1), widths = c(0.8, 0.7))

par(
  oma = c(0, 0, 0, 0),
  xpd = NA
)
# ------------------------------------------------------------
# 1. FUNÇÃO DE SOBREVIVÊNCIA
# ------------------------------------------------------------
par(mar = c(4.5, 4.5, 2, 0.5))
plot(
  t, S(t, alpha[1]),
  type = "l",
  col = cores[1],
  lty = linhas[1],
  lwd = 1.8,
  xlim = c(0, 6),
  ylim = c(0, 1),
  xlab = "Tempo",
  ylab = expression(S(t)),
  main = "",
  yaxt = "n",
  bty = "l"
)

axis(
  side = 2,
  at = seq(0, 1, by = 0.2),
  labels = format(seq(0, 1, by = 0.2), decimal.mark = ",")
)

for (i in 2:length(alpha)) {
  lines(
    t, S(t, alpha[i]),
    col = cores[i],
    lty = linhas[i],
    lwd = 1.8
  )
}

# ------------------------------------------------------------
# 2. FUNÇÃO DE RISCO
# ------------------------------------------------------------
par(mar = c(4.5, 2.5, 2, 0.5))
plot(
  t, h(t, alpha[1]),
  type = "l",
  col = cores[1],
  lty = linhas[1],
  lwd = 1.8,
  xlim = c(0, 6),
  ylim = c(0.5,2.5 ),
  xlab = "Tempo",
  ylab = expression(h(t)),
  main = "",
  yaxt = "n",
  bty = "l"
)

axis(
  side = 2,
  at = seq(0.5, 2.5, by = 0.5),
  labels = format(seq(0.5, 2.5, by = 0.5), decimal.mark = ",")
)


for (i in 2:length(alpha)) {
  lines(
    t, h(t, alpha[i]),
    col = cores[i],
    lty = linhas[i],
    lwd = 1.8
  )
}


# Restaurar configurações gráficas
par(mfrow = c(1, 1), xpd = FALSE)


#######################################################################
## WEIBULL 
#####################################################################

# Parâmetros de forma (gamma) e escala (alpha)
gamma <- c(3, 4, 5, 1, 0.5)
alpha <- c(250, 350, 500, 150, 50)

cores <- c("black", "#CC0D0D", "#2c6552", "#2e64ab", "#eccb30")
linhas <- c(1, 2, 3, 4, 5)

# Intervalo de tempo
t <- seq(0, 800, length.out = 1000)

# Função de sobrevivência Weibull
S <- function(t, gamma, alpha) {
  exp(-(t / alpha)^gamma)
}

# Função de risco Weibull
h <- function(t, gamma, alpha) {
  (gamma / alpha) * (t / alpha)^(gamma - 1)
}

# Rótulos da legenda
rotulos <- expression(
  gamma == 3 ~ ";" ~ alpha == 250,
  gamma == 4 ~ ";" ~ alpha == 350,
  gamma == 5 ~ ";" ~ alpha == 500,
  gamma == 1 ~ ";" ~ alpha == 150,
  gamma == 0.5 ~ ";" ~ alpha == 50
)

# Configuração dos gráficos
layout(
  matrix(c(1, 2, 3), nrow = 1),
  widths = c(1, 1, 0.45)
)

par(
  oma = c(0, 0, 0, 0),
  mgp = c(3, 1, 0)
)

# ------------------------------------------------------------
# 1. FUNÇÃO DE SOBREVIVÊNCIA
# ------------------------------------------------------------

par(mar = c(4.5, 4.5, 2, 0.5))

plot(
  t, S(t, gamma[1], alpha[1]),
  type = "l",
  col = cores[1],
  lty = linhas[1],
  lwd = 1.8,
  xlim = c(0, 800),
  ylim = c(0, 1),
  xlab = "Tempo",
  ylab = expression(S(t)),
  main = "",
  xaxt = "n",
  yaxt = "n",
  bty = "l",
  xpd = FALSE
)

axis(
  side = 1,
  at = seq(0, 800, by = 200)
)

axis(
  side = 2,
  at = seq(0, 1, by = 0.2),
  labels = format(
    seq(0, 1, by = 0.2),
    decimal.mark = ","
  )
)

for (i in 2:length(gamma)) {
  lines(
    t,
    S(t, gamma[i], alpha[i]),
    col = cores[i],
    lty = linhas[i],
    lwd = 1.8
  )
}

# ------------------------------------------------------------
# 2. FUNÇÃO DE RISCO
# ------------------------------------------------------------

# Evita problemas numéricos em t = 0 para gamma < 1
t_risco <- seq(0.01, 800, length.out = 1000)

par(mar = c(4.5, 4.5, 2, 0.5))

plot(
  t_risco, h(t_risco, gamma[1], alpha[1]),
  type = "l",
  col = cores[1],
  lty = linhas[1],
  lwd = 1.8,
  xlim = c(0, 800),
  ylim = c(0, 0.07),
  xlab = "Tempo",
  ylab = expression(h(t)),
  main = "",
  xaxt = "n",
  yaxt = "n",
  bty = "l",
  xpd = FALSE
)

axis(
  side = 1,
  at = seq(0, 800, by = 200)
)

axis(
  side = 2,
  at = seq(0, 0.07, by = 0.01),
  labels = format(
    seq(0, 0.07, by = 0.01),
    decimal.mark = ",",
    nsmall = 2
  )
)

for (i in 2:length(gamma)) {
  lines(
    t_risco,
    h(t_risco, gamma[i], alpha[i]),
    col = cores[i],
    lty = linhas[i],
    lwd = 1.8
  )
}

# ------------------------------------------------------------
# 3. LEGENDA ÚNICA À DIREITA
# ------------------------------------------------------------

par(mar = c(0, 0, 0, 0))

plot.new()

legend(
  "center",
  legend = rotulos,
  col = cores,
  lty = linhas,
  lwd = 1.8,
  bty = "n",
  cex = 0.85,
  seg.len = 3
)

# Restaurar configurações gráficas
par(mfrow = c(1, 1), xpd = FALSE)

#######################################################################
## LOG-NORMAL
#####################################################################



# Parâmetros da distribuição log-normal
mi <- c(0, 0, 0, 1,1)
sigma <- c(0.5,0.7,1.5,0.7,2)

cores <- c("black", "#CC0D0D", "#2c6552", "#2e64ab", "#eccb30")
linhas <- c(1, 2, 3, 4,5)

# Intervalo de tempo
t <- seq(0.001, 4, length.out = 2000)

# Função de sobrevivência log-normal
S <- function(t, mi, sigma) {
  1 - pnorm((log(t) - mi) / sigma)
}

# Função de densidade log-normal
f <- function(t, mi, sigma) {
  dlnorm(t, meanlog = mi, sdlog = sigma)
}

# Função de risco log-normal
h <- function(t, mi, sigma) {
  f(t, mi, sigma) / S(t, mi, sigma)
}

# Rótulos da legenda
rotulos <- expression(
  mu == 0 ~ ";" ~ sigma == 0*","*5,
  mu == 0 ~ ";" ~ sigma == 0*","*7,
  mu == 0 ~ ";" ~ sigma == 1*","*5,
  mu == 1 ~ ";" ~ sigma == 0*","*7,
  mu == 1 ~ ";" ~ sigma == 2*","*0
)

# Configuração dos gráficos
layout(
  matrix(c(1, 2, 3), nrow = 1),
  widths = c(1, 1, 0.45)
)

par(
  oma = c(0, 0, 0, 0),
  mgp = c(3, 1, 0)
)

# ------------------------------------------------------------
# 1. FUNÇÃO DE SOBREVIVÊNCIA
# ------------------------------------------------------------

par(mar = c(4.5, 4.5, 2, 0.5))

plot(
  t, S(t, mi[1], sigma[1]),
  type = "l",
  col = cores[1],
  lty = linhas[1],
  lwd = 1.8,
  xlim = c(0, 4),
  ylim = c(0, 1),
  xlab = "log(Tempo)",
  ylab = expression(S(t)),
  main = "",
  xaxt = "n",
  yaxt = "n",
  bty = "l",
  xpd = FALSE
)

axis(
  side = 1,
  at = seq(0, 4, by = 1)
)

axis(
  side = 2,
  at = seq(0, 1, by = 0.2),
  labels = format(
    seq(0, 1, by = 0.2),
    decimal.mark = ","
  )
)

for (i in 2:length(mi)) {
  lines(
    t,
    S(t, mi[i], sigma[i]),
    col = cores[i],
    lty = linhas[i],
    lwd = 1.8
  )
}

# ------------------------------------------------------------
# 2. FUNÇÃO DE RISCO
# ------------------------------------------------------------

par(mar = c(4.5, 4.5, 2, 0.5))

plot(
  t, h(t, mi[1], sigma[1]),
  type = "l",
  col = cores[1],
  lty = linhas[1],
  lwd = 1.8,
  xlim = c(0, 4),
  ylim = c(0, 2.5),
  xlab = "log(Tempo)",
  ylab = expression(h(t)),
  main = "",
  xaxt = "n",
  yaxt = "n",
  bty = "l",
  xpd = FALSE
)

axis(
  side = 1,
  at = seq(0, 4, by = 1)
)

axis(
  side = 2,
  at = seq(0, 2.5, by = 0.5),
  labels = format(
    seq(0, 2.5, by = 0.5),
    decimal.mark = ",",
    nsmall = 1
  )
)

for (i in 2:length(mi)) {
  lines(
    t,
    h(t, mi[i], sigma[i]),
    col = cores[i],
    lty = linhas[i],
    lwd = 1.8
  )
}

# ------------------------------------------------------------
# 3. LEGENDA ÚNICA À DIREITA
# ------------------------------------------------------------

par(mar = c(0, 0, 0, 0))

plot.new()

legend(
  "center",
  legend = rotulos,
  col = cores,
  lty = linhas,
  lwd = 1.8,
  bty = "n",
  cex = 0.85,
  seg.len = 2
)

# Restaurar configurações gráficas
par(mfrow = c(1, 1), xpd = FALSE)


