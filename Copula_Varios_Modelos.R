library(readODS)
library(survival)
library(survminer)
library(dplyr)
library(tidyr)
library(AdequacyModel)
library(ggplot2)
library(car)

# Carregando e Tratando Dados

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
  filter(POL_ABV %in% c("pnacac","pnaacto")) %>%
  select(SIGUF,MUNICÍPIO,POL_ABV,t,ADOPT,TX_VETO,TX_CONFLITO_MP,TX_PARTC,
         TX_SUCESSO,TX_DOMINANCIA,TX_FRAG_PARTD_NEP,
         TX_DISCIP_BASE,TX_PROP_MNST,TAM_PROP_BASE,
         T_MEDIO_PERMAN_MINISTRO_P_MANDATO,POP_PRSD,ANO_ELEITORAL_MUNIC,
         ANO_ELEITORAL_NAC,MRG_PRSD,IDEOL_PRSD,IDEOL_PRF,PROP_SERVIDORES,
         CARREIRAS,TI_TX,ORC_TOTAL_TX,DISCR_TX,PIB)


dados <- dados %>%
  pivot_wider(
    id_cols = c(SIGUF, MUNICÍPIO),
    names_from = POL_ABV,
    values_from = -c(POL_ABV,SIGUF, MUNICÍPIO),
    names_glue = "{.value}_{POL_ABV}")



# ============================================================
# 1. VARIÁVEIS CANDIDATAS
# ============================================================

variaveis <- c(
  "TX_VETO",
  "TX_CONFLITO_MP",
  "TX_PARTC",
  "TX_SUCESSO",
  "TX_DOMINANCIA",
  "TX_FRAG_PARTD_NEP",
  "TX_DISCIP_BASE",
  "TX_PROP_MNST",
  "TAM_PROP_BASE",
  "T_MEDIO_PERMAN_MINISTRO_P_MANDATO",
  "POP_PRSD",
  "ANO_ELEITORAL_MUNIC",
  "ANO_ELEITORAL_NAC",
  "MRG_PRSD",
  "IDEOL_PRSD",
  "IDEOL_PRF",
  "PROP_SERVIDORES",
  "CARREIRAS",
  "TI_TX",
  "ORC_TOTAL_TX",
  "DISCR_TX"
)

# O PIB será incluído obrigatoriamente em ambas as marginais.
# k representa a quantidade de covariáveis adicionais ao PIB.
max_k <- 5


# ============================================================
# 2. RESTRIÇÕES DE COMPATIBILIDADE
# ============================================================
pode_pnacac <- list(
  
  TX_PROP_MNST = c(
    "TX_CONFLITO_MP", "IDEOL_PRF"
  ),
  
  TAM_PROP_BASE = c(
    "TX_CONFLITO_MP", "IDEOL_PRF"
  ),
  
  PROP_SERVIDORES = c(
    "T_MEDIO_PERMAN_MINISTRO_P_MANDATO",
    "IDEOL_PRF", "TX_SUCESSO"
  ),
  
  POP_PRSD = c(
    "TX_CONFLITO_MP",
    "T_MEDIO_PERMAN_MINISTRO_P_MANDATO",
    "IDEOL_PRF"
  ),
  
  MRG_PRSD = c(
    "ANO_ELEITORAL_MUNIC",
    "T_MEDIO_PERMAN_MINISTRO_P_MANDATO",
    "IDEOL_PRF",
    "TX_SUCESSO",
    "ANO_ELEITORAL_NAC"
  ),
  
  ANO_ELEITORAL_MUNIC = c(
    "IDEOL_PRF", "TX_SUCESSO", "TI_TX",
    "TX_FRAG_PARTD_NEP", "TX_DISCIP_BASE",
    "CARREIRAS"
  ),
  
  TX_CONFLITO_MP = c(
    "IDEOL_PRF", "TX_SUCESSO", "TI_TX",
    "TX_DISCIP_BASE", "ORC_TOTAL_TX",
    "TX_DOMINANCIA", "TX_PARTC", "DISCR_TX",
    "TX_VETO", "IDEOL_PRSD"
  ),
  
  T_MEDIO_PERMAN_MINISTRO_P_MANDATO = c(
    "IDEOL_PRF", "TI_TX", "ANO_ELEITORAL_NAC",
    "TX_DISCIP_BASE", "ORC_TOTAL_TX", "DISCR_TX",
    "IDEOL_PRSD", "TX_PARTC", "CARREIRAS"
  ),
  
  IDEOL_PRF = variaveis,
  
  TX_SUCESSO = c(
    "TI_TX", "ANO_ELEITORAL_NAC",
    "TX_FRAG_PARTD_NEP", "ORC_TOTAL_TX",
    "TX_PARTC", "CARREIRAS"
  ),
  
  TI_TX = c(
    "TX_FRAG_PARTD_NEP", "TX_DISCIP_BASE",
    "DISCR_TX"
  ),
  
  ANO_ELEITORAL_NAC = c(
    "TX_FRAG_PARTD_NEP", "CARREIRAS"
  ),
  
  TX_FRAG_PARTD_NEP = character(0),
  
  TX_DISCIP_BASE = c("CARREIRAS"),
  
  IDEOL_PRSD = character(0),
  TX_VETO = character(0),
  DISCR_TX = character(0),
  TX_PARTC = character(0),
  TX_DOMINANCIA = character(0),
  ORC_TOTAL_TX = character(0),
  CARREIRAS = character(0)
)

pode_pnaacto <- list(
  
  TX_PROP_MNST = c(
    "TI_TX", "IDEOL_PRF", "TX_FRAG_PARTD_NEP"
  ),
  
  TAM_PROP_BASE = c(
    "TI_TX", "IDEOL_PRF", "TX_FRAG_PARTD_NEP"
  ),
  
  PROP_SERVIDORES = c(
    "TX_CONFLITO_MP", "ANO_ELEITORAL_MUNIC",
    "TI_TX", "IDEOL_PRF",
    "T_MEDIO_PERMAN_MINISTRO_P_MANDATO",
    "TX_SUCESSO"
  ),
  
  TX_CONFLITO_MP = c(
    "POP_PRSD", "MRG_PRSD", "TI_TX",
    "IDEOL_PRF", "CARREIRAS"
  ),
  
  POP_PRSD = c(
    "ANO_ELEITORAL_MUNIC", "TI_TX",
    "IDEOL_PRF", "TX_SUCESSO"
  ),
  
  MRG_PRSD = c(
    "ANO_ELEITORAL_MUNIC", "TI_TX",
    "IDEOL_PRF", "TX_SUCESSO",
    "TX_DISCIP_BASE", "ANO_ELEITORAL_NAC",
    "TX_DOMINANCIA"
  ),
  
  ANO_ELEITORAL_MUNIC = c(
    "TI_TX", "IDEOL_PRF", "TX_FRAG_PARTD_NEP",
    "TX_SUCESSO", "TX_DISCIP_BASE"
  ),
  
  TI_TX = c(
    "IDEOL_PRF", "TX_FRAG_PARTD_NEP",
    "T_MEDIO_PERMAN_MINISTRO_P_MANDATO",
    "TX_DISCIP_BASE", "ANO_ELEITORAL_NAC",
    "ORC_TOTAL_TX", "DISCR_TX", "IDEOL_PRSD",
    "TX_VETO", "TX_PARTC"
  ),
  
  IDEOL_PRF = variaveis,
  
  TX_FRAG_PARTD_NEP = c(
    "TX_SUCESSO", "TX_DISCIP_BASE",
    "ANO_ELEITORAL_NAC", "TX_DOMINANCIA",
    "ORC_TOTAL_TX", "DISCR_TX",
    "TX_VETO", "TX_PARTC"
  ),
  
  T_MEDIO_PERMAN_MINISTRO_P_MANDATO = c(
    "CARREIRAS", "TX_DISCIP_BASE", "IDEOL_PRSD"
  ),
  
  TX_SUCESSO = c(
    "CARREIRAS", "TX_DISCIP_BASE",
    "ANO_ELEITORAL_NAC"
  ),
  
  CARREIRAS = c(
    "TX_DISCIP_BASE", "ANO_ELEITORAL_NAC"
  ),
  
  TX_DISCIP_BASE = character(0),
  ANO_ELEITORAL_NAC = character(0),
  TX_DOMINANCIA = character(0),
  ORC_TOTAL_TX = character(0),
  DISCR_TX = character(0),
  IDEOL_PRSD = character(0),
  TX_VETO = character(0),
  TX_PARTC = character(0)
)


# ============================================================
# 3. CONSTRUIR MATRIZES DE COMPATIBILIDADE
# ============================================================

criar_matriz_compatibilidade <- function(variaveis, pode = list()) {
  
  mat <- matrix(
    FALSE,
    nrow = length(variaveis),
    ncol = length(variaveis),
    dimnames = list(variaveis, variaveis)
  )
  
  diag(mat) <- TRUE
  
  # Compatibilidade simétrica: basta uma lista mencionar o par.
  for (v in names(pode)) {
    if (v %in% variaveis) {
      permitidas <- intersect(pode[[v]], variaveis)
      mat[v, permitidas] <- TRUE
      mat[permitidas, v] <- TRUE
    }
  }
  return(mat)
}


compat_pnacac <- criar_matriz_compatibilidade(variaveis,pode_pnacac)

compat_pnaacto <- criar_matriz_compatibilidade(variaveis,pode_pnaacto)


modelo_valido <- function(vars, compatibilidade) {
  
  if (length(vars) <= 1) {
    return(TRUE)
  }
  
  pares <- combn(vars, 2)
  
  all(vapply(
    seq_len(ncol(pares)),
    function(i) {
      compatibilidade[pares[1, i], pares[2, i]]
    },
    logical(1)
  ))
}


# ============================================================
# 4. ENUMERAR TODOS OS SUBCONJUNTOS VÁLIDOS
# ============================================================

enumerar_modelos <- function(
    variaveis, compatibilidade, max_k = 5) {
  
  modelos_por_k <- vector("list", max_k + 1)
  names(modelos_por_k) <- as.character(0:max_k)
  
  for (k in 0:max_k) {
    
    if (k == 0) {
      modelos_por_k[[as.character(k)]] <- list(character(0))
      next
    }
    
    candidatos <- combn(
      variaveis,
      k,
      simplify = FALSE
    )
    
    validos <- Filter(
      function(vars) modelo_valido(vars, compatibilidade),
      candidatos
    )
    
    modelos_por_k[[as.character(k)]] <- validos
    
    message(
      "k = ", k,
      ": ", length(validos),
      " modelos válidos."
    )
  }
  
  modelos_por_k
}


modelos_pnacac <- enumerar_modelos(variaveis, compat_pnacac, max_k)

modelos_pnaacto <- enumerar_modelos(variaveis, compat_pnaacto, max_k)


# Quantidade de combinações bivariadas por k.
contagens <- data.frame(
  k = 0:max_k,
  PNACAC = vapply(
    modelos_pnacac, length, integer(1)
  ),
  PNAACTO = vapply(
    modelos_pnaacto, length, integer(1)
  )
)

contagens$combinacoes <- contagens$PNACAC *contagens$PNAACTO
print(contagens)

total_modelos <- sum(contagens$combinacoes)

message(
  "Total de modelos bivariados a ajustar: ",
  total_modelos
)


# ============================================================
# 5. FUNÇÃO DE VEROSSIMILHANÇA LOG-NORMAL + CLAYTON
# ============================================================

verosi_ln_ln <- function(param, X1, X2, y1, y2, cens1, cens2) {
  
  p1 <- ncol(X1)
  p2 <- ncol(X2)
  
  beta1 <- param[seq_len(p1)]
  beta2 <- param[p1 + seq_len(p2)]
  
  sigma1 <- param[p1 + p2 + 1]
  sigma2 <- param[p1 + p2 + 2]
  alpha  <- param[p1 + p2 + 3]
  
  if (sigma1 <= 0 || sigma2 <= 0 || alpha <= 0) {
    return(Inf)
  }
  
  if (any(y1 <= 0) || any(y2 <= 0)) {
    return(Inf)
  }
  
  mu1 <- drop(X1 %*% beta1)
  mu2 <- drop(X2 %*% beta2)
  
  # Sobrevivências marginais log-normais.
  z1 <- (mu1 - log(y1)) / sigma1
  z2 <- (mu2 - log(y2)) / sigma2
  
  S1 <- pnorm(z1)
  S2 <- pnorm(z2)
  
  f1 <- dlnorm(y1, meanlog = mu1, sdlog = sigma1)
  f2 <- dlnorm(y2, meanlog = mu2, sdlog = sigma2)
  
  # Cópula de Clayton para as sobrevivências.
  A <- S1^(-alpha) + S2^(-alpha) - 1
  
  if (any(!is.finite(A)) || any(A <= 0)) {
    return(Inf)
  }
  
  S12 <- A^(-1 / alpha)
  
  S121 <- -A^(-1 / alpha - 1) * S1^(-alpha - 1) * f1
  
  S122 <- -A^(-1 / alpha - 1) * S2^(-alpha - 1) * f2
  
  S1212 <- (1 + alpha) * A^(-1 / alpha - 2) * S1^(-alpha - 1) * S2^(-alpha - 1) * f1 * f2
  
  
  loglik <- cens1 * cens2 * log(S1212) +
    cens1 * (1 - cens2) * log(-S121) + (1 - cens1) * cens2 * log(-S122) +
    (1 - cens1) * (1 - cens2) * log(S12)
  
  if (any(!is.finite(loglik))) {
    return(Inf)
  }
  
  -sum(loglik)
}


# ============================================================
# 6. PREPARAR UMA AMOSTRA COMUM PARA TODOS OS MODELOS
# ============================================================
#
# Usar a mesma amostra evita comparar BIC calculados com
# números diferentes de observações.
# ============================================================

colunas_obrigatorias <- c(
  "t_pnacac", "t_pnaacto",
  "ADOPT_pnacac", "ADOPT_pnaacto",
  paste0("PIB_pnacac"),
  paste0("PIB_pnaacto"),
  paste0(variaveis, "_pnacac"),
  paste0(variaveis, "_pnaacto")
)

colunas_ausentes <- setdiff(
  colunas_obrigatorias,
  names(dados)
)

if (length(colunas_ausentes) > 0) {
  stop(
    "Estas colunas não foram encontradas em dados: ",
    paste(colunas_ausentes, collapse = ", ")
  )
}

dados_modelagem <- dados[
  complete.cases(dados[, colunas_obrigatorias]),
]

message(
  "Linhas originais: ", nrow(dados),
  "; linhas completas utilizadas: ",
  nrow(dados_modelagem)
)

if (nrow(dados_modelagem) == 0) {
  stop("Não há observações completas para ajustar os modelos.")
}

if (any(dados_modelagem$t_pnacac <= 0) ||
    any(dados_modelagem$t_pnaacto <= 0)) {
  stop("Os tempos de sobrevivência devem ser positivos.")
}

if (!all(dados_modelagem$ADOPT_pnacac %in% c(0, 1)) ||
    !all(dados_modelagem$ADOPT_pnaacto %in% c(0, 1))) {
  stop("As variáveis de evento devem conter apenas 0 e 1.")
}

kendall_global <- cor(
  dados_modelagem$t_pnacac,
  dados_modelagem$t_pnaacto,
  method = "kendall"
)

if (!is.finite(kendall_global)) {
  stop("Não foi possível calcular a correlação de Kendall.")
}


# ============================================================
# 7. AJUSTAR UM MODELO BIVARIADO
# ============================================================

ajustar_modelo <- function(vars1, vars2, dados) {
  
  d1 <- data.frame(
    y = dados$t_pnacac,
    evento = dados$ADOPT_pnacac,
    PIB = dados$PIB_pnacac
  )
  
  for (v in vars1) {
    d1[[v]] <- dados[[paste0(v, "_pnacac")]]
  }
  
  d2 <- data.frame(
    y = dados$t_pnaacto,
    evento = dados$ADOPT_pnaacto,
    PIB = dados$PIB_pnaacto
  )
  
  for (v in vars2) {
    d2[[v]] <- dados[[paste0(v, "_pnaacto")]]
  }
  
  formula1 <- reformulate(
    c("PIB", vars1),
    response = "Surv(y, evento)"
  )
  
  formula2 <- reformulate(
    c("PIB", vars2),
    response = "Surv(y, evento)"
  )
  
  marginal1 <- survreg(
    formula1, data = d1, dist = "lognormal"
  )
  
  marginal2 <- survreg(
    formula2, data = d2, dist = "lognormal"
  )
  
  X1 <- model.matrix(
    reformulate(c("PIB", vars1)),
    data = d1
  )
  
  X2 <- model.matrix(
    reformulate(c("PIB", vars2)),
    data = d2
  )
  
  p1 <- ncol(X1)
  p2 <- ncol(X2)
  
  valor_inicial <- c(
    coef(marginal1),
    coef(marginal2),
    marginal1$scale,
    marginal2$scale,
    0.4
  )
  
  limites_inferiores <- c(
    rep(-Inf, p1 + p2),
    1e-6, 1e-6, 1e-6
  )
  
  ajuste <- optim(
    par = valor_inicial,
    fn = verosi_ln_ln,
    X1 = X1,
    X2 = X2,
    y1 = d1$y,
    y2 = d2$y,
    cens1 = d1$evento,
    cens2 = d2$evento,
    method = "L-BFGS-B",
    lower = limites_inferiores,
    hessian = FALSE,
    control = list(maxit = 500)
  )
  
  logLik_modelo <- -ajuste$value
  n_parametros <- length(ajuste$par)
  
  BIC_modelo <- -2 * logLik_modelo +
    n_parametros * log(nrow(dados))
  
  alpha <- ajuste$par[n_parametros]
  tau_clayton <- alpha / (alpha + 2)
  
  list(
    vars1 = vars1,
    vars2 = vars2,
    logLik = logLik_modelo,
    BIC = BIC_modelo,
    n_parametros = n_parametros,
    alpha = alpha,
    tau_clayton = tau_clayton,
    kendall = kendall_global,
    diferenca = abs(tau_clayton - kendall_global),
    convergence = ajuste$convergence,
    mensagem = if (is.null(ajuste$message)) {
      ""
    } else {
      ajuste$message
    }
  )
}


# ============================================================
# 8. GERAR TODOS OS PARES DE MODELOS VÁLIDOS
# ============================================================

pares <- do.call(
  rbind,
  lapply(0:max_k, function(k) {
    
    m1 <- modelos_pnacac[[as.character(k)]]
    m2 <- modelos_pnaacto[[as.character(k)]]
    
    if (length(m1) == 0 || length(m2) == 0) {
      return(NULL)
    }
    
    data.frame(
      k = k,
      i1 = rep(seq_along(m1), each = length(m2)),
      i2 = rep(seq_along(m2), times = length(m1))
    )
  })
)

rownames(pares) <- NULL

stopifnot(nrow(pares) == total_modelos)


# ============================================================
# 9. EXECUTAR AJUSTES COM CHECKPOINT
# ============================================================

arquivo_checkpoint <- "resultados_completos_checkpoint.csv"
arquivo_final <- "resultados_todos_modelos.csv"
arquivo_top10 <- "top10_modelos_clayton.csv"

resultados <- vector("list", nrow(pares))

for (i in seq_len(nrow(pares))) {
  
  k <- pares$k[i]
  
  vars1 <- modelos_pnacac[[as.character(k)]][[
    pares$i1[i]
  ]]
  
  vars2 <- modelos_pnaacto[[as.character(k)]][[
    pares$i2[i]
  ]]
  
  if (i %% 100 == 1) {
    message(
      "Modelo ", i, " de ", nrow(pares),
      " (", round(100 * i / nrow(pares), 2), "%)"
    )
  }
  
  ajuste <- tryCatch(
    ajustar_modelo(
      vars1 = vars1,
      vars2 = vars2,
      dados = dados_modelagem
    ),
    error = function(e) {
      list(
        vars1 = vars1,
        vars2 = vars2,
        logLik = NA_real_,
        BIC = Inf,
        n_parametros = NA_integer_,
        alpha = NA_real_,
        tau_clayton = NA_real_,
        kendall = kendall_global,
        diferenca = NA_real_,
        convergence = 99L,
        mensagem = conditionMessage(e)
      )
    }
  )
  
  resultados[[i]] <- data.frame(
    k = k,
    
    PNACAC = paste(
      c("PIB", vars1),
      collapse = " + "
    ),
    
    PNAACTO = paste(
      c("PIB", vars2),
      collapse = " + "
    ),
    
    logLik = ajuste$logLik,
    BIC = ajuste$BIC,
    alpha = ajuste$alpha,
    tau_clayton = ajuste$tau_clayton,
    correlacao_kendall = ajuste$kendall,
    diferenca_tau_kendall = ajuste$diferenca,
    n_parametros = ajuste$n_parametros,
    convergencia = ajuste$convergence,
    mensagem = ajuste$mensagem,
    
    stringsAsFactors = FALSE
  )
  
  # Salvar checkpoint a cada 100 ajustes e no último.
  if (i %% 100 == 0 || i == nrow(pares)) {
    
    parcial <- do.call(
      rbind,
      resultados[seq_len(i)]
    )
    
    write.csv(
      parcial,
      arquivo_checkpoint,
      row.names = FALSE,
      na = ""
    )
  }
}


# ============================================================
# 10. SALVAR TODOS OS RESULTADOS
# ============================================================

resultados_df <- do.call(rbind, resultados)

write.csv(
  resultados_df,
  arquivo_final,
  row.names = FALSE,
  na = ""
)


# ============================================================
# 11. SELECIONAR MODELOS CONVERGIDOS
# ============================================================
#
# Só entram no ranking os ajustes com convergence = 0,
# BIC finito e diferença finita.
# ============================================================

validos <- resultados_df[
  resultados_df$convergencia == 0 &
    is.finite(resultados_df$BIC) &
    is.finite(resultados_df$diferenca_tau_kendall),
]

if (nrow(validos) == 0) {
  stop(
    "Nenhum modelo convergiu com critérios válidos. ",
    "Confira os resultados e a função de verossimilhança."
  )
}


# ============================================================
# 12. RANKING GLOBAL E TOP 10
# ============================================================

validos$posto_BIC <- rank(
  validos$BIC,
  ties.method = "min"
)

validos$posto_diferenca <- rank(
  validos$diferenca_tau_kendall,
  ties.method = "min"
)

validos$pontuacao <- (
  validos$posto_BIC +
    validos$posto_diferenca
)

validos <- validos[
  order(
    validos$pontuacao,
    validos$BIC,
    validos$diferenca_tau_kendall
  ),
]

validos$posicao <- seq_len(nrow(validos))

top10 <- head(validos, 10)

# Organizar as colunas de apresentação.
top10 <- top10[, c(
  "posicao",
  "k",
  "PNACAC",
  "PNAACTO",
  "logLik",
  "BIC",
  "alpha",
  "tau_clayton",
  "correlacao_kendall",
  "diferenca_tau_kendall",
  "posto_BIC",
  "posto_diferenca",
  "pontuacao",
  "n_parametros",
  "convergencia"
)]

print(top10, row.names = FALSE)

write.csv(
  top10,
  arquivo_top10,
  row.names = FALSE,
  na = ""
)

message("Execução concluída.")
message("Resultados completos: ", arquivo_final)
message("Top 10: ", arquivo_top10)