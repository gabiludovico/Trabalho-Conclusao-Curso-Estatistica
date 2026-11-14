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

# Preparando dados

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
  

###########################################################
####################### POLÍTICA PNACAC ##################
###########################################################

# dados

dados_pnacac <- dados%>%
  filter(POL_ABV == "pnacac")


# km do tempo

km <- survfit(Surv(dados_pnacac$t, dados_pnacac$ADOPT) ~ 1, conf.int = F)
plot(km, conf.int = F, xlab = "Tempo", ylab = "S(t)")
plot(km, conf.int = F, xlab = "Tempo", ylab = "S(t)", mark.time = T)


# INTERPRETAÇÃO: a curva de sobrevivencia dos tempos mostra que 
# os municipios nao aderem muito a essa politica ao longo do tempo
# ate o final do periodo de 2022, a chance de um municipio nao aderir
# a essa politica esta na escala dos 80%

# TTT plot
TTT(dados_pnacac$t, lwd = 2.5, col = "cyan", grid = T, lty = 2)

# INTERPRETAÇÃO: quanto mais o tempo passa, maior o risco
# instantanio de um municipio aderir a essa politica


# Risco acumulado
plot(km,fun = "cumhaz",xlab = "Tempo",ylab = "Risco acumulado")

# INTERPRETAÇÃO: o risco acumulado sobe de maneira gradual ao longo de todo 
# o tempo, mostrando que ate o final do periodo a chance de o municipio
# aderir uma politica nao passa de 25% 


## boxplot variaveis explicativas

# TX_VETO (taxa de veto presidencial aos projetos de lei)
boxplot(dados_pnacac$TX_VETO~dados_pnacac$ADOPT)

# TX_CONFLITO_MP (taxa de medidas provisorias vetadas,rejeitadas ou nao votadas)
boxplot(dados_pnacac$TX_CONFLITO_MP~dados_pnacac$ADOPT)

# TX_PARTC (percentual de projetos apresentados pelo executivo em relacao aos enviados ao congresso)
boxplot(dados_pnacac$TX_PARTC~dados_pnacac$ADOPT)

# TX_SUCESSO (percentual de projetos apresentados pelo executivo em relacao ao total de apresentados)
boxplot(dados_pnacac$TX_SUCESSO~dados_pnacac$ADOPT)

# TX_DOMINANCIA (percentual de projetos do Executivo aprovados em relação ao total de projetos aprovados pelo Congresso)
boxplot(dados_pnacac$TX_DOMINANCIA~dados_pnacac$ADOPT)

# TX_FRAG_PARTD_NEP (número de partidos existentes, considerando o peso relativo de cada um)
boxplot(dados_pnacac$TX_FRAG_PARTD_NEP~dados_pnacac$ADOPT)

# TAM_PROP_BASE (percentual de deputados da base governista em relação ao total de deputados)
boxplot(dados_pnacac$TAM_PROP_BASE~dados_pnacac$ADOPT)

# TX_DISCIP_BASE (percentual dos deputados que votaram de acordo com a orientação do líder em votações nominais)
boxplot(dados_pnacac$TX_DISCIP_BASE~dados_pnacac$ADOPT)

# TX_PROP_MNST (relação entre o tamanho relativo de cada partido na base governista e o número de ministérios ocupados)
boxplot(dados_pnacac$TX_PROP_MNST~dados_pnacac$ADOPT)

# POP_PRSD (taxa de avaliação "ótimo" e "bom" do governo)
boxplot(dados_pnacac$POP_PRSD~dados_pnacac$ADOPT)

# T_MEDIO_PERMAN_MINISTRO_P_MANDATO (média dos meses em que os ministros permaneceram no cargo em cada gestão)
boxplot(dados_pnacac$T_MEDIO_PERMAN_MINISTRO_P_MANDATO~dados_pnacac$ADOPT)

# CARREIRAS (pontuação do ministério em relação à existência de carreira finalística própria e transversal)
boxplot(dados_pnacac$CARREIRAS~dados_pnacac$ADOPT)

# NUM_ENTIDADES (número de autarquia, fundações e empresas estatais vinculadas a cada ministério)
boxplot(dados_pnacac$NUM_ENTIDADES~dados_pnacac$ADOPT)

# PROP_SERVIDORES (prop entre servidores efetivos e total)
boxplot(dados_pnacac$PROP_SERVIDORES~dados_pnacac$ADOPT)

# DISCR_TX (percentual de orçamento discricionário empenhado em relação ao autorizado)
boxplot(dados_pnacac$DISCR_TX~dados_pnacac$ADOPT)

# ORC_TOTAL_TX (percentual de orçamento total empenhado em relação ao autorizado)
boxplot(dados_pnacac$ORC_TOTAL_TX~dados_pnacac$ADOPT)

# TI_TX  (taxa de orçamento de TI executado sobre TI previsto )
boxplot(dados_pnacac$TI_TX~dados_pnacac$ADOPT)

# INDICE_MNST (Índice Ministerial)
boxplot(dados_pnacac$INDICE_MNST~dados_pnacac$ADOPT)

# PIB (log do PIB do município)
boxplot(dados_pnacac$PIB~dados_pnacac$ADOPT)

# ANO_ELEITORAL_MUNIC (situação no ano de adoção da política, se ano eleitoral municipal ou não)
boxplot(dados_pnacac$ANO_ELEITORAL_MUNIC~dados_pnacac$ADOPT)

# ANO_ELEITORAL_NAC (situação no ano de adoção da política, se ano eleitoral nacional ou não)
boxplot(dados_pnacac$ANO_ELEITORAL_NAC~dados_pnacac$ADOPT)

# MRG_PRSD (margem de vitória do presidente em relação ao segundo colocado) eu acho
boxplot(dados_pnacac$MRG_PRSD~dados_pnacac$ADOPT)

# IDEOL_PRSD (ideologia do partido do presidente)
boxplot(dados_pnacac$IDEOL_PRSD~dados_pnacac$ADOPT)

# IDEOL_PRF (ideologia do partido do prefeito)
boxplot(dados_pnacac$IDEOL_PRF~dados_pnacac$ADOPT)


# ============================================================
# 1. Testando Ajustes de Modelos
# ============================================================

# Apenas 1 variável

## Dimensão governabilidade
m_lognormal <- survreg(Surv(t, ADOPT) ~ TX_VETO, data = dados_pnacac,dist = "lognormal");summary(m_lognormal)
m_lognormal <- survreg(Surv(t, ADOPT) ~ TX_CONFLITO_MP, data = dados_pnacac,dist = "lognormal");summary(m_lognormal)
m_lognormal <- survreg(Surv(t, ADOPT) ~ TX_PARTC, data = dados_pnacac,dist = "lognormal");summary(m_lognormal)
m_lognormal <- survreg(Surv(t, ADOPT) ~ TX_SUCESSO, data = dados_pnacac,dist = "lognormal");summary(m_lognormal)
m_lognormal <- survreg(Surv(t, ADOPT) ~ TX_DOMINANCIA, data = dados_pnacac,dist = "lognormal");summary(m_lognormal)
m_lognormal <- survreg(Surv(t, ADOPT) ~ TX_FRAG_PARTD_NEP, data = dados_pnacac,dist = "lognormal");summary(m_lognormal)
m_lognormal <- survreg(Surv(t, ADOPT) ~ TAM_PROP_BASE, data = dados_pnacac,dist = "lognormal");summary(m_lognormal)
m_lognormal <- survreg(Surv(t, ADOPT) ~ TX_DISCIP_BASE, data = dados_pnacac,dist = "lognormal");summary(m_lognormal)
m_lognormal <- survreg(Surv(t, ADOPT) ~ TX_PROP_MNST, data = dados_pnacac,dist = "lognormal");summary(m_lognormal)
m_lognormal <- survreg(Surv(t, ADOPT) ~ POP_PRSD, data = dados_pnacac,dist = "lognormal");summary(m_lognormal)
m_lognormal <- survreg(Surv(t, ADOPT) ~ T_MEDIO_PERMAN_MINISTRO_P_MANDATO, data = dados_pnacac,dist = "lognormal");summary(m_lognormal)


## Dimensão capacidades institucionais
m_lognormal <- survreg(Surv(t, ADOPT) ~ CARREIRAS, data = dados_pnacac,dist = "lognormal");summary(m_lognormal)
m_lognormal <- survreg(Surv(t, ADOPT) ~ PROP_SERVIDORES, data = dados_pnacac,dist = "lognormal");summary(m_lognormal)
m_lognormal <- survreg(Surv(t, ADOPT) ~ DISCR_TX, data = dados_pnacac,dist = "lognormal");summary(m_lognormal)
m_lognormal <- survreg(Surv(t, ADOPT) ~ ORC_TOTAL_TX, data = dados_pnacac,dist = "lognormal");summary(m_lognormal)
m_lognormal <- survreg(Surv(t, ADOPT) ~ TI_TX, data = dados_pnacac,dist = "lognormal");summary(m_lognormal)


## Dimensão eleitoral
m_lognormal <- survreg(Surv(t, ADOPT) ~ ANO_ELEITORAL_MUNIC, data = dados_pnacac,dist = "lognormal");summary(m_lognormal)
m_lognormal <- survreg(Surv(t, ADOPT) ~ ANO_ELEITORAL_NAC, data = dados_pnacac,dist = "lognormal");summary(m_lognormal)
m_lognormal <- survreg(Surv(t, ADOPT) ~ IDEOL_PRSD, data = dados_pnacac,dist = "lognormal");summary(m_lognormal)
m_lognormal <- survreg(Surv(t, ADOPT) ~ IDEOL_PRF, data = dados_pnacac,dist = "lognormal");summary(m_lognormal)

 
## Variável controle
m_lognormal <- survreg(Surv(t, ADOPT) ~ PIB, data = dados_pnacac,dist = "lognormal");summary(m_lognormal)

## Testando modelos com uma variável de cada dimensão + variável controle
## e considerando variáveis com baixa correlação entre si

# op1 
m_lognormal <- survreg(
  Surv(t, ADOPT) ~ 
    TX_SUCESSO +
    CARREIRAS +
    PIB +
    ANO_ELEITORAL_MUNIC,
  data = dados_pnacac,
  dist = "lognormal"
);summary(m_lognormal)


#op2
m_lognormal <- survreg(
  Surv(t, ADOPT) ~ 
    T_MEDIO_PERMAN_MINISTRO_P_MANDATO +
    PROP_SERVIDORES +
    PIB +
    IDEOL_PRF,
  data = dados_pnacac,
  dist = "lognormal"
);summary(m_lognormal)


# op3
m_lognormal <- survreg(
  Surv(t, ADOPT) ~ 
    T_MEDIO_PERMAN_MINISTRO_P_MANDATO +
    ORC_TOTAL_TX  +
    PIB +
    IDEOL_PRF,
  data = dados_pnacac,
  dist = "lognormal"
);summary(m_lognormal)


# op4
m_lognormal <- survreg(
  Surv(t, ADOPT) ~ 
    TX_SUCESSO +
    TI_TX  +
    PIB +
    ANO_ELEITORAL_MUNIC,
  data = dados_pnacac,
  dist = "lognormal"
);summary(m_lognormal)


###########################################################
###################### POLÍTICA PNAACTO ##################
###########################################################

# dados

dados_pnaacto <- dados%>%
  filter(POL_ABV == "pnaacto")

# km do tempo

km <- survfit(Surv(dados_pnaacto$t, dados_pnaacto$ADOPT) ~ 1, conf.int = F)
plot(km, conf.int = F, xlab = "Tempo", ylab = "S(t)")
plot(km, conf.int = F, xlab = "Tempo", ylab = "S(t)", mark.time = T)


# INTERPRETAÇÃO: a curva de sobrevivencia dos tempos mostra que 
# os municipios nao aderem muito a essa politica ao longo do tempo
# ate o final do periodo de 2022, a chance de um municipio nao aderir
# a essa politica esta na escala dos 80%

# TTT plot
TTT(dados_pnaacto$t, lwd = 2.5, col = "cyan", grid = T, lty = 2)

# INTERPRETAÇÃO: quanto mais o tempo passa, maior o risco
# instantanio de um municipio aderir a essa politica


# Risco acumulado
plot(km,fun = "cumhaz",xlab = "Tempo",ylab = "Risco acumulado")

# INTERPRETAÇÃO: o risco acumulado sobe em formato de escada,
# demonstrando as épocas em que ocorreram adesões da politica
# apesar de observar esse fenômeno a chance de 
# aderir uma politica nao passou de 12% 


## boxplot variaveis explicativas

# TX_VETO (taxa de veto presidencial aos projetos de lei)
boxplot(dados_pnaacto$TX_VETO~dados_pnaacto$ADOPT)

# TX_CONFLITO_MP (taxa de medidas provisorias vetadas,rejeitadas ou nao votadas)
boxplot(dados_pnaacto$TX_CONFLITO_MP~dados_pnaacto$ADOPT)

# TX_PARTC (percentual de projetos apresentados pelo executivo em relacao aos enviados ao congresso)
boxplot(dados_pnaacto$TX_PARTC~dados_pnaacto$ADOPT)

# TX_SUCESSO (percentual de projetos apresentados pelo executivo em relacao ao total de apresentados)
boxplot(dados_pnaacto$TX_SUCESSO~dados_pnaacto$ADOPT)

# TX_DOMINANCIA (percentual de projetos do Executivo aprovados em relação ao total de projetos aprovados pelo Congresso)
boxplot(dados_pnaacto$TX_DOMINANCIA~dados_pnaacto$ADOPT)

# TX_FRAG_PARTD_NEP (número de partidos existentes, considerando o peso relativo de cada um)
boxplot(dados_pnaacto$TX_FRAG_PARTD_NEP~dados_pnaacto$ADOPT)

# TAM_PROP_BASE (percentual de deputados da base governista em relação ao total de deputados)
boxplot(dados_pnaacto$TAM_PROP_BASE~dados_pnaacto$ADOPT)

# TX_DISCIP_BASE (percentual dos deputados que votaram de acordo com a orientação do líder em votações nominais)
boxplot(dados_pnaacto$TX_DISCIP_BASE~dados_pnaacto$ADOPT)

# TX_PROP_MNST (relação entre o tamanho relativo de cada partido na base governista e o número de ministérios ocupados)
boxplot(dados_pnaacto$TX_PROP_MNST~dados_pnaacto$ADOPT)

# POP_PRSD (taxa de avaliação "ótimo" e "bom" do governo)
boxplot(dados_pnaacto$POP_PRSD~dados_pnaacto$ADOPT)

# T_MEDIO_PERMAN_MINISTRO_P_MANDATO (média dos meses em que os ministros permaneceram no cargo em cada gestão)
boxplot(dados_pnaacto$T_MEDIO_PERMAN_MINISTRO_P_MANDATO~dados_pnaacto$ADOPT)

# CARREIRAS (pontuação do ministério em relação à existência de carreira finalística própria e transversal)
boxplot(dados_pnaacto$CARREIRAS~dados_pnaacto$ADOPT)

# NUM_ENTIDADES (número de autarquia, fundações e empresas estatais vinculadas a cada ministério)
boxplot(dados_pnaacto$NUM_ENTIDADES~dados_pnaacto$ADOPT)

# PROP_SERVIDORES (prop entre servidores efetivos e total)
boxplot(dados_pnaacto$PROP_SERVIDORES~dados_pnaacto$ADOPT)

# DISCR_TX (percentual de orçamento discricionário empenhado em relação ao autorizado)
boxplot(dados_pnaacto$DISCR_TX~dados_pnaacto$ADOPT)

# ORC_TOTAL_TX (percentual de orçamento total empenhado em relação ao autorizado)
boxplot(dados_pnaacto$ORC_TOTAL_TX~dados_pnaacto$ADOPT)

# TI_TX  (taxa de orçamento de TI executado sobre TI previsto )
boxplot(dados_pnaacto$TI_TX~dados_pnaacto$ADOPT)

# INDICE_MNST (Índice Ministerial)
boxplot(dados_pnaacto$INDICE_MNST~dados_pnaacto$ADOPT)

# PIB (log do PIB do município)
boxplot(dados_pnaacto$PIB~dados_pnaacto$ADOPT)

# ANO_ELEITORAL_MUNIC (situação no ano de adoção da política, se ano eleitoral municipal ou não)
boxplot(dados_pnaacto$ANO_ELEITORAL_MUNIC~dados_pnaacto$ADOPT)

# ANO_ELEITORAL_NAC (situação no ano de adoção da política, se ano eleitoral nacional ou não)
boxplot(dados_pnaacto$ANO_ELEITORAL_NAC~dados_pnaacto$ADOPT)

# MRG_PRSD (margem de vitória do presidente em relação ao segundo colocado) eu acho
boxplot(dados_pnaacto$MRG_PRSD~dados_pnaacto$ADOPT)

# IDEOL_PRSD (ideologia do partido do presidente)
boxplot(dados_pnaacto$IDEOL_PRSD~dados_pnaacto$ADOPT)

# IDEOL_PRF (ideologia do partido do prefeito)
boxplot(dados_pnaacto$IDEOL_PRF~dados_pnaacto$ADOPT)

# ============================================================
# 1. Testando Ajustes de Modelos
# ============================================================

### Distribuição Log-Normal

# Apenas 1 variável 

## Dimensão governabilidade
m_lognormal2 <- survreg(Surv(t, ADOPT) ~ TX_VETO, data = dados_pnaacto,dist = "lognormal");summary(m_lognormal2)
m_lognormal2 <- survreg(Surv(t, ADOPT) ~ TX_CONFLITO_MP, data = dados_pnaacto,dist = "lognormal");summary(m_lognormal2)
m_lognormal2 <- survreg(Surv(t, ADOPT) ~ TX_PARTC, data = dados_pnaacto,dist = "lognormal");summary(m_lognormal2)
m_lognormal2 <- survreg(Surv(t, ADOPT) ~ TX_SUCESSO, data = dados_pnaacto,dist = "lognormal");summary(m_lognormal2)
m_lognormal2 <- survreg(Surv(t, ADOPT) ~ TX_DOMINANCIA, data = dados_pnaacto,dist = "lognormal");summary(m_lognormal2)
m_lognormal2 <- survreg(Surv(t, ADOPT) ~ TX_FRAG_PARTD_NEP, data = dados_pnaacto,dist = "lognormal");summary(m_lognormal2)
m_lognormal2 <- survreg(Surv(t, ADOPT) ~ TAM_PROP_BASE, data = dados_pnaacto,dist = "lognormal");summary(m_lognormal2)
m_lognormal2 <- survreg(Surv(t, ADOPT) ~ TX_DISCIP_BASE, data = dados_pnaacto,dist = "lognormal");summary(m_lognormal2)
m_lognormal2 <- survreg(Surv(t, ADOPT) ~ TX_PROP_MNST, data = dados_pnaacto,dist = "lognormal");summary(m_lognormal2)
m_lognormal2 <- survreg(Surv(t, ADOPT) ~ POP_PRSD, data = dados_pnaacto,dist = "lognormal");summary(m_lognormal2)
m_lognormal2 <- survreg(Surv(t, ADOPT) ~ T_MEDIO_PERMAN_MINISTRO_P_MANDATO, data = dados_pnaacto,dist = "lognormal");summary(m_lognormal2)


## Dimensão capacidades institucionais
m_lognormal2 <- survreg(Surv(t, ADOPT) ~ CARREIRAS, data = dados_pnaacto,dist = "lognormal");summary(m_lognormal2)
m_lognormal2 <- survreg(Surv(t, ADOPT) ~ PROP_SERVIDORES, data = dados_pnaacto,dist = "lognormal");summary(m_lognormal2)
m_lognormal2 <- survreg(Surv(t, ADOPT) ~ DISCR_TX, data = dados_pnaacto,dist = "lognormal");summary(m_lognormal2)
m_lognormal2 <- survreg(Surv(t, ADOPT) ~ ORC_TOTAL_TX, data = dados_pnaacto,dist = "lognormal");summary(m_lognormal2)
m_lognormal2 <- survreg(Surv(t, ADOPT) ~ TI_TX, data = dados_pnaacto,dist = "lognormal");summary(m_lognormal2)


## Dimensão eleitoral
m_lognormal2 <- survreg(Surv(t, ADOPT) ~ ANO_ELEITORAL_MUNIC, data = dados_pnaacto,dist = "lognormal");summary(m_lognormal2)
m_lognormal2 <- survreg(Surv(t, ADOPT) ~ ANO_ELEITORAL_NAC, data = dados_pnaacto,dist = "lognormal");summary(m_lognormal2)
m_lognormal2 <- survreg(Surv(t, ADOPT) ~ IDEOL_PRSD, data = dados_pnaacto,dist = "lognormal");summary(m_lognormal2)
m_lognormal2 <- survreg(Surv(t, ADOPT) ~ IDEOL_PRF, data = dados_pnaacto,dist = "lognormal");summary(m_lognormal2)


## Variável controle
m_lognormal2 <- survreg(Surv(t, ADOPT) ~ PIB, data = dados_pnaacto,dist = "lognormal");summary(m_lognormal2)


## Testando modelos com uma variável de cada dimensão + variável controle
## e considerando variáveis com baixa correlação entre si

# op1
m_lognormal2 <- survreg(
  Surv(t, ADOPT) ~ 
    TX_SUCESSO +
    CARREIRAS +
    PIB +
    IDEOL_PRF,
  data = dados_pnaacto,
  dist = "lognormal"
);summary(m_lognormal2)

# op2 
m_lognormal2 <- survreg(
  Surv(t, ADOPT) ~ 
    POP_PRSD  +
    TI_TX +
    PIB +
    ANO_ELEITORAL_MUNIC,
  data = dados_pnaacto,
  dist = "lognormal");summary(m_lognormal2)

# op3

m_lognormal2 <- survreg(
  Surv(t, ADOPT) ~ 
    TX_FRAG_PARTD_NEP +
    ORC_TOTAL_TX  +
    PIB +
    IDEOL_PRF,
  data = dados_pnaacto,
  dist = "lognormal");summary(m_lognormal2)


# op4

m_lognormal2 <- survreg(
  Surv(t, ADOPT) ~ 
    TX_DISCIP_BASE +
    CARREIRAS +
    PIB +
    ANO_ELEITORAL_MUNIC ,
  data = dados_pnaacto,
  dist = "lognormal");summary(m_lognormal2)


# op5
m_lognormal2 <- survreg(
  Surv(t, ADOPT) ~ 
    TX_FRAG_PARTD_NEP +
    CARREIRAS +
    PIB +
    IDEOL_PRF ,
  data = dados_pnaacto,
  dist = "lognormal");summary(m_lognormal2)

### Distribuição Exponencial

# Apenas 1 variável 

## Dimensão governabilidade
m_exponencial <- survreg(Surv(t, ADOPT) ~ TX_VETO, data = dados_pnaacto,dist = "exponential");summary(m_exponencial)
m_exponencial <- survreg(Surv(t, ADOPT) ~ TX_CONFLITO_MP, data = dados_pnaacto,dist = "exponential");summary(m_exponencial)
m_exponencial <- survreg(Surv(t, ADOPT) ~ TX_PARTC, data = dados_pnaacto,dist = "exponential");summary(m_exponencial)
m_exponencial <- survreg(Surv(t, ADOPT) ~ TX_SUCESSO, data = dados_pnaacto,dist = "exponential");summary(m_exponencial)
m_exponencial <- survreg(Surv(t, ADOPT) ~ TX_DOMINANCIA, data = dados_pnaacto,dist = "exponential");summary(m_exponencial)
m_exponencial <- survreg(Surv(t, ADOPT) ~ TX_FRAG_PARTD_NEP, data = dados_pnaacto,dist = "exponential");summary(m_exponencial)
m_exponencial <- survreg(Surv(t, ADOPT) ~ TAM_PROP_BASE, data = dados_pnaacto,dist = "exponential");summary(m_exponencial)
m_exponencial <- survreg(Surv(t, ADOPT) ~ TX_DISCIP_BASE, data = dados_pnaacto,dist = "exponential");summary(m_exponencial)
m_exponencial <- survreg(Surv(t, ADOPT) ~ TX_PROP_MNST, data = dados_pnaacto,dist = "exponential");summary(m_exponencial)
m_exponencial <- survreg(Surv(t, ADOPT) ~ POP_PRSD, data = dados_pnaacto,dist = "exponential");summary(m_exponencial)
m_exponencial <- survreg(Surv(t, ADOPT) ~ T_MEDIO_PERMAN_MINISTRO_P_MANDATO, data = dados_pnaacto,dist = "exponential");summary(m_exponencial)


## Dimensão capacidades institucionais
m_exponencial <- survreg(Surv(t, ADOPT) ~ CARREIRAS, data = dados_pnaacto,dist = "exponential");summary(m_exponencial)
m_exponencial <- survreg(Surv(t, ADOPT) ~ PROP_SERVIDORES, data = dados_pnaacto,dist = "exponential");summary(m_exponencial)
m_exponencial <- survreg(Surv(t, ADOPT) ~ DISCR_TX, data = dados_pnaacto,dist = "exponential");summary(m_exponencial)
m_exponencial <- survreg(Surv(t, ADOPT) ~ ORC_TOTAL_TX, data = dados_pnaacto,dist = "exponential");summary(m_exponencial)
m_exponencial <- survreg(Surv(t, ADOPT) ~ TI_TX, data = dados_pnaacto,dist = "exponential");summary(m_exponencial)


## Dimensão eleitoral
m_exponencial <- survreg(Surv(t, ADOPT) ~ ANO_ELEITORAL_MUNIC, data = dados_pnaacto,dist = "exponential");summary(m_exponencial)
m_exponencial <- survreg(Surv(t, ADOPT) ~ ANO_ELEITORAL_NAC, data = dados_pnaacto,dist = "exponential");summary(m_exponencial)
m_exponencial <- survreg(Surv(t, ADOPT) ~ IDEOL_PRSD, data = dados_pnaacto,dist = "exponential");summary(m_exponencial)
m_exponencial <- survreg(Surv(t, ADOPT) ~ IDEOL_PRF, data = dados_pnaacto,dist = "exponential");summary(m_exponencial)


## Variável controle
m_exponencial <- survreg(Surv(t, ADOPT) ~ PIB, data = dados_pnaacto,dist = "exponential");summary(m_exponencial)


## Testando modelos com uma variável de cada dimensão + variável controle
## e considerando variáveis com baixa correlação entre si

# op1
m_exponencial <- survreg(
  Surv(t, ADOPT) ~ 
    TX_SUCESSO +
    CARREIRAS +
    PIB +
    IDEOL_PRF,
  data = dados_pnaacto,
  dist = "exponential"
);summary(m_exponencial)

# op2 
m_exponencial <- survreg(
  Surv(t, ADOPT) ~ 
    POP_PRSD  +
    TI_TX +
    PIB +
    ANO_ELEITORAL_MUNIC,
  data = dados_pnaacto,
  dist = "exponential");summary(m_exponencial)

# op3

m_exponencial <- survreg(
  Surv(t, ADOPT) ~ 
    TX_FRAG_PARTD_NEP +
    ORC_TOTAL_TX  +
    PIB +
    IDEOL_PRF,
  data = dados_pnaacto,
  dist = "exponential");summary(m_exponencial)


# op4

m_exponencial <- survreg(
  Surv(t, ADOPT) ~ 
    TX_DISCIP_BASE +
    CARREIRAS +
    PIB +
    ANO_ELEITORAL_MUNIC ,
  data = dados_pnaacto,
  dist = "exponential");summary(m_exponencial)


# op5
m_exponencial <- survreg(
  Surv(t, ADOPT) ~ 
    TX_FRAG_PARTD_NEP +
    CARREIRAS +
    PIB +
    IDEOL_PRF ,
  data = dados_pnaacto,
  dist = "exponential");summary(m_exponencial)
