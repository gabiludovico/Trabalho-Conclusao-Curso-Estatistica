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

# Tratando Dados
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
  select(-t,-SIGUF,-MUNICÍPIO,-AREA,-ADOPT,-ANO,-DPST_CTL,-TX_CTL,-DSPT_CTPTD,-TX_CTPTD,-NUM_PALAVRA,
         -NUM_DSPT,-MLTSTLD,-NUM_ENTIDADES,-DISCR_DOTACAO,-DISCR_EMPENHADO,-ORC_TOTAL_DOTACAO,
         -ORC_TOTAL_EMPENHADO,-TI_PROJ_LEI,-TI_EMPENHADO,-INDICE_MNST,-SERVIDORES,
         -SERVIDORES_RJU,-PIB) %>%
  rename(`Eleição Municipal` = ANO_ELEITORAL_MUNIC,
         `Fragmentação Partidária` = TX_FRAG_PARTD_NEP,
         `Taxa TI` = TI_TX,
         `Ideologia Prefeito` = IDEOL_PRF,
         `Taxa Sucesso` = TX_SUCESSO,
         `Tempo Gestão` = T_MEDIO_PERMAN_MINISTRO_P_MANDATO,
         `Ideologia Presidente` = IDEOL_PRSD,
         `Taxa Participação`= TX_PARTC,
         `Orc Total` = ORC_TOTAL_TX,
         `Orc Discrisionário` = DISCR_TX,
         `Carreiras` = `CARREIRAS`,
         `Taxa Disciplina` = `TX_DISCIP_BASE`,
         `Taxa Servidores` = PROP_SERVIDORES,
         `Taxa Conflito` = TX_CONFLITO_MP,
         `Eleição Nacional` = ANO_ELEITORAL_NAC,
         `Taxa Veto` = TX_VETO,
         `Taxa Dominancia` = TX_DOMINANCIA,
         `Proporção Ministerial` = TX_PROP_MNST,
         `Popularidade Governo` = POP_PRSD,
         `Base Governista` = TAM_PROP_BASE,
         `Margem Presidente` = MRG_PRSD)


# Separando as bases
dados_pnacac <- dados %>%
  filter(POL_ABV == "pnacac") %>%
  select(-POL_ABV)

dados_pnaacto <- dados %>%
  filter(POL_ABV == "pnaacto") %>%
  select(-POL_ABV)


# Matriz de correlação de Pearson
matriz_cor_pnacac <- cor(
  dados_pnacac,
  use = "pairwise.complete.obs",
  method = "pearson"
)


matriz_cor_pnaacto <- cor(
  dados_pnaacto,
  use = "pairwise.complete.obs",
  method = "pearson"
)

# Exibir a matriz completa
round(matriz_cor_pnacac,3)
round(matriz_cor_pnaacto, 3)


# Grafico das matrizes de correlacao
corrplot(
  matriz_cor_pnacac,
  method = "color",
  type = "upper",
  order = "FPC",
  addCoef.col = "black",
  tl.col = "black",
  tl.cex = 0.5,
  tl.srt = 45,
  rect.lwd = 2,
  number.cex = 0.5,
  cl.cex = 0.6
)

corrplot(
  matriz_cor_pnaacto,
  method = "color",
  type = "upper",
  order = "FPC",
  addCoef.col = "black",
  tl.col = "black",
  tl.cex = 0.5,
  tl.srt = 45,
  rect.lwd = 2,
  number.cex = 0.5,
  cl.cex = 0.6
)
