library(readODS)
library(survival)
library(survminer)
library(dplyr)
library(tidyr)
library(corrplot)
library(AdequacyModel)
library(ggplot2)
library(car)
library(scales)
library(knitr)
library(xtable)

dados <- read_ods("/dados/unb/TCC-2/Dados/Base_saude.ods")

# padronizando variaveis
dados <- dados %>%
  mutate(TI_TX = TI_EMPENHADO/TI_PROJ_LEI,
         CARREIRAS = CARREIRAS/3) %>%
  filter(POL_ABV %in% c("pnacac","pnaacto"))


dados_pnacac <- dados%>%
  filter(POL_ABV == "pnacac")

dados_pnaacto <- dados%>%
  filter(POL_ABV == "pnaacto")


dados_conjunto <- dados %>%
  filter(
    POL_ABV %in% c("pnacac", "pnaacto")
  ) %>%
  mutate(ADOPT = case_when(
    ADOPT == 0 ~ "Não adotou",
    ADOPT == 1 ~ "Adotou",
    TRUE ~ NA_character_),
    POL_ABV = factor(
      POL_ABV,
      levels = c("pnacac", "pnaacto"),
      labels = c("PNACAC", "PNAACTO")
    ))


dados_conjunto2 <- dados %>%
  filter(
    POL_ABV %in% c("pnacac", "pnaacto")
  ) %>%
  mutate(
    POL_ABV = factor(
      POL_ABV,
      levels = c("pnacac", "pnaacto"),
      labels = c("PNACAC", "PNAACTO")
    ))


# EXPLORATORIA

# boxplot TX_SUCESSO

ggplot(
  dados_conjunto,
  aes(
    x = POL_ABV,
    y = TX_SUCESSO,
    fill = ADOPT
  )
) +
  geom_boxplot(
    width = 0.55,
    position = position_dodge(width = 0.7),
    outlier.shape = 16,
    outlier.size = 2.5
  ) +
  
  stat_summary(
    aes(group = ADOPT),
    fun = mean,
    geom = "point",
    shape = 18,
    size = 4,
    color = "black",
    position = position_dodge(width = 0.7)
  ) +
  scale_y_continuous(
    labels = scales::label_percent()
  ) +
  scale_fill_manual(
    values = c(
      "Adotou" = "#007EA7",
      "Não adotou" = "#9D0208"
    )
  ) +
  
  labs(
    x = NULL,
    y = "Taxa de sucesso",
    fill = "Adoção à política"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.text = element_text(size = 10),
    axis.text.x = element_text(color = "black"),
    axis.title.y = element_text(size = 13),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )



# quadro de medidas TX_SUCESSO

tabela_suc <- dados_conjunto %>%
  group_by(POL_ABV, ADOPT) %>%
  summarise(
    Média = mean(TX_SUCESSO),
    `Desvio-padrão` = sd(TX_SUCESSO),
    Mínimo = min(TX_SUCESSO),
    `1º Quartil` = quantile(TX_SUCESSO, 0.25),
    Mediana = median(TX_SUCESSO),
    `3º Quartil` = quantile(TX_SUCESSO, 0.75),
    Máximo = max(TX_SUCESSO),
    .groups = "drop"
  )


tabela_suc <- tabela_suc %>%
  pivot_longer(
    cols = c(
      Média,
      `Desvio-padrão`,
      Mínimo,
      `1º Quartil`,
      Mediana,
      `3º Quartil`,
      Máximo
    ),
    names_to = "Estatística",
    values_to = "Valor"
  ) %>%
  mutate(
    Grupo = paste(POL_ABV, ADOPT, sep = "_")
  ) %>%
  select(Estatística, Grupo, Valor) %>%
  pivot_wider(
    names_from = Grupo,
    values_from = Valor
  )

tabela_suc <- tabela_suc %>%
  mutate(
    across(
      -Estatística,
      ~ format(
        round(.x, 4),
        big.mark = ".",
        decimal.mark = ",",
        scientific = FALSE,
        nsmall = 2
      )
    )
  )


xtable(tabela_suc)

# boxplot TI_TX

ggplot(
  dados_conjunto,
  aes(
    x = POL_ABV,
    y = TI_TX,
    fill = ADOPT
  )
) +
  geom_boxplot(
    width = 0.55,
    position = position_dodge(width = 0.7),
    outlier.shape = 16,
    outlier.size = 2.5
  ) +
  
  stat_summary(
    aes(group = ADOPT),
    fun = mean,
    geom = "point",
    shape = 18,
    size = 4,
    color = "black",
    position = position_dodge(width = 0.7)
  ) +
  
  scale_fill_manual(
    values = c(
      "Adotou" = "#007EA7",
      "Não adotou" = "#9D0208"
    )
  ) +
  scale_y_continuous(
    labels = scales::label_percent()
  ) +
  labs(
    x = NULL,
    y = "Taxa de Investimento em TI",
    fill = "Adoção à política"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.text = element_text(size = 10),
    axis.text.x = element_text(color = "black"),
    axis.title.y = element_text(size = 13),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

# quadro de medidas TI_TX

tabela_ti <- dados_conjunto %>%
  group_by(POL_ABV, ADOPT) %>%
  summarise(
    Média = mean(TI_TX),
    `Desvio-padrão` = sd(TI_TX),
    Mínimo = min(TI_TX),
    `1º Quartil` = quantile(TI_TX, 0.25),
    Mediana = median(TI_TX),
    `3º Quartil` = quantile(TI_TX, 0.75),
    Máximo = max(TI_TX),
    .groups = "drop"
  )


tabela_ti <- tabela_ti %>%
  pivot_longer(
    cols = c(
      Média,
      `Desvio-padrão`,
      Mínimo,
      `1º Quartil`,
      Mediana,
      `3º Quartil`,
      Máximo
    ),
    names_to = "Estatística",
    values_to = "Valor"
  ) %>%
  mutate(
    Grupo = paste(POL_ABV, ADOPT, sep = "_")
  ) %>%
  select(Estatística, Grupo, Valor) %>%
  pivot_wider(
    names_from = Grupo,
    values_from = Valor
  )

tabela_ti <- tabela_ti %>%
  mutate(
    across(
      -Estatística,
      ~ format(
        round(.x, 4),
        big.mark = ".",
        decimal.mark = ",",
        scientific = FALSE,
        nsmall = 2
      )
    )
  )


xtable(tabela_ti)


# boxplot PIB

ggplot(
  dados_conjunto,
  aes(
    x = POL_ABV,
    y = PIB,
    fill = ADOPT
  )
) +
  geom_boxplot(
    width = 0.55,
    position = position_dodge(width = 0.7),
    outlier.shape = 16,
    outlier.size = 2.5
  ) +
  
  stat_summary(
    aes(group = ADOPT),
    fun = mean,
    geom = "point",
    shape = 18,
    size = 4,
    color = "black",
    position = position_dodge(width = 0.7)
  ) +
  
  scale_fill_manual(
    values = c(
      "Adotou" = "#007EA7",
      "Não adotou" = "#9D0208"
    )
  ) +
  
  labs(
    x = NULL,
    y = "PIB per capita",
    fill = "Adoção à política"
  ) +
  scale_y_continuous(
    labels = function(x) {
      ifelse(
        x == 0,
        "0",
        paste0(
          format(round(x / 1000, 1), decimal.mark = ",", trim = TRUE),
          " mil"
        )
      )
    }
  )+
  
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.text = element_text(size = 10),
    axis.text.x = element_text(color = "black"),
    axis.title.y = element_text(size = 13),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )


# quadro de medidas PIB

tabela_pib <- dados_conjunto %>%
  group_by(POL_ABV, ADOPT) %>%
  summarise(
    Média = mean(PIB),
    `Desvio-padrão` = sd(PIB),
    Mínimo = min(PIB),
    `1º Quartil` = quantile(PIB, 0.25),
    Mediana = median(PIB),
    `3º Quartil` = quantile(PIB, 0.75),
    Máximo = max(PIB),
    .groups = "drop"
  )


tabela_pib <- tabela_pib %>%
  pivot_longer(
    cols = c(
      Média,
      `Desvio-padrão`,
      Mínimo,
      `1º Quartil`,
      Mediana,
      `3º Quartil`,
      Máximo
    ),
    names_to = "Estatística",
    values_to = "Valor"
  ) %>%
  mutate(
    Grupo = paste(POL_ABV, ADOPT, sep = "_")
  ) %>%
  select(Estatística, Grupo, Valor) %>%
  pivot_wider(
    names_from = Grupo,
    values_from = Valor
  )

tabela_pib <- tabela_pib %>%
  mutate(
    across(
      -Estatística,
      ~ format(
        round(.x, 0),
        big.mark = ".",
        decimal.mark = ",",
        scientific = FALSE
      )
    )
  )


xtable(tabela_pib)


# km ANO_ELEITORAL_MUNIC

km_grupo <- survfit(
  Surv(t, ADOPT) ~ POL_ABV + ANO_ELEITORAL_MUNIC,
  data = dados_conjunto2,
  conf.int = FALSE
)

km_data <- data.frame(
  time = km_grupo$time,
  surv = km_grupo$surv,
  strata = rep(
    names(km_grupo$strata),
    km_grupo$strata
  )
)


km_data$eleicao <- ifelse(
  grepl("ANO_ELEITORAL_MUNIC=0", km_data$strata),
  "Ano sem eleição",
  "Ano com eleição"
)

km_data$politica <- ifelse(
  grepl("PNACAC", km_data$strata),
  "PNACAC",
  "PNAACTO"
)

km_data$politica <- factor(
  km_data$politica,
  levels = c("PNACAC", "PNAACTO")
)

km_eleitoral <- ggplot(
  km_data,
  aes(
    x = time,
    y = surv,
    color = eleicao,
    linetype = eleicao
  )
) +
  geom_step(linewidth = 1) +
  
  facet_wrap(~ politica, nrow = 1) +
  
  scale_color_manual(
    values = c(
      "Ano sem eleição" = "#007EA7",
      "Ano com eleição" = "#007EA7"
    )
  ) +
  
  scale_linetype_manual(
    values = c(
      "Ano sem eleição" = "solid",
      "Ano com eleição" = 11
    )
  ) +
  
  scale_x_continuous(
    limits = c(0, max(km_data$time)),
    breaks = seq(
      0,
      max(km_data$time),
      by = 2000
    ),
    expand = c(0, 0)
  ) +
  scale_y_continuous(labels = 
      number_format(decimal.mark = ",", big.mark = ".")) +
  
  labs(
    x = "Tempo",
    y = "S(t)",
    color = "Eleição Municipal",
    linetype = "Eleição Municipal"
  )+
  
  theme_minimal() +
  
  theme(
    strip.text = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title.y = element_text(size = 13),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )


# boxplot CARREIRAS

ggplot(
  dados_conjunto,
  aes(
    x = POL_ABV,
    y = CARREIRAS,
    fill = ADOPT
  )
) +
  geom_boxplot(
    width = 0.55,
    position = position_dodge(width = 0.7),
    outlier.shape = 16,
    outlier.size = 2.5
  ) +
  
  stat_summary(
    aes(group = ADOPT),
    fun = mean,
    geom = "point",
    shape = 18,
    size = 4,
    color = "black",
    position = position_dodge(width = 0.7)
  ) +
  
  scale_fill_manual(
    values = c(
      "Adotou" = "#007EA7",
      "Não adotou" = "#9D0208"
    )
  ) +
  scale_y_continuous(
    labels = number_format(decimal.mark = ",", big.mark = ".")
  ) +
  
  labs(
    x = NULL,
    y = "Pontuação das carreiras",
    fill = "Adoção à política"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.text = element_text(size = 10),
    axis.text.x = element_text(color = "black"),
    axis.title.y = element_text(size = 13),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

# quadro de medidas CARREIRAS

tabela_carreiras <- dados_conjunto %>%
  group_by(POL_ABV, ADOPT) %>%
  summarise(
    Média = mean(CARREIRAS),
    `Desvio-padrão` = sd(CARREIRAS),
    Mínimo = min(CARREIRAS),
    `1º Quartil` = quantile(CARREIRAS, 0.25),
    Mediana = median(CARREIRAS),
    `3º Quartil` = quantile(CARREIRAS, 0.75),
    Máximo = max(CARREIRAS),
    .groups = "drop"
  )


tabela_carreiras <- tabela_carreiras %>%
  pivot_longer(
    cols = c(
      Média,
      `Desvio-padrão`,
      Mínimo,
      `1º Quartil`,
      Mediana,
      `3º Quartil`,
      Máximo
    ),
    names_to = "Estatística",
    values_to = "Valor"
  ) %>%
  mutate(
    Grupo = paste(POL_ABV, ADOPT, sep = "_")
  ) %>%
  select(Estatística, Grupo, Valor) %>%
  pivot_wider(
    names_from = Grupo,
    values_from = Valor
  )

tabela_carreiras <- tabela_carreiras %>%
  mutate(
    across(
      -Estatística,
      ~ format(
        round(.x, 2),
        big.mark = ".",
        decimal.mark = ",",
        scientific = FALSE,
        nsmall = 2
      )
    )
  )

xtable(tabela_carreiras)

# boxplot IDEOL_PRF

ggplot(
  dados_conjunto,
  aes(
    x = POL_ABV,
    y = IDEOL_PRF,
    fill = ADOPT
  )
) +
  geom_boxplot(
    width = 0.55,
    position = position_dodge(width = 0.7),
    outlier.shape = 16,
    outlier.size = 2.5
  ) +
  
  stat_summary(
    aes(group = ADOPT),
    fun = mean,
    geom = "point",
    shape = 18,
    size = 4,
    color = "black",
    position = position_dodge(width = 0.7)
  ) +
  
  scale_fill_manual(
    values = c(
      "Adotou" = "#007EA7",
      "Não adotou" = "#9D0208"
    )
  ) +
  scale_y_continuous(
    limits = c(-0.8, 0.8),
    breaks = seq(-0.8, 0.8, by = 0.2),
    labels = c(
      "-0,8", "",
      "-0,4", "",
      "0,0", "",
      "0,4", "",
      "0,8"
    )
  ) +
  
  labs(
    x = NULL,
    y = "Ideologia do prefeito",
    fill = "Adoção à política"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.text = element_text(size = 10),
    axis.text.x = element_text(color = "black"),
    axis.title.y = element_text(size = 13),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )


# quadro de medidas IDEOL_PRF

tabela_prf <- dados_conjunto %>%
  group_by(POL_ABV, ADOPT) %>%
  summarise(
    Média = mean(IDEOL_PRF),
    `Desvio-padrão` = sd(IDEOL_PRF),
    Mínimo = min(IDEOL_PRF),
    `1º Quartil` = quantile(IDEOL_PRF, 0.25),
    Mediana = median(IDEOL_PRF),
    `3º Quartil` = quantile(IDEOL_PRF, 0.75),
    Máximo = max(IDEOL_PRF),
    .groups = "drop"
  )


tabela_prf <- tabela_prf %>%
  pivot_longer(
    cols = c(
      Média,
      `Desvio-padrão`,
      Mínimo,
      `1º Quartil`,
      Mediana,
      `3º Quartil`,
      Máximo
    ),
    names_to = "Estatística",
    values_to = "Valor"
  ) %>%
  mutate(
    Grupo = paste(POL_ABV, ADOPT, sep = "_")
  ) %>%
  select(Estatística, Grupo, Valor) %>%
  pivot_wider(
    names_from = Grupo,
    values_from = Valor
  )

tabela_prf <- tabela_prf %>%
  mutate(
    across(
      -Estatística,
      ~ format(
        round(.x, 2),
        big.mark = ".",
        decimal.mark = ",",
        scientific = FALSE,
        nsmall = 2
      )
    )
  )


xtable(tabela_prf)


# tempo 

km <- survfit(Surv(dados$t, dados$ADOPT) ~ dados$POL_ABV, conf.int = F)

km_data <- data.frame(
  time = km$time,
  surv = km$surv,
  strata = rep(
    names(km$strata),
    times = km$strata
  )
)

ggplot(km_data, aes(x = time, y = surv, color = strata)) +
  geom_step(linewidth = 0.8) +
  scale_color_manual(
    name = NULL,
    breaks = c(
      "dados$POL_ABV=pnaacto",
      "dados$POL_ABV=pnacac"
    ),
    labels = c("PNAACTO", "PNACAC"),
    values = c(
      "dados$POL_ABV=pnaacto" = "#eccb30",
      "dados$POL_ABV=pnacac" = "#2c6552"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    labels = number_format(decimal.mark = ",", big.mark = ".")
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



# boxplot TX_PARTC

ggplot(
  dados_conjunto,
  aes(
    x = POL_ABV,
    y = TX_PARTC,
    fill = ADOPT
  )
) +
  geom_boxplot(
    width = 0.55,
    position = position_dodge(width = 0.7),
    outlier.shape = 16,
    outlier.size = 2.5
  ) +
  
  stat_summary(
    aes(group = ADOPT),
    fun = mean,
    geom = "point",
    shape = 18,
    size = 4,
    color = "black",
    position = position_dodge(width = 0.7)
  ) +
  scale_y_continuous(
    breaks = seq(0.4,1.0, by=0.2),
    limits = c(0.4,1.0),
    labels = scales::label_percent()
  ) +
  scale_fill_manual(
    values = c(
      "Adotou" = "#007EA7",
      "Não adotou" = "#9D0208"
    )
  ) +
  
  labs(
    x = NULL,
    y = "Taxa de Participação",
    fill = "Adoção à política"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.text = element_text(size = 10),
    axis.text.x = element_text(color = "black"),
    axis.title.y = element_text(size = 13),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11))


# quadro de medidas TX_PARTC

tabela_part <- dados_conjunto %>%
  group_by(POL_ABV, ADOPT) %>%
  summarise(
    Média = mean(TX_PARTC),
    `Desvio-padrão` = sd(TX_PARTC),
    Mínimo = min(TX_PARTC),
    `1º Quartil` = quantile(TX_PARTC, 0.25),
    Mediana = median(TX_PARTC),
    `3º Quartil` = quantile(TX_PARTC, 0.75),
    Máximo = max(TX_PARTC),
    .groups = "drop"
  )

tabela_part  <- tabela_part  %>%
  pivot_longer(
    cols = c(
      Média,
      `Desvio-padrão`,
      Mínimo,
      `1º Quartil`,
      Mediana,
      `3º Quartil`,
      Máximo
    ),
    names_to = "Estatística",
    values_to = "Valor"
  ) %>%
  mutate(
    Grupo = paste(POL_ABV, ADOPT, sep = "_")
  ) %>%
  select(Estatística, Grupo, Valor) %>%
  pivot_wider(
    names_from = Grupo,
    values_from = Valor
  )

tabela_part  <- tabela_part  %>%
  mutate(
    across(
      -Estatística,
      ~ format(
        round(.x, 4),
        big.mark = ".",
        decimal.mark = ",",
        scientific = FALSE,
        nsmall = 2
      )
    )
  )


xtable(tabela_part)

# boxplot tx de disciplina da base

ggplot(
  dados_conjunto,
  aes(
    x = POL_ABV,
    y = TX_DISCIP_BASE,
    fill = ADOPT
  )
) +
  geom_boxplot(
    width = 0.55,
    position = position_dodge(width = 0.7),
    outlier.shape = 16,
    outlier.size = 2.5
  ) +
  
  stat_summary(
    aes(group = ADOPT),
    fun = mean,
    geom = "point",
    shape = 18,
    size = 4,
    color = "black",
    position = position_dodge(width = 0.7)
  ) +
  scale_y_continuous(
    labels = scales::label_percent()
  ) +
  scale_fill_manual(
    values = c(
      "Adotou" = "#007EA7",
      "Não adotou" = "#9D0208"
    )
  ) +
  
  labs(
    x = NULL,
    y = "Taxa de Disciplina",
    fill = "Adoção à política"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.text = element_text(size = 10),
    axis.text.x = element_text(color = "black"),
    axis.title.y = element_text(size = 13),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11))


# quadro de medidas tx de disciplina na base

tabela_disc <- dados_conjunto %>%
  group_by(POL_ABV, ADOPT) %>%
  summarise(
    Média = mean(TX_DISCIP_BASE),
    `Desvio-padrão` = sd(TX_DISCIP_BASE),
    Mínimo = min(TX_DISCIP_BASE),
    `1º Quartil` = quantile(TX_DISCIP_BASE, 0.25),
    Mediana = median(TX_DISCIP_BASE),
    `3º Quartil` = quantile(TX_DISCIP_BASE, 0.75),
    Máximo = max(TX_DISCIP_BASE),
    .groups = "drop"
  )

tabela_disc  <- tabela_disc  %>%
  pivot_longer(
    cols = c(
      Média,
      `Desvio-padrão`,
      Mínimo,
      `1º Quartil`,
      Mediana,
      `3º Quartil`,
      Máximo
    ),
    names_to = "Estatística",
    values_to = "Valor"
  ) %>%
  mutate(
    Grupo = paste(POL_ABV, ADOPT, sep = "_")
  ) %>%
  select(Estatística, Grupo, Valor) %>%
  pivot_wider(
    names_from = Grupo,
    values_from = Valor
  )

tabela_disc  <- tabela_disc  %>%
  mutate(
    across(
      -Estatística,
      ~ format(
        round(.x, 4),
        big.mark = ".",
        decimal.mark = ",",
        scientific = FALSE,
        nsmall = 2
      )
    )
  )


xtable(tabela_disc)

# boxplot tx frag part

ggplot(
  dados_conjunto,
  aes(
    x = POL_ABV,
    y = TX_FRAG_PARTD_NEP,
    fill = ADOPT
  )
) +
  geom_boxplot(
    width = 0.55,
    position = position_dodge(width = 0.7),
    outlier.shape = 16,
    outlier.size = 2.5
  ) +
  
  stat_summary(
    aes(group = ADOPT),
    fun = mean,
    geom = "point",
    shape = 18,
    size = 4,
    color = "black",
    position = position_dodge(width = 0.7)
  ) +
  scale_fill_manual(
    values = c(
      "Adotou" = "#007EA7",
      "Não adotou" = "#9D0208"
    )
  ) +
  scale_y_continuous(labels = number_format(decimal.mark = ",", big.mark = ".")
                     )+
  labs(
    x = NULL,
    y = "Fragmentação Partidária",
    fill = "Adoção à política"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.text = element_text(size = 10),
    axis.text.x = element_text(color = "black"),
    axis.title.y = element_text(size = 13),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11))


# quadro de medidas

tabela_partido <- dados_conjunto %>%
  group_by(POL_ABV, ADOPT) %>%
  summarise(
    Média = mean(TX_FRAG_PARTD_NEP),
    `Desvio-padrão` = sd(TX_FRAG_PARTD_NEP),
    Mínimo = min(TX_FRAG_PARTD_NEP),
    `1º Quartil` = quantile(TX_FRAG_PARTD_NEP, 0.25),
    Mediana = median(TX_FRAG_PARTD_NEP),
    `3º Quartil` = quantile(TX_FRAG_PARTD_NEP, 0.75),
    Máximo = max(TX_FRAG_PARTD_NEP),
    .groups = "drop"
  )

tabela_partido  <- tabela_partido %>%
  pivot_longer(
    cols = c(
      Média,
      `Desvio-padrão`,
      Mínimo,
      `1º Quartil`,
      Mediana,
      `3º Quartil`,
      Máximo
    ),
    names_to = "Estatística",
    values_to = "Valor"
  ) %>%
  mutate(
    Grupo = paste(POL_ABV, ADOPT, sep = "_")
  ) %>%
  select(Estatística, Grupo, Valor) %>%
  pivot_wider(
    names_from = Grupo,
    values_from = Valor
  )

tabela_partido  <- tabela_partido %>%
  mutate(
    across(
      -Estatística,
      ~ format(
        round(.x, 2),
        big.mark = ".",
        decimal.mark = ",",
        scientific = FALSE,
        nsmall = 2
      )
    )
  )


xtable(tabela_partido)


# boxplot ORC_TOTAL_TX

ggplot(
  dados_conjunto,
  aes(
    x = POL_ABV,
    y = ORC_TOTAL_TX,
    fill = ADOPT
  )
) +
  geom_boxplot(
    width = 0.55,
    position = position_dodge(width = 0.7),
    outlier.shape = 16,
    outlier.size = 2.5
  ) +
  
  stat_summary(
    aes(group = ADOPT),
    fun = mean,
    geom = "point",
    shape = 18,
    size = 4,
    color = "black",
    position = position_dodge(width = 0.7)
  ) +
  
  scale_fill_manual(
    values = c(
      "Adotou" = "#007EA7",
      "Não adotou" = "#9D0208"
    )
  ) +
  labs(
    x = NULL,
    y = "Taxa de Orçamento",
    fill = "Adoção à política"
  ) +
  scale_y_continuous(
    labels = scales::label_percent()
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.text = element_text(size = 10),
    axis.text.x = element_text(color = "black"),
    axis.title.y = element_text(size = 13),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )


# quadro de medidas ORC_TOTAL_TX

tabela_orc <- dados_conjunto %>%
  group_by(POL_ABV, ADOPT) %>%
  summarise(
    Média = mean(ORC_TOTAL_TX),
    `Desvio-padrão` = sd(ORC_TOTAL_TX),
    Mínimo = min(ORC_TOTAL_TX),
    `1º Quartil` = quantile(ORC_TOTAL_TX, 0.25),
    Mediana = median(ORC_TOTAL_TX),
    `3º Quartil` = quantile(ORC_TOTAL_TX, 0.75),
    Máximo = max(ORC_TOTAL_TX),
    .groups = "drop"
  )

tabela_orc <- tabela_orc %>%
  pivot_longer(
    cols = c(
      Média,
      `Desvio-padrão`,
      Mínimo,
      `1º Quartil`,
      Mediana,
      `3º Quartil`,
      Máximo
    ),
    names_to = "Estatística",
    values_to = "Valor"
  ) %>%
  mutate(
    Grupo = paste(POL_ABV, ADOPT, sep = "_")
  ) %>%
  select(Estatística, Grupo, Valor) %>%
  pivot_wider(
    names_from = Grupo,
    values_from = Valor
  )

tabela_orc <- tabela_orc%>%
  mutate(
    across(
      -Estatística,
      ~ format(
        round(.x, 4),
        big.mark = ".",
        decimal.mark = ",",
        scientific = FALSE,
        nsmall = 2
      )
    )
  )


xtable(tabela_orc)



# km ANO_ELEITORAL_NAC

km_grupo <- survfit(
  Surv(t, ADOPT) ~ POL_ABV + ANO_ELEITORAL_NAC,
  data = dados_conjunto2,
  conf.int = FALSE
)

km_data <- data.frame(
  time = km_grupo$time,
  surv = km_grupo$surv,
  strata = rep(
    names(km_grupo$strata),
    km_grupo$strata
  )
)


km_data$eleicao <- ifelse(
  grepl("ANO_ELEITORAL_NAC=0", km_data$strata),
  "Ano sem eleição",
  "Ano com eleição"
)

km_data$politica <- ifelse(
  grepl("PNACAC", km_data$strata),
  "PNACAC",
  "PNAACTO"
)

km_data$politica <- factor(
  km_data$politica,
  levels = c("PNACAC", "PNAACTO")
)

km_eleitoral <-ggplot(
  km_data,
  aes(
    x = time,
    y = surv,
    color = eleicao,
    linetype = eleicao
  )
) +
  geom_step(linewidth = 1) +
  
  facet_wrap(~ politica, nrow = 1) +
  
  scale_color_manual(
    values = c(
      "Ano sem eleição" = "#007EA7",
      "Ano com eleição" = "#007EA7"
    )
  ) +
  
  scale_linetype_manual(
    values = c(
      "Ano sem eleição" = "solid",
      "Ano com eleição" = 11
    )
  ) +
  
  scale_x_continuous(
    limits = c(0, max(km_data$time)),
    breaks = seq(
      0,
      max(km_data$time),
      by = 2000
    ),
    expand = c(0, 0)
  ) +
  scale_y_continuous(labels = 
                       number_format(decimal.mark = ",", big.mark = ".")) +
  
  labs(
    x = "Tempo",
    y = "S(t)",
    color = "Eleição Nacional",
    linetype = "Eleição Nacional"
  )+
  
  theme_minimal() +
  
  theme(
    strip.text = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title.y = element_text(size = 13),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave("km_eleitoral_nac.pdf",plot= km_eleitoral, width = 16.53,height = 10.82,units ="cm")

