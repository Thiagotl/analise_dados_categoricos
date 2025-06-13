library(dplyr)
library(ggplot2)
library(tidyr)


dados<-data.frame(
  Sexo = c(0, 0, 1, 1),
  ECG  = c(0, 1, 0, 1),
  DoencaPresente = c(4, 8, 9, 21),
  DoencaAusente = c(11, 10, 9, 6)
)


  
dados_long <- dados |>
  tidyr::uncount(weights = DoencaPresente, .id = "id_presente") |>
  mutate(Doenca = 1) |>
  bind_rows(
    dados |>
      tidyr::uncount(weights = DoencaAusente, .id = "id_ausente") |>
      mutate(Doenca = 0)
  ) |>
  select(Sexo, ECG, Doenca)

table(dados_long$Sexo, dados_long$ECG, dados_long$Doenca)

modelo <- glm(Doenca ~ Sexo + ECG, data = dados_long, family = binomial())

exp(coef(modelo))

predict(modelo, newdata = data.frame(Sexo=1, ECG=1), type = "response")

predict(modelo, newdata = data.frame(Sexo=0, ECG=0), type = "response")

# dados_plot <- expand.grid(
#   Sexo = c(0, 1),
#   ECG = c(0, 1)
# ) |> 
#   mutate(
#     Prob_Doenca = predict(modelo, newdata = data.frame(Sexo=0, ECG=0), type = "response"),
#     Sexo = factor(Sexo, levels = c(0,1), labels = c("Feminino", "Masculino")),
#     ECG = factor(ECG, levels = c(0,1), labels = c("< 0,1 ST", ">= 0,1 ST"))
#   )
# 
# # Gráfico de barras com probabilidades previstas
# ggplot(dados_plot, aes(x = ECG, y = Prob_Doenca, fill = Sexo)) +
#   geom_bar(stat = "identity", position = position_dodge()) +
#   labs(
#     title = "Probabilidade prevista de doença coronária",
#     x = "Resultado do ECG",
#     y = "Probabilidade",
#     fill = "Sexo"
#   ) +
#   ylim(0, 1) +
#   theme_minimal(base_size = 14)






dados1 <- data.frame(
  Diagnostico = c("Complicada", "Complicada", "Complicada", 
                  "NaoComplicada", "NaoComplicada", "NaoComplicada"),
  Tratamento = c("A", "B", "C", "A", "B", "C"),
  Cura_Sim = c(78, 101, 68, 40, 54, 34),
  Cura_Nao = c(28, 11, 46, 5, 5, 6)
)

# Expandindo os dados
dados_expandido <- 
  dados1 |> 
  uncount(weights = Cura_Sim) |> 
  mutate(Cura = 1) |> 
  bind_rows(
    dados1 |> 
      uncount(weights = Cura_Nao) |> 
      mutate(Cura = 0)
  )

# Criando variáveis dummy conforme o livro
dados_expandido <- dados_expandido |> 
  mutate(
    X1 = ifelse(Diagnostico == "Complicada", 1, 0),
    X21 = ifelse(Tratamento == "A", 1, 0),
    X22 = ifelse(Tratamento == "B", 1, 0)
  )

# Conferindo os dados
table(dados_expandido$X1, dados_expandido$X21, dados_expandido$X22, dados_expandido$Cura)

modelo <- glm(Cura ~ X1 + X21 + X22, data = dados_expandido, family = binomial())

# Resumo do modelo
summary(modelo)

exp(cbind(OddsRatio = coef(modelo), confint(modelo)))

predict(modelo, newdata = data.frame(X1=1, X21=1, X22=0), type="response")

predict(modelo, newdata = data.frame(X1=0, X21=0, X22=1), type="response")


combinacoes <- expand.grid(
  X1 = c(0, 1),           # 0 = não complicada, 1 = complicada
  X21 = c(0, 1),          # 1 = tratamento A
  X22 = c(0, 1)           # 1 = tratamento B
) |> 
  # Eliminando combinações inválidas (não pode ser A e B ao mesmo tempo)
  filter(!(X21 == 1 & X22 == 1)) |> 
  mutate(
    Diagnostico = ifelse(X1 == 1, "Complicada", "NaoComplicada"),
    Tratamento = case_when(
      X21 == 1 ~ "A",
      X22 == 1 ~ "B",
      X21 == 0 & X22 == 0 ~ "C"
    )
  )

# Adicionando as probabilidades previstas
combinacoes <- combinacoes |> 
  mutate(
    Prob_Cura = predict(modelo, newdata = ., type = "response")
  )

# Organizando o quadro
quadro_resultado <- combinacoes |> 
  select(Diagnostico, Tratamento, Prob_Cura) |> 
  arrange(Diagnostico, Tratamento)

# Exibir o quadro
print(quadro_resultado)




# Dados da tabela
dados <- data.frame(
  smk = c(0, 0, 0, 0, 1, 1, 1, 1),
  ses = c(0, 0, 1, 1, 0, 0, 1, 1),
  idade = c(0, 1, 0, 1, 0, 1, 0, 1),
  BRC_sim = c(38, 53, 48, 47, 73, 86, 82, 59),
  BRC_nao = c(73, 81, 56, 67, 61, 48, 55, 53)
)

# Expandindo os dados
dados_expandido <- 
  dados |> 
  uncount(weights = BRC_sim) |> 
  mutate(BRC = 1) |> 
  bind_rows(
    dados |> 
      uncount(weights = BRC_nao) |> 
      mutate(BRC = 0)
  ) |> 
  select(smk, ses, idade, BRC)

# Conferindo os dados
table(dados_expandido$smk, dados_expandido$ses, dados_expandido$idade, dados_expandido$BRC)


# Ajuste do modelo com todas as interações
modelo <- glm(BRC ~ smk * ses * idade, data = dados_expandido, family = binomial())

# Resumo dos coeficientes
summary(modelo)


exp(cbind(OddsRatio = coef(modelo), confint(modelo)))

# Modelo completo (com interação tripla)
modelo_completo <- glm(BRC ~ smk * ses * idade, data = dados_expandido, family = binomial())

# Modelo sem interação tripla
modelo_sem_tripla <- glm(BRC ~ smk * ses + smk * idade + ses * idade, data = dados_expandido, family = binomial())

# Comparar modelos
anova(modelo_sem_tripla, modelo_completo, test = "Chisq")

modelo_sem_ses_idade <- glm(BRC ~ smk * ses + smk * idade, data = dados_expandido, family = binomial())

anova(modelo_sem_ses_idade, modelo_sem_tripla, test = "Chisq")

modelo_sem_smk_idade <- glm(BRC ~ smk * ses + ses * idade, data = dados_expandido, family = binomial())

anova(modelo_sem_smk_idade, modelo_sem_tripla, test = "Chisq")


modelo_sem_smk_ses <- glm(BRC ~ smk * idade + ses * idade, data = dados_expandido, family = binomial())

anova(modelo_sem_smk_ses, modelo_sem_tripla, test = "Chisq")







