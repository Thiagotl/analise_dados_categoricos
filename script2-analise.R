library(tidyverse)
library(readxl)
library(MASS)
library(rsq)


library(fmsb)
library(modEvA)
library(hnp)



dados_94_alunos <- read_excel("dados_94_alunos.xlsx",
                              sheet = "dados", na = "-")

# 
# # 0 masculino e 1 feminino
# 
# 
# mediana_idade<-median(dados_94_alunos$idade)
# mediana_altura<-median(dados_94_alunos$altura)
# mediana_peso<-median(dados_94_alunos$peso)
# 
# attach(dados_94_alunos)
# 
# dados_94_alunos$turma<-factor(dados_94_alunos$turma)
# 
# dados_94_alunos$turma<-relevel(dados_94_alunos$turma, ref="C")
# 
# 
# dados_94_alunos<-dados_94_alunos |> mutate(dummy_idade = ifelse(idade>=mediana_idade,1,0), # 1(velho)
#                                            dummy_altura = ifelse(altura>=mediana_altura,1,0), #1(alto)
#                                            dummy_peso = ifelse(peso>=mediana_peso,1,0))# 1(pesado)
# 
# 
# 
# library(VGAM)
# 
# 
# 
# # modelo 1 - apenar intercepto
# 
# modelo<-vglm(turma ~ 1, family = multinomial(),data=dados_94_alunos)
# 
# 
# # modelo 2 -
# 
# 
# step4vglm(modelo)

funcao_regressao <- function(df, var_resposta = "turma", var_explicativas = c("sexo", "dummy_idade", "dummy_altura", "dummy_peso")) {
  
  library(VGAM)
  library(dplyr)
  library(tibble)
  
  # Criar dummies
  mediana_idade <- median(df$idade)
  mediana_altura <- median(df$altura)
  mediana_peso <- median(df$peso)
  
  df <- df |> 
    mutate(
      dummy_idade = ifelse(idade >= mediana_idade, 1, 0),
      dummy_altura = ifelse(altura >= mediana_altura, 1, 0),
      dummy_peso = ifelse(peso >= mediana_peso, 1, 0)
    )
  
  # Transformar resposta em fator
  df[[var_resposta]] <- relevel(as.factor(df[[var_resposta]]), ref = "C")
  
  resultados <- list()
  
  extract_coefs <- function(model) {
    coefs <- coef(summary(model))
    as.data.frame(coefs) |> 
      rownames_to_column("term") |> 
      rename(
        estimate = Estimate, 
        std.error = "Std. Error",
        statistic = "z value",
        p.value = "Pr(>|z|)"
      )
  }
  
  # 1. Modelo nulo
  modelo_nulo <- vglm(as.formula(paste(var_resposta, "~ 1")), family = multinomial(), data = df)
  resultados$nulo <- list(
    deviance = deviance(modelo_nulo),
    aic = AIC(modelo_nulo),
    coefficients = extract_coefs(modelo_nulo),
    modelo = modelo_nulo
  )
  
  # 2. Modelos com variáveis individuais
  resultados$var_individuais <- list()
  for (var in var_explicativas) {
    formula <- as.formula(paste(var_resposta, "~", var))
    modelo <- vglm(formula, family = multinomial(), data = df)
    
    resultados$var_individuais[[var]] <- list(
      deviance = deviance(modelo),
      aic = AIC(modelo),
      coefficients = extract_coefs(modelo),
      modelo = modelo
    )
  }
  
  # 3. Modelo completo
  formula_completo <- as.formula(paste(var_resposta, "~", paste(var_explicativas, collapse = " + ")))
  modelo_completo <- vglm(formula_completo, family = multinomial(), data = df)
  resultados$completo <- list(
    deviance = deviance(modelo_completo),
    aic = AIC(modelo_completo),
    coefficients = extract_coefs(modelo_completo),
    modelo = modelo_completo
  )
  
  # 4. Modelo com todas as interações
  formula_interacoes <- as.formula(paste(var_resposta, "~ (", paste(var_explicativas, collapse = " + "), ")^2"))
  modelo_interacoes <- tryCatch({
    vglm(formula_interacoes, family = multinomial(), data = df)
  }, error = function(e) NULL)
  
  if (!is.null(modelo_interacoes)) {
    resultados$modelo_interacoes <- list(
      formula = format(formula_interacoes),
      deviance = deviance(modelo_interacoes),
      aic = AIC(modelo_interacoes),
      coefficients = extract_coefs(modelo_interacoes),
      modelo = modelo_interacoes
    )
  }
  
  # 5. Interações individuais
  resultados$interacoes_individuais <- list()
  pares_interacao <- combn(var_explicativas, 2, simplify = FALSE)
  for (par in pares_interacao) {
    termo_interacao <- paste(par, collapse = ":")
    formula_int <- as.formula(paste(var_resposta, "~", paste(var_explicativas, collapse = " + "), "+", termo_interacao))
    modelo_int <- tryCatch({
      vglm(formula_int, family = multinomial(), data = df)
    }, error = function(e) NULL)
    
    if (!is.null(modelo_int)) {
      resultados$interacoes_individuais[[termo_interacao]] <- list(
        deviance = deviance(modelo_int),
        aic = AIC(modelo_int),
        coefficients = extract_coefs(modelo_int),
        modelo = modelo_int
      )
    }
  }
  
  # 6. Incremental com interações
  resultados$modelo_incremental_interacoes <- list()
  interacoes <- combn(var_explicativas, 2, simplify = FALSE)
  formula_base <- paste(var_resposta, "~", paste(var_explicativas, collapse = " + "))
  modelo_base <- vglm(as.formula(formula_base), family = multinomial(), data = df)
  
  resultados$modelo_incremental_interacoes[["sem_interacoes"]] <- list(
    formula = formula_base,
    deviance = deviance(modelo_base),
    aic = AIC(modelo_base),
    coefficients = extract_coefs(modelo_base),
    modelo = modelo_base
  )
  
  for (i in seq_along(interacoes)) {
    interacoes_atuais <- sapply(interacoes[1:i], function(x) paste(x, collapse = ":"))
    formula_atual <- paste(formula_base, "+", paste(interacoes_atuais, collapse = " + "))
    modelo <- tryCatch({
      vglm(as.formula(formula_atual), family = multinomial(), data = df)
    }, error = function(e) NULL)
    
    if (!is.null(modelo)) {
      resultados$modelo_incremental_interacoes[[paste0("interacao_", i)]] <- list(
        formula = formula_atual,
        deviance = deviance(modelo),
        aic = AIC(modelo),
        coefficients = extract_coefs(modelo),
        modelo = modelo
      )
    }
  }
  
  return(resultados)
}

resultados<-funcao_regressao(dados_94_alunos)




get_min_model_info <- function(model_list) {
  model_list <- model_list[!sapply(model_list, is.null)]
  devs <- sapply(model_list, function(x) x$deviance)
  aics <- sapply(model_list, function(x) x$aic)
  
  min_dev_idx <- which.min(devs)
  min_aic_idx <- which.min(aics)
  
  tibble::tibble(
    modelo_deviance = names(devs)[min_dev_idx],
    menor_deviance = devs[min_dev_idx],
    modelo_aic = names(aics)[min_aic_idx],
    menor_aic = aics[min_aic_idx],
    modelo_obj_deviance = list(model_list[[names(devs)[min_dev_idx]]]$modelo),
    modelo_obj_aic = list(model_list[[names(aics)[min_aic_idx]]]$modelo)
  )
}


resumo_modelos <- tibble::tibble(
  modelo_tipo = c("Modelo 3: Completo", "Modelo 4: Todas Interações", "Modelo 5: Interações Individuais", "Modelo 6: Interações Incrementais"),
  
  modelo_deviance = c(
    "completo",
    if (!is.null(resultados$modelo_interacoes)) "modelo_interacoes" else NA,
    info_individuais$modelo_deviance,
    info_incrementais$modelo_deviance
  ),
  
  menor_deviance = c(
    resultados$completo$deviance,
    if (!is.null(resultados$modelo_interacoes)) resultados$modelo_interacoes$deviance else NA,
    info_individuais$menor_deviance,
    info_incrementais$menor_deviance
  ),
  
  modelo_aic = c(
    "completo",
    if (!is.null(resultados$modelo_interacoes)) "modelo_interacoes" else NA,
    info_individuais$modelo_aic,
    info_incrementais$modelo_aic
  ),
  
  menor_aic = c(
    resultados$completo$aic,
    if (!is.null(resultados$modelo_interacoes)) resultados$modelo_interacoes$aic else NA,
    info_individuais$menor_aic,
    info_incrementais$menor_aic
  )
)


print(resumo_modelos)

info_individuais <- get_min_model_info(resultados$interacoes_individuais)
info_incrementais <- get_min_model_info(resultados$modelo_incremental_interacoes)

summary(info_individuais$modelo_obj_deviance[[1]])
summary(info_incrementais$modelo_obj_aic[[1]])



