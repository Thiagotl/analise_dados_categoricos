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

funcao_regressao<-function(df, var_resposta="turma", var_explicativas=c("sexo","dummy_idade", "dummy_altura", "dummy_peso")){
  
  library(VGAM)
  
  mediana_idade<-median(df$idade)
  mediana_altura<-median(df$altura)
  mediana_peso<-median(df$peso)
  
  
  df<-df |> 
    dplyr::mutate(dummy_idade = ifelse(idade>=mediana_idade,1,0), # 1(velho)
                   dummy_altura = ifelse(altura>=mediana_altura,1,0), #1(alto)
                   dummy_peso = ifelse(peso>=mediana_peso,1,0))# 1(pesado)
  
  
  df[[var_resposta]]<-relevel(as.factor(df[[var_resposta]]), ref="C")
  
  resultados<-list()
  
  extract_coefs <- function(model) {
    coefs <- coef(summary(model))
    as.data.frame(coefs) |> 
      tibble::rownames_to_column("term") |> 
      rename(estimate = Estimate, 
             std.error = "Std. Error",
             statistic = "z value",
             p.value = "Pr(>|z|)")
}
  
  
  #1 - MODELO NULO
  
  modelo_nulo <- vglm(as.formula(paste(var_resposta, "~ 1")),
                      family = multinomial(), data = df)
  
    resultados$nulo <- list(
    deviance = deviance(modelo_nulo),
    aic = AIC(modelo_nulo),
    coefficients = extract_coefs(modelo_nulo)
  )
  
  
  #2 - MODELO COM ADICAO DAS COVARIAVEIS
  
  
  # ESSE AQUI EU PREVISO ARRUMAR PRA DEIXAR COMO RESOLUÇÃO DA ATIVIDADE 1  
  # resultados$var_individuais<-list()
  # 
  # 
  # for (var in var_explicativas) {
  #   formula <- as.formula(paste(var_resposta, "~", var))
  #   modelo <- vglm(formula, family = multinomial(), data = df)
  #   
  #   resultados$var_individuais[[var]]<-list(
  #       deviance = deviance(modelo),
  #       aic = AIC(modelo),
  #       coefficients = extract_coefs(modelo)
  #     )
  #   
  # }
  
    
    
  #2 - MODELO COM ADICAO DAS COVARIAVEIS  
  
  resultados$modelo_acumulativo<-list()
  var_acumulativas<-c()
  
  for (i in vector) {
    
  }
    
      
  #3 - MODELO COM TODOS EFEITOS
  
  formula_completo <- as.formula(paste(var_resposta, "~", 
                                       paste(var_explicativas, collapse = " + ")))
  modelo_completo <- vglm(formula_completo, family = multinomial(), data = df)
  
  resultados$completo <- list(
    deviance = deviance(modelo_completo),
    aic = AIC(modelo_completo),
    coefficients = extract_coefs(modelo_completo)
  )
  
  #5 - MODELO COM INTERACOES INDIVIDUAIS
  
  
  resultados$interacoes <- list()
  
  # Criar combinações de interações
  pares_interacao <- combn(var_explicativas, 2, simplify = FALSE)
  
  for (par in pares_interacao) {
    termo_interacao <- paste(par, collapse = ":")
    formula_int <- as.formula(paste(var_resposta, "~", 
                                    paste(var_explicativas, collapse = " + "),
                                    "+", termo_interacao))
    
    modelo_int <- tryCatch({
      vglm(formula_int, family = multinomial(), data = df)
    }, error = function(e) NULL)
    
    if (!is.null(modelo_int)) {
      resultados$interacoes[[termo_interacao]] <- list(
        deviance = deviance(modelo_int),
        aic = AIC(modelo_int),
        coefficients = extract_coefs(modelo_int)
      )
    }
  }
  

  return(resultados)
  
}

teste<-funcao_regressao(dados_94_alunos)
teste$var_individuais

