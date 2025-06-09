library(tidyverse)
library(readxl)
library(MASS)
library(rsq)


library(fmsb)
library(modEvA)
library(hnp)



dados_94_alunos <- read_excel("dados_94_alunos.xlsx",
                              sheet = "dados", na = "-")


# 0 masculino e 1 feminino


mediana_idade<-median(dados_94_alunos$idade)
mediana_altura<-median(dados_94_alunos$altura)
mediana_peso<-median(dados_94_alunos$peso)

attach(dados_94_alunos)

dados_94_alunos<-dados_94_alunos |>
  mutate(dummy_idade = ifelse(idade<=mediana_idade,1,0),
         dummy_altura = ifelse(altura<=mediana_altura,1,0),
         dummy_peso = ifelse(peso<=mediana_peso,1,0))

modelo<-glm(sexo ~ dummy_idade+dummy_altura+dummy_peso, family = "binomial", data = dados_94_alunos)

summary(modelo)

modelo2<-glm(sexo ~ dummy_idade*dummy_altura+dummy_peso, family = "binomial", data = dados_94_alunos)

summary(modelo2)

modelo3<-glm(sexo ~ dummy_idade+dummy_altura*dummy_peso, family = "binomial", data = dados_94_alunos)
summary(modelo3)
