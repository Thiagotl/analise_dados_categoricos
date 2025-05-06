(tabela <-array(c(20, 9, 382, 214, 10, 7, 172, 120, 12, 6, 327, 183),
                dim= c(2, 2, 3),
                dimnames= list(
                  Particulas = c("Alto", "Baixo"),
                  Bronquite = c("Sim", "Não"),
                  Idade = c("15-24", "23-39", "40+"))))
ftable(tabela, col.vars= "Bronquite",
       row.vars= c("Idade","Particulas"))





# Dados da tabela
dados <- matrix(c(
  15, 15, 31, 17, 42, 49, 38, 27,     # Tiro ou explosão
  95,113, 94,125,124,126,148,127,     # Esfaqueamento
  23, 16, 34, 34, 35, 33, 41, 41,     # Instrumento cortante
  9,  4,  8,  3,  5,  3,  1,  4,     # Veneno
  47, 60, 54, 70, 69, 66, 70, 60,     # Violência manual
  43, 45, 43, 53, 51, 63, 47, 51,     # Estrangulamento
  26, 16, 20, 24, 15, 15, 15, 15      # Asfixia ou afogamento
), nrow = 7, byrow = TRUE)

# Nomear linhas e colunas
rownames(dados) <- c("Tiro ou explosão", "Esfaqueamento", "Instrumento cortante",
                     "Veneno", "Violência manual", "Estrangulamento", "Asfixia ou afogamento")
colnames(dados) <- c("1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977")

# Visualizar tabela
print(dados)



  # caso não tenha instalado ainda
library(ca)



resultado_ac <- ca(dados)

# Plotar os resultados
plot(resultado_ac)



# ----------------


# Criar o array com os dados
tabela <- array(c(20, 9, 382, 214, 
                  10, 7, 172, 120, 
                  12, 6, 327, 183),
                dim = c(2, 2, 3),
                dimnames = list(
                  Particulas = c("Alto", "Baixo"),
                  Bronquite = c("Sim", "Não"),
                  Idade = c("15-24", "25-39", "40+")))

# Visualizar a tabela
ftable(tabela, col.vars = "Bronquite", row.vars = c("Idade", "Particulas"))


mantelhaen.test(tabela)

















# --------------------

# 
# 
# library(FactoMineR)
# library(factoextra)
# 
# # Matriz de dados
# dados <- matrix(c(
#   15, 15, 31, 17, 42, 49, 38, 27,     # Tiro ou explosão
#   95,113, 94,125,124,126,148,127,     # Esfaqueamento
#   23, 16, 34, 34, 35, 33, 41, 41,     # Instrumento cortante
#   9,  4,  8,  3,  5,  3,  1,  4,     # Veneno
#   47, 60, 54, 70, 69, 66, 70, 60,     # Violência manual
#   43, 45, 43, 53, 51, 63, 47, 51,     # Estrangulamento
#   26, 16, 20, 24, 15, 15, 15, 15      # Asfixia ou afogamento
# ), nrow = 7, byrow = TRUE)
# 
# rownames(dados) <- c("Tiro ou explosão", "Esfaqueamento", "Instrumento cortante",
#                      "Veneno", "Violência manual", "Estrangulamento", "Asfixia ou afogamento")
# colnames(dados) <- c("1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977")
# 
# # Análise de correspondência com FactoMineR
# res.ca <- CA(dados, graph = FALSE)
# 
# fviz_ca_row(res.ca, repel = TRUE, col.row = "blue") +
#   ggtitle("Métodos de Assassinato - Análise de Correspondência")
# fviz_ca_col(res.ca, repel = TRUE, col.col = "red") +
#   ggtitle("Anos de Ocorrência - Análise de Correspondência")
# 
# fviz_ca_biplot(res.ca, repel = TRUE) +
#   ggtitle("Análise de Correspondência: Métodos vs Anos")
