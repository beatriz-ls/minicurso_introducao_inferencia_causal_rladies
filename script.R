library(tidyverse)
library(knitr)
library(dagitty)
library(ggdag)
library(ggplot2)

# Construindo a base de dados
data <- c(
  rep(c(0,1,1), 81),
  rep(c(0,1,0), 6),
  rep(c(0,0,1), 234),
  rep(c(0,0,0), 36),
  rep(c(1,1,1), 192),
  rep(c(1,1,0), 71),
  rep(c(1,0,1), 55),
  rep(c(1,0,0), 25)
)

dt <- matrix(data, ncol = 3, byrow = TRUE) %>%
  as_tibble() %>%
  setNames(c("Z", "T", "C"))

# Contingência
tbl <- dt %>%
  count(Z, T, C) %>%
  pivot_wider(names_from = C, values_from = n, values_fill = 0) %>%
  rename("Não Curou (C=0)" = `0`, "Curou (C=1)" = `1`)

# Exibindo como tabela
kable(tbl, caption = "Frequência conjunta das variáveis $Z$, $T$ e $C$")



# Simulação de Confundimento em Estudo Observacional
n <- 1000 
dados_observacionais <- tibble(
  # Variável de confusão (ex: gravidade da doença)
  gravidade_doenca = rbinom(n, 1, 0.5),
  
  # Probabilidade de receber tratamento depende da gravidade
  prob_tratamento = case_when(
    gravidade_doenca == 1 ~ 0.75,  # Casos graves: 75% chance de tratamento
    gravidade_doenca == 0 ~ 0.25   # Casos leves: 25% chance de tratamento
  ),
  
  # Tratamento atribuído (não randomizado)
  recebeu_tratamento = rbinom(n, 1, prob_tratamento),
  
  # Desfecho: afetado pela gravidade + ruído aleatório
  resultado_saude = gravidade_doenca + rnorm(n)
) |> select(-prob_tratamento)

dados_observacionais

model1 <-lm(resultado_saude ~ recebeu_tratamento, data = dados_observacionais)
model1$coef

model2 <- lm(resultado_saude ~ recebeu_tratamento + gravidade_doenca, data = dados_observacionais)
model2$coef

dados_observacionais |>
  group_by(recebeu_tratamento) |>
  summarise(media_resultado = mean(resultado_saude))

dados_observacionais |>
  group_by(recebeu_tratamento) |>
  summarise(media_resultado = mean(resultado_saude)) |>
  pivot_wider(
    names_from = recebeu_tratamento, 
    values_from = media_resultado,  
    names_prefix = "tratamento_"
  ) |>
  summarise(diferenca_media = tratamento_1 - tratamento_0)

# Construindo o grafo de cadeia
grafo <- dagitty("dag {
V1 -> V2
V2 -> V3
}")
# Exibir a figura do grafo
ggdag(grafo, layout = "circle") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  xlab("") + ylab("")

# Construindo o grafo confundidor
grafo <- dagitty("dag {
V1 -> V2
V1 -> V3
}")
# Exibir a figura do grafo
ggdag(grafo, layout = "circle") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  xlab("") + ylab("")

# Construindo o grafo colisor
grafo <- dagitty("dag {
V1 -> V2
V3 -> V2
}")
# Exibir a figura do grafo
ggdag(grafo, layout = "circle") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  xlab("") + ylab("")

#Construindo o grafo com D-separação

# Construindo o Grafo
grafo_exemplo <-dagitty('dag {
    V1 -> V2 <- V4
    V2 -> V3 <- V4
}')

# Visualizando
ggdag(grafo_exemplo) + theme_dag()

#D-separação: Exemplo no R 

library(dagitty)

# PERGUNTA 1: V1 e V3 são d-separados marginalmente?
dseparated(grafo_exemplo, "V1", "V3")

# PERGUNTA 2: V1 e V3 são d-separados dado V2?
dseparated(grafo_exemplo, "V1", "V3", "V2")

# PERGUNTA 3: V1 e V3 são d-separados dado V2 e V4?
dseparated(grafo_exemplo, "V1", "V3", c("V2", "V4"))

# Isso mostra TODAS as relações de independência que o grafo implica
impliedConditionalIndependencies(grafo_exemplo)

#Paradoxo de Simpson: Cenário 1 (Z = Sexo)

library(dagitty)
library(ggdag)
library(ggplot2)

# Construindo o Grafo
grafo <- dagitty("dag {
T -> C
Z -> C
Z -> T
}")
# Exibir a figura do grafo
ggdag(grafo, layout = "circle") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  xlab("") + ylab("")

#Paradoxo de Simpson: Cenário 2 (Z = Pressão alta)

library(dagitty)
library(ggdag)
library(ggplot2)

# Construindo o Grafo
grafo <- dagitty("dag {
T -> C
Z -> C
T -> Z
}")
# Exibir a figura do grafo
ggdag(grafo, layout = "circle") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  xlab("") + ylab("")

