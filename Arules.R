### Apriori 

library(tidyr)
library(plotly)
library(magrittr)
library(arules)
library(arulesViz)
library(dplyr)
library(reshape2)
library(data.table)
library(ggplot2)
library(stringr)
library(readr)
library(microbenchmark)
library(RSQLServer)
library(RODBC)

# função criada 
db<- conectar_com_banco("--------")

# pegar os dados de cupons 
# data, cpf, loja, cupom, pdv, niv, categoria, qtd, valor unitário
dados <- sqlQuery(
  db,
  "------------"
)

# remove cpf
dados <- dados[, -2]

dados$DATA <- paste0(substring(dados$DAT_OPERACAO_FISCAL, first = 1, last = 4),
                     substring(dados$DAT_OPERACAO_FISCAL, first = 6, last = 7),
                     substring(dados$DAT_OPERACAO_FISCAL, first = 9, last = 10))

# remove data no formato antigo
dados <- dados[, - 1]

dados <- dados[!duplicated(dados), ]

# para saber quantos cupons diferentes e multiplicar pelo suporte 
# isso dá o total de transações com aquela combinação
chave_cupom <- dados %$% paste0(DATA, LOJA, CUPOM, COD_PDV)
#length(unique(chave_cupom))

# Identificador de cupom
dados <- as.data.table(dados)
TID <- unique(dados[, list(LOJA, CUPOM, COD_PDV, DATA)])
TID[, TID := 1:nrow(TID)]

dados_2 <- dados[, list(LOJA, CUPOM, COD_PDV, NIVEL_4, DATA)] %>%
  left_join(y = TID, by = c("LOJA", "CUPOM", "COD_PDV", "DATA")) %>%
  select(TID, NIVEL_4)

setnames(x = dados_2, old = 2, new = "ITEM")

dados_2 <- unique(dados_2)

transDat <- as(split(dados_2[, 'ITEM'], dados_2[, 'TID']), "transactions")

LIST(transDat) # convert 'transactions' to a list, note the LIST in CAPS

itemFrequencyPlot(transDat, topN = 10, type = 'absolute') # plota freq

# apriori é melhor que eclat para base grande
rules <- apriori(transDat, parameter = list(supp = 0.0001, conf = 0.3, target = "rules"))

arules::inspect(rules)
inspect(head(rules))

rules <- rules[!is.redundant(rules)]

# m <- interestMeasure(rules, c("lift", "confidence", "certainty"),
#                      transactions = transDat)
# m

rules <- sort(rules, by = "lift", decreasing = TRUE)

df = data.frame(
  lhs = labels(lhs(rules)),
  rhs = labels(rhs(rules)), 
  rules@quality
)

head(df)

### create graphic 

######### Para inserir nome ao lado do cod. #########
Query <- sqlQuery(db, "--------------")
Query <- unique(Query)
Query$lhs <- as.character(Query$lhs)

df %<>% mutate_if(is.factor, as.character)

# identificar a combinacao
df$ID <- 1:nrow(df)

df %<>% mutate(lhs = gsub( "\\{|\\}", "", df[,1]))
df %<>% mutate(rhs = gsub( "\\{|\\}", "", df[,2]))

# criar uma coluna para cada quarto nível do LHS
df_sep <- df %>% separate_rows(lhs, sep = "\\,")

df_sep$lhs <- trimws(df_sep$lhs)

df_sep <- left_join(df_sep, Query,by = "lhs")

base_concat <- df_sep %>% 
  group_by(ID) %>% 
  summarise(NOME = paste0(NOM, collapse = " + "))

base_final <- left_join(df,base_concat, by = "ID")

# renomear para fazer o join
Query <- rename(Query, rhs = lhs)

# botar como character para fazer o join 
base_final$rhs <- as.character(base_final$rhs)
Query$rhs <- as.character(Query$rhs)

base_final <- left_join(base_final,Query,by = "rhs")

base_final %<>% select(
  lhs, LHS = NOME, rhs, RHS = NOM, 
  SUPPORT = support, CONFIDENCE = confidence, LIFT = lift
)

base_final %<>% arrange(desc(base_final$LIFT))

criar_chave <- function(col1, col2){
  
  x <- sort(c(col1, col2))
  x <- paste0(x, collapse = "_")
  x
}

base_final %<>% mutate_if(is.factor, as.character)

chave <- rep(NA_character_, nrow(base_final))

for (i in 1:nrow(base_final)){
  chave[i] <- criar_chave(base_final$LHS[i], base_final$RHS[i])
}

base_final$chave <- chave

base_final %<>% mutate(chave_2 = paste0(chave, SUPPORT, CONFIDENCE, LIFT))

dist <- base_final %>% distinct(chave_2, .keep_all = TRUE)

base_graph <- dist[,1:7]

# total de transações com aquela combinação
base_graph %<>% mutate(tot = SUPPORT * length(unique(chave_cupom)))

#base_graph <- head(base_graph, n=50)

# dupli <- base_final$chave_2[duplicated(base_final$chave_2)]
# nrow(base_final) - length(dupli) # base dist tem que ter esse tamanho

df <- data.frame(
  lhs = base_graph$lhs,
  rhs = base_graph$rhs,
  Lift = base_graph$LIFT,
  lhs_nome = base_graph$LHS,
  rhs_nome = base_graph$RHS
)

df$Lift %<>% round(2)

p<-ggplot(df, aes(lhs, rhs)) +
  geom_point(aes(colour = Lift,
                 text = paste0("LHS: ", df$lhs_nome, "\n",                
                               "RHS: ", df$rhs_nome, "\n",
                               "Lift: ", df$Lift))
             ,size = 3) +
  scale_colour_gradient(low = "#80bfff", high = "#003366") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin=unit(c(0,0,1,0.5), "cm"))

ggplotly(p, tooltip = "text", width = 1000, height = 500)

#write_rds(base_graph, "---------/base.rds")
