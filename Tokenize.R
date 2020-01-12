library(tidyverse)
library(dplyr)
library(tidytext)
library(stringr)
library(tibble)
library(readr)
library(tm)
library(wordcloud)

df_norm = normalizada_PORT <- read.csv("C:/Users/thoma/Desktop/Data Science Project/normalizada_PORT.csv", stringsAsFactors = FALSE)

base_tokens <- df_norm %>% unnest_tokens(token,norm_1)
base_dual <- df_norm %>% unnest_tokens(token,norm_1, token = "ngrams", n=2)

################## tratamento de termos para termos padrões 

# "cientista de dados"
base_dual$token <- str_replace(base_dual$token,"ciencia dados","cientista de dados")
base_dual$token <- str_replace(base_dual$token,"cientista dados","cientista de dados")
base_dual$token <- str_replace(base_dual$token,"cientistas dados","cientista de dados")
base_dual$token <- str_replace(base_dual$token,"dados ciencia","cientista de dados")

# "analise de dados"
base_dual$token <- str_replace(base_dual$token,"dados analise","analise de dados")
base_dual$token <- str_replace(base_dual$token,"analise dados","analise de dados")

# "modelos estatisticos"
base_dual$token <- str_replace(base_dual$token,"modelos estatisticos","modelos estatisticos")
base_dual$token <- str_replace(base_dual$token,"modelagem estatistica","modelos estatisticos")

# "modelos preditivos"
base_dual$token <- str_replace(base_dual$token,"modelos preditivos","modelos preditivos")
base_dual$token <- str_replace(base_dual$token,"modelagem preditiva","modelos preditivos")


# "Machine learning"
base_dual$token <- str_replace(base_dual$token,"aprendizado maquina","machine learning")
base_dual$token <- str_replace(base_dual$token,"aprendizagem maquina","machine learning")
base_dual$token <- str_replace(base_dual$token,"machine learning","machine learning")

# "banco dados "
base_dual$token <- str_replace(base_dual$token,"banco dados","banco de dados")
base_dual$token <- str_replace(base_dual$token,"bases dados","banco de dados")
base_dual$token <- str_replace(base_dual$token,"bancos dados","banco de dados")

# "mineracao dados"
base_dual$token <- str_replace(base_dual$token,"mineracao dados","mineracao de dados")

# "conjuntos dados"
base_dual$token <- str_replace(base_dual$token,"conjuntos dados","conjuntos de dados")

# "resolucao problemas"
base_dual$token <- str_replace(base_dual$token,"resolucao problemas","resolver problemas")
base_dual$token <- str_replace(base_dual$token,"resolver problemas","resolver problemas")

# "linguagens programacao"
base_dual$token <- str_replace(base_dual$token,"linguagens programacao","linguagens de programacao")
base_dual$token <- str_replace(base_dual$token,"linguagem programacao","linguagens de programacao")

# "visualizacao dados"
base_dual$token <- str_replace(base_dual$token,"visualizacao dados","visualizacao de dados")

# "tomada decisao
base_dual$token <- str_replace(base_dual$token,"tomada decisao","tomada de decisao")


# "matematica "
base_tokens$token <- str_replace(base_tokens$token,"matematicas","matematica")
base_tokens$token <- str_replace(base_tokens$token,"matematicos","matematica")
base_tokens$token <- str_replace(base_tokens$token,"matematicas","matematica")
base_tokens$token <- str_replace(base_tokens$token,"matematico","matematica")

# "modelos"
base_tokens$token <- str_replace(base_tokens$token,"modelo","modelos")
base_tokens$token <- str_replace(base_tokens$token,"modelagem","modelos")
base_tokens$token <- str_replace(base_tokens$token,"modelagens","modelos")
base_tokens$token <- str_replace(base_tokens$token,"modeloss","modelos")

# "analises"
base_tokens$token <- str_replace(base_tokens$token,"analises","analise")

# "negocios"
base_tokens$token <- str_replace(base_tokens$token,"negocios","negocio")

# "tecnologias"
base_tokens$token <- str_replace(base_tokens$token,"tecnologias","tecnologia")

# "estatisticas"
base_tokens$token <- str_replace(base_tokens$token,"estatisticas","estatistica")


################## monta as tabelas com as frequencias 

#frequencia de palavras
fr_token <- base_tokens %>% count(token,sort=TRUE)
fr_dual <- base_dual %>% count(token,sort=TRUE)


################## tratamento da base dual

#exclusao de conjuntos com menos de 10 observacoes
#exclusao de termos que nao fazem sentido e/ou deveriam ser analisada como palavras unicas

fr_dual <- fr_dual %>% filter(n >=10)
fr_dual <- fr_dual %>% filter(token !="estatistica matematica")
fr_dual <- fr_dual %>% filter(token !="r python")
fr_dual <- fr_dual %>% filter(token !="python r")
fr_dual <- fr_dual %>% filter(token !="anos experiencia")
fr_dual <- fr_dual %>% filter(token !="fazer parte")
fr_dual <- fr_dual %>% filter(token !="por exemplo")
fr_dual <- fr_dual %>% filter(token !="vale refeicao")
fr_dual <- fr_dual %>% filter(token !="vale transporte")
fr_dual <- fr_dual %>% filter(token !="todo mundo")
fr_dual <- fr_dual %>% filter(token !="partes interessadas")
fr_dual <- fr_dual %>% filter(token !="ajudar clientes")
fr_dual <- fr_dual %>% filter(token !="plano saude")
fr_dual <- fr_dual %>% filter(token !='ambiente trabalho')
fr_dual <- fr_dual %>% filter(token !='computacao engenharia')
fr_dual <- fr_dual %>% filter(token !='hum ambiente')
fr_dual <- fr_dual %>% filter(token !='ml dl')
fr_dual <- fr_dual %>% filter(token !='mundo real')
fr_dual <- fr_dual %>% filter(token !='seguro vida')
fr_dual <- fr_dual %>% filter(token !='matematica fisica')
fr_dual <- fr_dual %>% filter(token !='matematica estatistica')
fr_dual <- fr_dual %>% filter(token !='requisitos qualificacoes')
fr_dual <- fr_dual %>% filter(token !='responsabilidades atribuicoes')
fr_dual <- fr_dual %>% filter(token !='tecnologia informacao')
fr_dual <- fr_dual %>% filter(token !='dados experiencia')
fr_dual <- fr_dual %>% filter(token !='dados empresa')

#cria um wordcloud com os termos que mais aparecem
wordcloud(words = fr_dual$token, freq = fr_dual$n, random.order=FALSE,
          colors=brewer.pal(8, "Dark2"),
          scale=c(2,1))

################## tratamento da base dual

#exclusao de conjuntos com menos de 30 observacoes
#exclusao de termos que nao fazem sentido

base_aux_dual <- fr_dual %>% unnest_tokens(token,token)
base_aux_dual <- base_aux_dual %>%
                 group_by(token) %>% 
                 summarize(qtd_obs=sum(n))

fr_token <- fr_token %>% filter(n>=30)

fr_token_2<-merge(x=fr_token,y=base_aux_dual,by="token",all.x=TRUE)
fr_token_2[is.na(fr_token_2)] <- 0

fr_token_2$qtd_n <- fr_token_2$n - fr_token_2$qtd_obs

fr_token_2 <- fr_token_2 %>% filter(qtd_n>=30)

fr_token_2 <- fr_token_2 %>% filter(token !="experiencia")
fr_token_2 <- fr_token_2 %>% filter(token !="trabalho")
fr_token_2 <- fr_token_2 %>% filter(token !="conhecimento")
fr_token_2 <- fr_token_2 %>% filter(token !='equipe')
fr_token_2 <- fr_token_2 %>% filter(token !='empresa')
fr_token_2 <- fr_token_2 %>% filter(token !='fazer')
fr_token_2 <- fr_token_2 %>% filter(token !='trabalhar')
fr_token_2 <- fr_token_2 %>% filter(token !='sobre')
fr_token_2 <- fr_token_2 %>% filter(token !='rapido')
fr_token_2 <- fr_token_2 %>% filter(token !='desenvolvimento')
fr_token_2 <- fr_token_2 %>% filter(token !='hum')
fr_token_2 <- fr_token_2 %>% filter(token !='areas')
fr_token_2 <- fr_token_2 %>% filter(token !='requisitos')
fr_token_2 <- fr_token_2 %>% filter(token !='ser')
fr_token_2 <- fr_token_2 %>% filter(token !='tempo')
fr_token_2 <- fr_token_2 %>% filter(token !='ambiente')
fr_token_2 <- fr_token_2 %>% filter(token !='desenvolver')
fr_token_2 <- fr_token_2 %>% filter(token !='mundo')
fr_token_2 <- fr_token_2 %>% filter(token !='cliente')
fr_token_2 <- fr_token_2 %>% filter(token !='saude')
fr_token_2 <- fr_token_2 %>% filter(token !='vale')
fr_token_2 <- fr_token_2 %>% filter(token !='pessoas')
fr_token_2 <- fr_token_2 %>% filter(token !='capacidade')
fr_token_2 <- fr_token_2 %>% filter(token !='etc')
fr_token_2 <- fr_token_2 %>% filter(token !='novas')
fr_token_2 <- fr_token_2 %>% filter(token !='responsabilidades')
fr_token_2 <- fr_token_2 %>% filter(token !='grandes')
fr_token_2 <- fr_token_2 %>% filter(token !='todos')
fr_token_2 <- fr_token_2 %>% filter(token !='atraves')
fr_token_2 <- fr_token_2 %>% filter(token !='oportunidades')
fr_token_2 <- fr_token_2 %>% filter(token !='parte')
fr_token_2 <- fr_token_2 %>% filter(token !='that')
fr_token_2 <- fr_token_2 %>% filter(token !='conhecimentos')
fr_token_2 <- fr_token_2 %>% filter(token !='criar')
fr_token_2 <- fr_token_2 %>% filter(token !='ajudar')
fr_token_2 <- fr_token_2 %>% filter(token !='equipes')
fr_token_2 <- fr_token_2 %>% filter(token !='vai')
fr_token_2 <- fr_token_2 %>% filter(token !='todo')
fr_token_2 <- fr_token_2 %>% filter(token !='usando')
fr_token_2 <- fr_token_2 %>% filter(token !='bem')
fr_token_2 <- fr_token_2 %>% filter(token !='linguagem')
fr_token_2 <- fr_token_2 %>% filter(token !='linguagens')
fr_token_2 <- fr_token_2 %>% filter(token !='oportunidade')
fr_token_2 <- fr_token_2 %>% filter(token !='profissional')
fr_token_2 <- fr_token_2 %>% filter(token !='beneficios')
fr_token_2 <- fr_token_2 %>% filter(token !='empresas')
fr_token_2 <- fr_token_2 %>% filter(token !='forma')
fr_token_2 <- fr_token_2 %>% filter(token !='processo')
fr_token_2 <- fr_token_2 %>% filter(token !='ter')
fr_token_2 <- fr_token_2 %>% filter(token !='area')
fr_token_2 <- fr_token_2 %>% filter(token !='incluindo')
fr_token_2 <- fr_token_2 %>% filter(token !='plano')
fr_token_2 <- fr_token_2 %>% filter(token !='principais')
fr_token_2 <- fr_token_2 %>% filter(token !='anos')
fr_token_2 <- fr_token_2 %>% filter(token !='e')
fr_token_2 <- fr_token_2 %>% filter(token !='ingles')
fr_token_2 <- fr_token_2 %>% filter(token !='se')
fr_token_2 <- fr_token_2 %>% filter(token !='uso')
fr_token_2 <- fr_token_2 %>% filter(token !='construcao')
fr_token_2 <- fr_token_2 %>% filter(token !='produto')
fr_token_2 <- fr_token_2 %>% filter(token !='data')

#cria um wordcloud com os termos que mais aparecem
wordcloud(words = fr_token_2$token, freq = fr_token_2$qtd_n, random.order=FALSE,
          colors=brewer.pal(8, "Dark2"),
          scale=c(3,0.6))

####################juntando os dois dataframes

fr_token_2 = select(fr_token_2, -c(2,3))

names(fr_token_2)[2] <- "n"

fr_fim<-rbind.data.frame(fr_dual, fr_token_2)

#cria um wordcloud com os termos que mais aparecem
wordcloud(words = fr_fim$token, freq = fr_fim$n, random.order=FALSE,
          colors=brewer.pal(8, "Dark2"),scale=c(2,0.3))


