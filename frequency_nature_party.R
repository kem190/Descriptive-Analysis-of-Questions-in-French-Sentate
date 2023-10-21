library(readxl)
library(dplyr)
data <- read_excel("D:\\A_File_Storage\\New_Desktop\\Rdir\\ganshefaguoneizheng1\\questions_parlementaires_AN.xlsx")
colnames(data)
df <- data %>%
  select(type, rubrique, analyse, auteur.groupe.developpe, texte) %>%
  rename(nature = type, theme = rubrique, summary = analyse, party = auteur.groupe.developpe, text = texte)

unique(df$party)

qe <- df %>%
  filter(nature == "QE")

qg <- df %>%
  filter(nature == "QG")

qosd <- df %>%
  filter(nature == "QOSD")

############################# BAG OF WORDS ##########################
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(tidytext)
library(stringdist)
library(corrplot)
library(janeaustenr)
library(quanteda)

myfrench <- c("ministre", "plus", "a", "demande", "mme", "santé",
                     "gouvernement", "l'attention", "ainsi", "d'une",
                     "d'un", "situation", "france", "si", "personnes")
forgivemyfrench <- c(stopwords("french"), myfrench)

freq_qg <- qg$text %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(forgivemyfrench) %>%
  dfm() %>%
  topfeatures(n = 20)

freq_qe <- qe$text %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(forgivemyfrench) %>%
  dfm() %>%
  topfeatures(n = 30)

freq_qosd <- qosd$text %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(forgivemyfrench) %>%
  dfm() %>%
  topfeatures(n = 30)

tb_qg <- as.data.frame(freq_qg)
tb_qe <- as.data.frame(freq_qe)
tb_qosd <- as.data.frame(freq_qosd)
library(wordcloud)

write.csv(tb_qe, file = "qg_freq.csv", fileEncoding = "UTF-8")

wordcloud(names(freq_qe), freq_qe, max.words = 30, colors = brewer.pal(8, "Dark2"))

