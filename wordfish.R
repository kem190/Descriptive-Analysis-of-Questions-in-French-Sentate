library(readxl)
library(dplyr)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(quanteda.textmodels)
library(tidytext)
library(stringdist)
library(corrplot)
library(janeaustenr)
library(quanteda)
library(SnowballC)
library(lsa)


data <- read_excel("D:\\A_File_Storage\\New_Desktop\\Rdir\\ganshefaguoneizheng1\\questions_parlementaires_AN.xlsx")

df <- data %>%
  select(type, rubrique, analyse, auteur.groupe.developpe, texte) %>%
  rename(nature = type, theme = rubrique, summary = analyse, party = auteur.groupe.developpe, text = texte)

df <- df %>%
  filter(nature != "QG")

myfrench <- c("ministre", "plus", "a", "demande", "mme", "santé",
              "gouvernement", "l'attention", "ainsi", "d'une",
              "d'un", "situation", "france", "si", "personnes", "em", "loi", "nationale", "2023", "qu'il", "francais", "attire",
              "souhaite", "interroge", "cas", "mettre", "savoir", "nombre",
              "député", "notamment", "c'est", "ans", "mise", "n'est", 
              "connaitre", "appelle", "face", "2021", "l'article", "2022", "° ", "000")

forgivemyfrench <- c(stopwords("french"), myfrench, stopwords_fr, urls)

df$party_nature_interaction <- interaction(df$party, df$nature)

#df <- df %>%
#  select(text, party_nature_interaction)


dfm <- df %>%
  corpus(text_field = "text") %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_remove(pattern = "https://[^\\s]*") %>%
  tokens_remove(forgivemyfrench) %>%
  dfm()

dfm_agg <- dfm_group(dfm, groups = docvars(dfm, "party_nature_interaction"))

dfm_agg <- dfm_trim(dfm_agg, min_docfreq = 2, docfreq_type = "count")


dfm_agg_wordfish <- textmodel_wordfish(dfm_agg, sparse = TRUE)

# Check results
summary(dfm_agg_wordfish)

textplot_scale1d(dfm_agg_wordfish)
textplot_scale1d(dfm_agg_wordfish, groups = docvars(dfm_agg, "party"))
textplot_scale1d(dfm_agg_wordfish, groups = docvars(dfm_agg, "nature"))
textplot_scale1d(dfm_agg_wordfish, margin = "features")

features <- dfm_agg_wordfish[["features"]]

betas <- dfm_agg_wordfish[["beta"]]

feat_betas <- as.data.frame(cbind(features, betas))
feat_betas$betas <- as.numeric(feat_betas$betas)

feat_betas %>%
  arrange(betas) %>%
  top_n(20)

