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
  select(type, rubrique, analyse, auteur.groupe.developpe, texte, texte.1, minInt.developpe) %>%
  rename(nature = type, theme = rubrique, summary = analyse, party = auteur.groupe.developpe, text = texte, answers = texte.1, answerer = minInt.developpe)

df <- df %>%
  filter(nature != "QG")

myfrench <- c("a","mme", "ainsi", "d'une", "d'un", "si", "em", "2023", "qu'il", "francais", "attire",
              "2021", "2022", "°", "000", "<", ">")

dict_for_frequency <- c("ministre", "ans", "c'est", "appelle", "souhaite", "mise",  "connaitre", "n'est", "l'article", "plus",
                        "député", "notamment", "demande", "interroge", "cas", "mettre", "savoir", "nombre", 
                        "gouvernement", "loi", "l'attention", "personnes", "situation", "face", "france", "nationale")
forgivemyfrench <- c(stopwords("french"), myfrench, stopwords_fr)
df$party_nature_interaction <- interaction(df$party, df$nature)
# Tagging and combining
df$QA <- paste0("Q.", df$text, " A.", df$answers)

dfm <- df %>%
  corpus(text_field = "text") %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_remove(pattern = "https://[^\\s]*") %>%
  tokens_remove(forgivemyfrench) %>%
  dfm()

topfeatures(dfm, n = 50)

dfm_agg <- dfm %>%
  dfm_group(groups = docvars(dfm, "party_nature_interaction")) %>%
  dfm_trim(min_docfreq = 2, docfreq_type = "count")

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

##################### do the same to the answers ##############
unique(data$minInt.developpe)

data <- data %>%
  filter(typeJO != "JO_LOI_DECRET") %>%
  filter(type != "QG")
  
answerers <- as.data.frame(table(data$minInt.developpe))
answerers <- answerers %>%
  arrange(-Freq)
