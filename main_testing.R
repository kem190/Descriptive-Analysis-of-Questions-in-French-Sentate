## descriptive
library(readxl)
library(writexl)
deputes <- read_excel("D:\\A_File_Storage\\New_Desktop\\Rdir\\ganshefaguoneizheng1\\listes_deputes.xlsx")
data <- read_excel("D:\\A_File_Storage\\New_Desktop\\Rdir\\ganshefaguoneizheng1\\questions_parlementaires_AN.xlsx")

library(dplyr)
library(stringr)
data <- data %>%
  filter(!is.na(texte)) # 删除了qg

data <- data %>%
  mutate(names = str_extract(texte, "(?<=^M\\. |^Mme\\s)[A-ZÀÂÇÉÈÊËÎÏÔŒÛÙÜ][a-zàâçéèêëîïôûùüÿñæœ]+(?:[-'/]?[A-ZÀÂÇÉÈÊËÎÏÔŒÛÙÜ][a-zàâçéèêëîïôûùüÿñæœ]+)?(?:\\s(?:d'|de\\s|la\\s|à\\s|l')?[A-ZÀÂÇÉÈÊËÎÏÔŒÛÙÜ][a-zàâçéèêëîïôûùüÿñæœ]+(?:[-'/]?[A-ZÀÂÇÉÈÊËÎÏÔŒÛÙÜ][a-zàâçéèêëîïôûùüÿñæœ]+)?)*"))
data <- data %>%
  mutate(names = if_else(names == "Loïc Prud", "Loïc Prud'homme", names),
         names = if_else(names == "Emmanuel Taché", "Emmanuel Taché de la Pagerie", names),
         names = if_else(names == "Christelle", "Christelle D'Intorni", names),
         names = if_else(names == "Emeline", "Emeline K/Bidi", names),
         names = if_else(names == "Pierre Morel", "Pierre Morel-À-L'Huissier", names),
         )


deputes <- deputes %>%
  mutate(full_name = paste(Prénom, Nom))
#unique_names_in_data <- unique(data$names)
#missing_names <- sort(setdiff(unique_names_in_data, deputes$full_name))
data_filtered <- data %>%
  filter(!names %in% missing_names)
merged_data <- data_filtered %>%
  left_join(deputes, by = c("names" = "full_name"))
merged_data <- merged_data %>%
  filter(!is.na(names)) # 删除了找不到的人

############# count speakers ##################
speaker_counts <- merged_data %>%
  group_by(names) %>%
  summarize(count = n()) %>%
  arrange(-count)

questions_analysis <- merged_data %>%
  group_by(names, rubrique) %>%
  summarize(count = n()) %>%
  arrange(-count)

print(questions_analysis, n = 20)


############# sorting with chatGPT4.0 (2023.10)#######################

#theme_count <- merged_data %>%
#  group_by(rubrique) %>%
#  summarise(count = n()) %>%
#  arrange(desc(count))

#print(theme_count, n = 209)

merged_data$category <- case_when(
  merged_data$rubrique %in% c("énergie et carburants", "eau et assainissement", "environnement", "biodiversité", "bois et forêts", "déchets", "numérique", "pollution", "aquaculture et pêche professionnelle", "mer et littoral", "climat", "développement durable") ~ "Environment and Energy",
  
  merged_data$rubrique %in% c("professions de santé", "personnes handicapées", "santé", "établissements de santé", "pharmacie et médicaments", "maladies", "médecine", "assurance maladie maternité", "sang et organes humains", "dépendance", "fin de vie et soins palliatifs", "personnes âgées", "interruption volontaire de grossesse", "bioéthique") ~ "Health and Welfare",
  
  merged_data$rubrique %in% c("agriculture", "animaux", "élevage", "chasse et pêche", "agroalimentaire", "cours d'eau, étangs et lacs") ~ "Agriculture and Animals",
  
  merged_data$rubrique %in% c("logement", "logement : aides et prêts", "urbanisme") ~ "Housing and Urbanism",
  
  merged_data$rubrique %in% c("enseignement", "enseignement supérieur", "enseignement secondaire", "enseignement maternel et primaire", "enseignement technique et professionnel", "enseignements artistiques", "enseignement privé", "enseignement agricole", "éducation physique et sportive") ~ "Education",
  
  merged_data$rubrique %in% c("outre-mer", "Français de l'étranger", "étrangers", "frontaliers", "réfugiés et apatrides", "immigration", "nationalité") ~ "International and Migration",
  
  merged_data$rubrique %in% c("sécurité des biens et des personnes", "sécurité routière", "ordre public", "police", "gendarmerie", "sécurité sociale", "harcèlement", "crimes, délits et contraventions", "justice") ~ "Security and Justice",
  
  merged_data$rubrique %in% c("enfants", "femmes", "jeunes", "famille", "gens du voyage") ~ "Demographics",
  
  merged_data$rubrique %in% c("commerce et artisanat", "entreprises", "industrie", "commerce extérieur", "tourisme et loisirs", "bâtiment et travaux publics", "professions libérales", "économie sociale et solidaire", "emploi et activité", "chômage", "travail", "services publics", "services", "politique économique", "emploi et activité") ~ "Economy and Employment",
  
  merged_data$rubrique %in% c("transports ferroviaires", "automobiles", "transports routiers", "transports", "transports urbains", "transports aériens", "transports par eau", "cycles et motocycles") ~ "Transportation",
  
  merged_data$rubrique %in% c("impôts et taxes", "impôt sur le revenu", "impôts locaux", "taxe sur la valeur ajoutée", "finances publiques", "impôt sur les sociétés", "impôt sur la fortune immobilière") ~ "Taxes and Finances",
  
  merged_data$rubrique %in% c("fonctionnaires et agents publics", "collectivités territoriales", "communes", "élus", "administration", "politique extérieure", "Parlement", "Gouvernement", "ministères et secrétariats d'État", "Union européenne") ~ "Government and Administration",
  
  merged_data$rubrique %in% c("associations et fondations", "banques et établissements financiers", "droits fondamentaux", "pauvreté", "handicapés", "laïcité", "religions et cultes", "démographie", "régions", "départements") ~ "Social Issues and Civil Rights",
  
  merged_data$rubrique %in% c("numérique", "télécommunications", "Internet", "nouvelles technologies") ~ "Technology and Communication",
  
  merged_data$rubrique %in% c("culture", "arts et spectacles", "patrimoine culturel", "presse et livres", "audiovisuel et communication", "langue française") ~ "Culture and Media",
  
  TRUE ~ "Others"
)

################## parties' concern #####################
#colnames(merged_data)
co_party_category <- merged_data %>%
  filter(category != "Others") %>%
  with(table(auteur.groupe.developpe, category)) %>%
  as.data.frame() %>%
  arrange(-Freq)

co_party_theme <- data %>%
  with(table(auteur.groupe.developpe, rubrique)) %>%
  as.data.frame() %>%
  arrange(-Freq)
  
question_count <- as.data.frame(table(data$auteur.groupe.developpe))
co_party_minister <- data %>%
  with(table(auteur.groupe.developpe, minInt.developpe)) %>%
  as.data.frame() %>%
  left_join(question_count, by = c("auteur.groupe.developpe" = "Var1")) %>%
  mutate(percentage = Freq.x/Freq.y * 100) %>%
  arrange(-percentage)

write.csv(co_occurrence, file = "party_category.csv", fileEncoding = "UTF-8")
###################### unused, plotting themes overtime ##############################

eng_data <- merged_data %>%
  select(type, rubrique, analyse, names, Région, Département, `Groupe politique (complet)`, Profession, dateJO) %>%
  rename(nature = type, theme = rubrique, summary = analyse, region = Région, party = `Groupe politique (complet)`, date = dateJO, profession = Profession)


library(lubridate)
eng_data <- eng_data %>%
  mutate(
    date = as.Date(date, format="%Y-%m-%d"),  # convert to Date type
    HalfMonth = if_else(day(date) <= 15, 
                        paste0(year(date), "-", month(date), "-1"),
                        paste0(year(date), "-", month(date), "-2"))
  )

summary(table(eng_data$theme))
themes <- arrange(as.data.frame(table(eng_data$theme)), desc(Freq))
print(length(unique(eng_data$region)))

themes$percentage = (themes$Freq/16665)
co_occurrence <- eng_data %>%
  with(table(HalfMonth, theme)) %>%
  as.data.frame() %>%
  arrange(-Freq)

themes_to_plot <- head(themes$Var1, n = 15)

mid_plot <- co_occurrence %>% 
  filter(theme %in% themes_to_plot)
  
mid_count <- co_occurrence %>%
  group_by(HalfMonth) %>%
  summarise(total = sum(Freq))

normalized_data <- mid_plot %>%
  left_join(mid_count, by = "HalfMonth") %>%
  mutate(percentage = (Freq / total) * 100)

normalized_data <- normalized_data %>% arrange(HalfMonth)

normalized_data$HalfMonth <- factor(normalized_data$HalfMonth, levels = unique(normalized_data$HalfMonth))

library(ggplot2)

ggplot(normalized_data, aes(x = HalfMonth, y = percentage, group = theme, color = theme)) +
  geom_line() +
  labs(title = "Percentage Line Chart for Selected Themes", x = "Date", y = "Percentage (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

co_occurrence <- eng_data %>%
  with(table(party, theme)) %>%
  as.data.frame() %>%
  arrange(-Freq)

write.csv(co_occurrence, file = "co_party_theme.csv", fileEncoding = "UTF-8")
write.csv(themes, file = "themes.csv", fileEncoding = "UTF-8")
write.csv(merged_data, file = "data_table.csv", fileEncoding = "UTF-8")
View(co_occurrence)




###############################################################


colnames(data)
colnames(qg)

qg <- read.csv("D:\\A_File_Storage\\New_Desktop\\Rdir\\ganshefaguoneizheng\\qg.csv", sep = ";", header = TRUE, fileEncoding = "windows-1252")

data_theme <- as.data.frame(table(data$Thème.s.))

data_theme <- data_theme[order(-data_theme$Freq), ]

qe <- subset(data, data$Nature == "QE")
qg <- subset(data, data$Nature == "QG")
qosd <- subset(data, data$Nature == "QOSD")

head(data$Ministère.de.réponse)

dfm_qe <- corpus(qe, text_field = "")

library(dplyr)
library(tibble)

summary_qe <- qe %>%
  count(Thème.s., name = "Count") %>%
  mutate(Percentage = (Count / n()) * 100) %>%
  rename(String = Thème.s.) %>%
  arrange(desc(Percentage))

summary_qg <- qg %>%
  count(Thème.s., name = "Count") %>%
  mutate(Percentage = (Count / n()) * 100) %>%
  rename(String = Thème.s.) %>%
  arrange(desc(Percentage))

summary_qosd <- qosd %>%
  count(Thème.s., name = "Count") %>%
  mutate(Percentage = (Count / n()) * 100) %>%
  rename(String = Thème.s.) %>%
  arrange(desc(Percentage))

table(data$Groupe)

theme_counts <- table(qe$Thème.s.)

percentages <- (theme_counts / nrow(qe)) * 100

summary_qe <- data.frame(
  String = names(theme_counts),
  Count = as.numeric(theme_counts),
  Percentage = as.numeric(percentages)
)

library(dplyr)

co_occurrence <- data %>%
  group_by(Groupe, Thème.s.) %>%
  tally() %>%
  arrange(desc(n))


######################################################

percentages <- (table(qe$Thème.s.)/ nrow(qe)) * 100




dim(data_Nature)


missing_values <- sum(is.na(data$Groupe))
print(missing_values)

