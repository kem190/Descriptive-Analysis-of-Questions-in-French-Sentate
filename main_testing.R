library(readxl)
deputes <- read_excel("D:\\A_File_Storage\\New_Desktop\\Rdir\\ganshefaguoneizheng\\listes_deputes.xlsx")
data <- read_excel("D:\\A_File_Storage\\New_Desktop\\Rdir\\ganshefaguoneizheng\\questions_parlementaires_AN.xlsx")

library(dplyr)
library(stringr)
data_n <- sample_n(data, 100)
data <- data %>%
  filter(!is.na(texte))

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
unique_names_in_data <- unique(data$names)
missing_names <- sort(setdiff(unique_names_in_data, deputes$full_name))
data_filtered <- data %>%
  filter(!names %in% missing_names)
merged_data <- data_filtered %>%
  left_join(deputes, by = c("names" = "full_name"))

colnames(merged_data)

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

