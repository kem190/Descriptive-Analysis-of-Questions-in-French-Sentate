# Descriptive-Analysis-of-Questions-in-French-Sentate
This is a simple analysis on French Senate "Basic Questions" 2022-2023
Data Source: https://data.senat.fr/la-base-questions/ (also attached as .csv)
Branch from a coursework assignment of ZhengHao Hu, who has done the analysis and the writing part of this work. He is also the person that have substantive knowledge of French Politics. While I have done all the coding.

# Introduction
Parliamentary questions play a fundamental role in France's democratic institutions. Asking such questions is one of the main ways in which parliamentarians exercise control and contributing to the transparency and public debate.
Among the three distinct forms that questions took place, Questions to the Government (QOSD) act as an important role as it is used for general interest and attract much more public attention, since it is made public on TV. This is also the questions that 

The questions we tried to answer:
"left blank"

As in this project I mainly worked on the code so I will try to emphisize more on the coding part for now.
# Background
It is vital to understand that the three different types (originally named nature in french) of questions, QG, QE and QOSD, pose differences in purpose, procedure and visibility, and they are also completely different in their data structure. 
QE (questions écrites) means that the question is written. These questions often tend to focus on specific issues like the difficulties in implementing laws, or to inquire about a minister's reform plans. They are written down and handed in sometimes weeks before answers so this allows the minister to form a complementary response. The questions and answers are published on the official journal yet receive less public attention compared to televised questions.
QG (Questions au Gouvernement) is used for broader general interests, often addressing significant national issues or policies. These questions are not exhibited earlier so they allow more dynamic and immediate responses. They are recorded during a TV session where deputies pose question directly to government officials so they also receive greater attention from the media and citizens.
QOSD (Questions Orales Sans Débat) focus on local and technical interests and tend to be more specific and succinct than other questions.

Concerning the data, the answers of the questions are subject to different formatting principles but are stored in the same place, this makes it harder to extract the texts when put together. For example, though the questions are stored in a consistant manner, the answer cell for QE always concerning two to three speakers, while the cell for QG contains multiple follow-up questions.

  # Steps
  ## Cleaning
For the data cleaning process, as the data itself is pretty organized, we tried to maintain as much data as possible.

Among the three question types, Written questions (QE) has a much higher proportion than the other to, of 95.34% and its text has a clearer structure compared to other two types, so I've decided to use this questions only (making the data biased in some sense)
For example, the we used seperate stopwords lists for our analysis, for estimating the wordfish model, we simple removed simple stopwords and urls, we even left "ministre" inside. 
```R
myfrench <- c("a","mme", "ainsi", "d'une", "d'un", "si", "em", "2023", "qu'il", "francais", "attire",
              "2021", "2022", "°", "000", "<", ">")
```
When inspecting the word frequencies, we have to remove the words that are simultaniously used among all the parties.
```R
dict_for_frequency <- c("ministre", "ans", "c'est", "appelle", "souhaite", "mise",  "connaitre", "n'est", "l'article", "plus",
                        "député", "notamment", "demande", "interroge", "cas", "mettre", "savoir", "nombre", 
                        "gouvernement", "loi", "l'attention", "personnes", "situation", "face", "france", "nationale")
```

## Merging
Since the data are distributed into two seperate .csv files, we have to merge them in order to 
We've decided to use the names of the parliament members as the key for the merge. From the main table, we managed to extract the Last names of the deputee (auteur) of the question. Looks horrible, but that's the bset I can do with French names.
```R
data <- data %>%
  mutate(names = str_extract(texte, "(?<=^M\\. |^Mme\\s)[A-ZÀÂÇÉÈÊËÎÏÔŒÛÙÜ][a-zàâçéèêëîïôûùüÿñæœ]+(?:[-'/]?[A-ZÀÂÇÉÈÊËÎÏÔŒÛÙÜ][a-zàâçéèêëîïôûùüÿñæœ]+)?(?:\\s(?:d'|de\\s|la\\s|à\\s|l')?[A-ZÀÂÇÉÈÊËÎÏÔŒÛÙÜ][a-zàâçéèêëîïôûùüÿñæœ]+(?:[-'/]?[A-ZÀÂÇÉÈÊËÎÏÔŒÛÙÜ][a-zàâçéèêëîïôûùüÿñæœ]+)?)*"))
```
This have managed to extract 557 names out of the main table, whereas the info table contains 577 names. With manual inspection, we've been able to locate the people that's not on the list, whose names are harder to find, like "Emmanuel Taché de la Pagerie" and two "Alexandra Martin"s, and I've fixed them manually. The names then came to 564.
```R
deputes <- deputes %>%
  mutate(full_name = paste(Prénom, Nom))
unique_names_in_data <- unique(data$names)
missing_names <- sort(setdiff(deputes$full_name, unique_names_in_data))
missing_names
```
We then inspected the missing values and found that there are 10 rows missing the name cell, and are able to locate 5 names, yet they are not on the "deputes" list, weird.
Checking again on the deputes.csv list, we found that people's full names can act as a mergeing key, so we did that.
```R
merged_data <- data%>%
  left_join(deputes, by = c("names" = "full_name"))
merged_data <- merged_data %>%
  filter(!is.na(names))
```

## Descriptive Statistical Analysis
We started with a few descriptive statistics based on the processed data.
First we chekced the deputees who are most constantly posting questions as well as their party names.
```R
question_count <- as.data.frame(table(data$auteur.groupe.developpe))
```
Then we've fiddled with the co occurence of different variables like this:
```R
co_person_party <- merged_data %>%
  with(table(auteur.groupe.developpe, names)) %>%
  as.data.frame() %>%
  left_join(question_count, by = c("auteur.groupe.developpe" = "Var1")) %>%
  mutate(percentage = Freq.x/Freq.y * 100) %>%
  rename(count.person = Freq.x, count.party = Freq.y) %>%
  filter(count.person != 0) %>%
  arrange(-percentage)

co_party_category <- merged_data %>%
  filter(category != "Others") %>%
  with(table(auteur.groupe.developpe, category)) %>%
  as.data.frame() %>%
  arrange(-Freq)
```
However, nothing really interesting popped up, since either of us two have 
Good thing is we have got a rather clear picture of the data through this process. 
Few things that we have noticed:
1. The most concerned catagory is carbon energy, which partly ressonates the energy crisis in Europe in 2022.
2. There are a huge number of questions concerning health (professions de santé), which is a quite vague catagory with huge amounts of questions focused on questioning the meaning of specific articles of law.
3. We have noticed that the original themes catagorized in the dataset is quite obfuscate. For example, "agriculture" contains crops, farm animals and even wild animals, so a further catagorization of the questions could be considered.

## Text Analysis: a Wordfish Model
After the initial description, we've decided to add more flavour to the analysis by digging deeper into the texts of the questions and answers, and trying to figure out the attitudes of the parties, we decided to use wordfish model.

As stated, in this dataset, answers often tend to have a different structure. Because of this, we have estimated several models to obtain a comprehensive understanding of the data.

Fist, we've estimated a model only from the questions:
![image](https://github.com/kem190/Descriptive-Analysis-of-Questions-in-French-Sentate/assets/115566439/a01a151b-c1ca-4574-a0d0-b990926bf41d)

and as confirmed by our substantive knowledge for French politics ("left blank"), also further validation of the models will be the focus of the next part.

This lead to interesting results as QE often find themselves on position that is slightly left compared to their QOSD counterparts, and the result of a Wilcoxon signed-rank test showed that the ranking of the parties are quite similar.
![image](https://github.com/kem190/Descriptive-Analysis-of-Questions-in-French-Sentate/assets/115566439/19257c1b-f649-4690-bea1-056a038298a8)

("intentionally left blank")
## Validation

# Conclusion
