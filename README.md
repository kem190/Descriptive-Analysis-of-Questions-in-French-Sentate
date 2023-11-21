# Descriptive-Analysis-of-Questions-in-French-Sentate
This is a simple analysis on French Senate "Basic Questions" 2022-2023
Data Source: https://data.senat.fr/la-base-questions/ (also attached as .csv)
Branch from a coursework assignment of ZhengHao Hu, who has done the analysis and the writing part of this work. He is also the person that have substantive knowledge of French Politics. While I have done all the coding.

# Introduction
Parliamentary questions play a fundamental role in France's democratic institutions. Asking such questions is one of the main ways in which parliamentarians exercise control and contributing to the transparency and public debate.
Among the three distinct forms that questions took place, Questions to the Government (QOSD) act as an important role as it is used for general interest and attract much more public attention, since it is made public on TV. This is also the questions that 

The questions we tried to answer:

# Background

# Steps
## Cleaning
For the data cleaning process, as the data itself is pretty organized, we tried to maintain as much data as possible.
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

## Descriptive Statistical Analysis
Among the three question types, Written questions (QE) has a much higher proportion than the other to, of 95.34%, 
## Text Analysis: a Wordfish Model
## Validation

# Conclusion
