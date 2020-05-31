library(dplyr) #für Data Prep
library(tidyr) #für Data Prep
library(tidytext) #für Textanalyse
library(lsa)  # Stopwörter 
library(ggplot2) # zur Visualisierung


text <- c('Sehr geehrter Herr Präsident! ...Vielen Dank.')
text <- data.frame(text)

##### Die Datenaufbereitung zum text mining #####
library(tidytext)
text_words <- text %>%
  mutate(text = gsub("[0-9]", "", text, fixed = F)) %>% # ohne Nummern
  mutate(text = gsub("[[:punct:]]", "", text, fixed = F)) %>% # ohne Satzzeichen
  mutate(text = tolower(text)) %>% # Kleinschreibung
  unnest_tokens(word, text) # Zerlegung in einzelne Wörter
head(text_words)
# Anzahl Wörter
nrow(text_words)
# Anzahl "aber"
text_words %>%
  filter(word == "aber") %>%
  count(word)
# Stoppwörter aussortieren
text_words <- text_words %>%
  anti_join(get_stopwords(language = "de"))
# Anzahl Wörter
nrow(text_words)
# Anzahl einzener Begriffe
length(unique(text_words$word))
# die häufigsten Wörter
text_words %>%
  count(word, sort = TRUE) %>%
  top_n(10)


##### Die Verknüpfung mit einer Sentiment-Bibliothek ##### 
# Aufbereitung der Sentiment Bibliothek
# SentiWS - Dateien hier runterladen: https://wortschatz.uni-leipzig.de/en/download
# a) negative Wörter
negativ <- read.table("~/.../Sentiment Analysis/SentiWS_v2.0_Negative.txt", fill = TRUE) # die Textdatei einlesen
# zuerst separieren wir die Wörter in V1
neg1 <- negativ %>%
  select(V1, V2) %>% #wir brauchen nur diese beiden Spalten
  mutate(V1 = as.character(V1)) %>%  #benötigt für den nächsten Schritt
  mutate(V1 = sub("\\|.*","\\",V1)) %>% #bereinigt ohne den Anhang nach "|"
  `colnames<-`(c("word", "sentiment")) #Spalten werden umbenannt
# nun separieren wir die Wörter in V2
einzel_negativ <- strsplit(as.character(negativ$V3), split = ",") #die aufgelisteten Wörter werden getrennt
neg2 <- data.frame(V1 = rep(negativ$V2, sapply(einzel_negativ, length)), V3 = unlist(einzel_negativ)) %>% #und mit den Werten in V2 wieder zusammengefügt
  `colnames<-`(c("sentiment", "word")) #Spalten werden umbenannt
# b) positive Wörter
positiv <- read.table("~/.../Sentiment Analysis/SentiWS_v2.0_Positive.txt", fill = TRUE) # die Textdatei einlesen
# zuerst separieren wir die Wörter in V1
pos1 <- positiv %>%
  select(V1, V2) %>% #wir brauchen nur diese beiden Spalten
  mutate(V1 = as.character(V1)) %>%  #benötigt für den nächsten Schritt
  mutate(V1 = sub("\\|.*","\\",V1)) %>% #bereinigt ohne den Anhang nach "|"
  `colnames<-`(c("word", "sentiment")) #Spalten werden umbenannt
# nun separieren wir die Wörter in V2
einzel_positiv <- strsplit(as.character(positiv$V3), split = ",") #die aufgelisteten Wörter werden getrennt
pos2 <- data.frame(V1 = rep(positiv$V2, sapply(einzel_positiv, length)), V3 = unlist(einzel_positiv)) %>% #und mit den Werten in V2 wieder zusammengefügt
  `colnames<-`(c("sentiment", "word")) #Spalten werden umbenannt (Achtung, andere Reihenfolge)
# c) gemeinsames Lexikon aus den vier Dataframes
SentiWS_df <- rbind(neg1 %>%
                      mutate(Polarität = "negative"),
                    neg2%>%
                      mutate(Polarität = "negative"),
                    pos1 %>%
                      mutate(Polarität = "positive"), 
                    pos2 %>%
                      mutate(Polarität = "positive")) %>%
  mutate("word" = as.character(word))
SentiWS_df <- SentiWS_df[!duplicated(SentiWS_df$word),] #manche Wörter kommen durch die Umwandlung dopppelt vor; jeweils der erste Eintrag wird behalten
head(SentiWS_df)
# Sentiment-Werte anfügen
text_words <- text_words %>%
  inner_join(SentiWS_df, by="word") 
head(text_words)


##### Die Sentimentanalyse ##### 
# Die Häufigkeit der Polaritäten
count(text_words, Polarität)
# Der Sentimentwert des Texts
sum(text_words$sentiment)
# der größte Einfluss der positiven & negativen Wörter
library(ggplot2)
text_words %>%
  group_by(word, Polarität) %>%
  summarise(contribution = sum(sentiment)) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = Polarität)) +
  geom_col(show.legend = FALSE) +
  labs(y = "Beitrag zum Gesamtscore",
       x = NULL) +
  coord_flip()



##### Der Text in Relation zu weiteren Texten ##### 
# Daten einlesen
text_long <- read.csv("~/Documents/Blog/Deutsche Textanalyse/Regierungserklärungen.csv", header=TRUE, stringsAsFactors=FALSE)
# Data Prep für alle Texte
texts_words <- text_long %>%
  mutate(text = gsub("[0-9]", "", text, fixed = F)) %>% # ohne Nummern
  mutate(text = gsub("[[:punct:]]", "", text, fixed = F)) %>% # ohne Satzzeichen
  mutate(text = tolower(text)) %>% #Kleinschreibung
  unnest_tokens(word, text) %>% # Zerlegung in einzelne Wörter
  group_by(date) %>%
  ungroup() %>%
  anti_join(get_stopwords(language = "de")) %>%
#  anti_join(stopwords_de2, by = "word") %>%
  inner_join(SentiWS_df, by="word") %>%
  mutate("date" = as.Date(date)) 
head(texts_words)
# Die Häufigkeit der Polaritäten im Verlauf
texts_words %>%
  group_by(date) %>%
  count(Polarität) %>%
  ggplot(aes(x=date, y=n, group=Polarität, color=Polarität)) +
  geom_line(size=1)+
  geom_point(size=2) +
  theme_minimal()
texts_words %>%
  group_by(date) %>%
  count(Polarität) %>%
  spread(Polarität, n, fill=0) %>%
  mutate(relation = positive/negative) %>%
  arrange(relation) %>%
  ggplot(aes(x=date, y=relation)) +
  geom_line(size=1, color='darkblue')+
  geom_point(size=2, color='darkblue') +
  theme_minimal()
# Der Sentimentwert im Verlauf
texts_words %>%
  group_by(date) %>%
  summarise(sentiment = sum(sentiment)) %>%
  ggplot(aes(x=date, y=sentiment)) +
  geom_smooth(size=0, se=TRUE, span=1) +  # span for amount of smoothing
  geom_line(size=1, color='darkblue')+
  geom_point(size=2, color='darkblue') +
  theme_minimal()
# Detaillierte Analyse - häufigsten Wörter 
unique(texts_words$date)
texts_words %>%
  filter(date == "2017-04-27") %>%
  group_by(Polarität) %>%
  count(word, sort=TRUE) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = Polarität)) +
  geom_col(show.legend = FALSE) +
  coord_flip()
# Detaillierte Analyse - beitragsstärkste Wörter 
texts_words %>%
  filter(date == "2017-09-03") %>%
  group_by(word, Polarität) %>%
  summarise(contribution = sum(sentiment)) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = Polarität)) +
  geom_col(show.legend = FALSE) +
  labs(y = "Beitrag zum Gesamtscore",
       x = NULL) +
  coord_flip()



