rm(list = ls())
#install.packages("wordcloud", dependencies = TRUE)
library (tm)
library(magrittr)
library(wordcloud)
library(RColorBrewer)

# Loading the three sources of Data
load ("C:/Users/Chouaib/Desktop/FinalReport/Data/my_scopus_ci_data.RData")
scopus_articles = my_articles
load ("C:/Users/Chouaib/Desktop/FinalReport/Data/my_STO_ci_data.RData")
sto_articles = my_articles
load ("C:/Users/Chouaib/Desktop/FinalReport/Data/my_twitter_ci_data.RData")
twitter_articles = my_articles

source("StopwordList.R")
stopword_list = final_stopword_list

scopus_text <- paste (scopus_articles$Title, scopus_articles$Abstract_clean)
sto_text <- paste (sto_articles$Title, sto_articles$Abstract_clean)
twitter_text <- paste (twitter_articles$Title, twitter_articles$Abstract_clean)


removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)

scopus_text = scopus_text %>% removeNumbers %>% removeSpecialChars %>% tolower
sto_text = sto_text %>% removeNumbers %>% removeSpecialChars %>% tolower
twitter_text = twitter_text %>% removeNumbers %>% removeSpecialChars %>% tolower

scopus_text =  removeWords(scopus_text,stopword_list)
sto_text =  removeWords(sto_text,stopword_list)
twitter_text =  removeWords(twitter_text,stopword_list)

png("C:/Users/Chouaib/Desktop/FinalReport/Graphs/Scopus_word_cloud.png")
wordcloud(scopus_text, scale=c(8,.3), max.words = 100, random.order = FALSE, colors=brewer.pal(8,"Dark2"))
dev.off()

png("C:/Users/Chouaib/Desktop/FinalReport/Graphs/STO_word_cloud.png")
wordcloud(sto_text, scale=c(8,.3), max.words = 100, random.order = FALSE, colors=brewer.pal(9, "Dark2"))
dev.off()

png("C:/Users/Chouaib/Desktop/FinalReport/Graphs/Twitter_word_cloud.png")
wordcloud(twitter_text, scale=c(8,.3), max.words = 100, random.order = FALSE, colors=brewer.pal(9, "Dark2"))
dev.off()
