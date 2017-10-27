rm(list = ls())

# Loading the three sources of Data
load ("C:/Users/Chouaib/Desktop/FinalReport/Data/my_scopus_ci_data.RData")
scopus_articles = my_articles
load ("C:/Users/Chouaib/Desktop/FinalReport/Data/my_STO_ci_data.RData")
sto_articles = my_articles
load ("C:/Users/Chouaib/Desktop/FinalReport/Data/my_twitter_ci_data.RData")
twitter_articles = my_articles

source("StopwordList.R")
stopword_list = final_stopword_list


# remove articles without a date
scopus_articles_2 <- scopus_articles[which(!is.na(scopus_articles$Date)),]
sto_articles_2 <- sto_articles[which(!is.na(sto_articles$Date)),]
twitter_articles_2 <- twitter_articles[which(!is.na(twitter_articles$Date)),]


#split from the middle NA's couse trouble so we need [which(!is.na(my_articles$Date))]
#scopus
scopus_new_titles <- scopus_articles_2$Title[scopus_articles_2$Date > median(scopus_articles_2$Date)]
scopus_old_titles <- scopus_articles_2$Title[scopus_articles_2$Date <= median(scopus_articles_2$Date)]
scopus_all_titles <- c(paste(scopus_new_titles, collapse=" "), paste(scopus_old_titles, collapse=" "))
#sto
sto_new_titles <- sto_articles_2$Title[sto_articles_2$Date > median(sto_articles_2$Date)]
sto_old_titles <- sto_articles_2$Title[sto_articles_2$Date <= median(sto_articles_2$Date)]
sto_all_titles <- c(paste(sto_new_titles, collapse=" "), paste(sto_old_titles, collapse=" "))
#twitter
twitter_new_titles <- twitter_articles_2$Title[twitter_articles_2$Date > median(twitter_articles_2$Date)]
twitter_old_titles <- twitter_articles_2$Title[twitter_articles_2$Date <= median(twitter_articles_2$Date)]
twitter_all_titles <- c(paste(twitter_new_titles, collapse=" "), paste(twitter_old_titles, collapse=" "))

#removing special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
scopus_all_titles <- removeSpecialChars(scopus_all_titles)
sto_all_titles <- removeSpecialChars(sto_all_titles)
twitter_all_titles <- removeSpecialChars(twitter_all_titles)

#removing stop words
scopus_all_titles <- removeWords(scopus_all_titles, stopword_list)
sto_all_titles <- removeWords(sto_all_titles, stopword_list)
twitter_all_titles <- removeWords(twitter_all_titles, stopword_list)

#Creating the TDM matrices
scopus_tdm <- scopus_all_titles %>% VectorSource %>% Corpus %>% TermDocumentMatrix %>% as.matrix
sto_tdm <- sto_all_titles %>% VectorSource %>% Corpus %>% TermDocumentMatrix %>% as.matrix
twitter_tdm <- twitter_all_titles %>% VectorSource %>% Corpus %>% TermDocumentMatrix %>% as.matrix

# Rename columnes
colnames(scopus_tdm) <- c (paste (">", as.character(median(scopus_articles_2$Date))), 
                    paste ("<=", as.character(median(scopus_articles_2$Date))))

colnames(sto_tdm) <- c (paste (">", as.character(median(sto_articles_2$Date))), 
                           paste ("<=", as.character(median(sto_articles_2$Date))))

colnames(twitter_tdm) <- c (paste (">", as.character(median(twitter_articles_2$Date))), 
                           paste ("<=", as.character(median(twitter_articles_2$Date))))
 
png("C:/Users/Chouaib/Desktop/FinalReport/Graphs/Scopus_DatesComparison_wordCloud.png")
comparison.cloud(scopus_tdm, scale=c(8,.3), max.words = 100, colors=brewer.pal(8,"Dark2"))
dev.off()

png("C:/Users/Chouaib/Desktop/FinalReport/Graphs/STO_DatesComparison_wordCloud.png")
comparison.cloud(sto_tdm, max.words = 100, colors=brewer.pal(9, "Dark2"))
dev.off()

png("C:/Users/Chouaib/Desktop/FinalReport/Graphs/Twitter_DatesComparison_wordCloud.png")
comparison.cloud(twitter_tdm, scale=c(8,.3), max.words = 100, colors=brewer.pal(9, "Dark2"))
dev.off()



#---------------------------------------------- Comparison Cloud for citations
#The "quantiles" are not divided to [0.25, 0.5, 0.75, 1] because of the data dispersion
#Most of the data is squeezed in the values 0 and 1 (most of the data has a low citation)
#This causes that some quadrants are empty. I had to adjust the probability to include 
#the next integer citation value

#SCOPUS
q1_t <- scopus_articles$Title[scopus_articles$Cites <= quantile(scopus_articles$Cites, probs = 0.25)]
q2_t <- scopus_articles$Title[scopus_articles$Cites > quantile(scopus_articles$Cites, probs = 0.25) &
                                scopus_articles$Cites <= quantile(scopus_articles$Cites, probs = 0.57)]
q3_t <- scopus_articles$Title[scopus_articles$Cites > quantile(scopus_articles$Cites, probs = 0.57) &
                                scopus_articles$Cites <= quantile(scopus_articles$Cites, probs = 0.75)]
q4_t <- scopus_articles$Title[scopus_articles$Cites > quantile(scopus_articles$Cites, probs = 0.75)]

scopus_all_titles <- c(paste(q1_t, collapse=" "), 
                  paste(q2_t, collapse=" "),
                  paste(q3_t, collapse=" "), 
                  paste(q4_t, collapse=" "))

removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
scopus_all_titles <- removeSpecialChars(scopus_all_titles)
scopus_all_titles <- removeWords(scopus_all_titles, stopword_list)

scopus_tdm <- scopus_all_titles %>% VectorSource %>% Corpus %>% TermDocumentMatrix %>% as.matrix
colnames(scopus_tdm) <- c ("Q1 <= 0", "0 < Q2 <= 1", "1 < Q3 <= 2", "2 < Q4")

png("C:/Users/Chouaib/Desktop/FinalReport/Graphs/Scopus_CitationsComparison_wordCloud.png")
comparison.cloud(scopus_tdm, max.words=100, title.size=1.5)
dev.off()


#STO
q1_t <- sto_articles$Title[sto_articles$Cites <= quantile(sto_articles$Cites, probs = 0.25)]
q2_t <- sto_articles$Title[sto_articles$Cites > quantile(sto_articles$Cites, probs = 0.25) &
                             sto_articles$Cites <= quantile(sto_articles$Cites, probs = 0.5)]
q3_t <- sto_articles$Title[sto_articles$Cites > quantile(sto_articles$Cites, probs = 0.5) &
                             sto_articles$Cites <= quantile(sto_articles$Cites, probs = 0.78)]
q4_t <- sto_articles$Title[sto_articles$Cites > quantile(sto_articles$Cites, probs = 0.75)]

sto_all_titles <- c(paste(q1_t, collapse=" "), 
                    paste(q2_t, collapse=" "),
                    paste(q3_t, collapse=" "), 
                    paste(q4_t, collapse=" "))

removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
sto_all_titles <- removeSpecialChars(sto_all_titles)
sto_all_titles <- removeWords(sto_all_titles, stopword_list)

sto_tdm <- sto_all_titles %>% VectorSource %>% Corpus %>% TermDocumentMatrix %>% as.matrix
colnames(sto_tdm) <- c ("Q1 <= 0", "0 < Q2 <= 1", "1 < Q3 <= 2", "2 < Q4")

png("C:/Users/Chouaib/Desktop/FinalReport/Graphs/STO_CitationsComparison_wordCloud.png")
comparison.cloud(sto_tdm, na.rm = TRUE, max.words=80, title.size=1.5)
dev.off()


#TWITTER  
q1_t <- twitter_articles$Title[twitter_articles$Cites <= quantile(twitter_articles$Cites, probs = 0.25)]
q2_t <- twitter_articles$Title[twitter_articles$Cites > quantile(twitter_articles$Cites, probs = 0.25) &
                                twitter_articles$Cites <= quantile(twitter_articles$Cites, probs = 0.51)]
q3_t <- twitter_articles$Title[twitter_articles$Cites > quantile(twitter_articles$Cites, probs = 0.51) &
                                twitter_articles$Cites <= quantile(twitter_articles$Cites, probs = 0.75)]
q4_t <- twitter_articles$Title[twitter_articles$Cites > quantile(twitter_articles$Cites, probs = 0.75)]

twitter_all_titles <- c(paste(q1_t, collapse=" "), 
                       paste(q2_t, collapse=" "),
                       paste(q3_t, collapse=" "), 
                       paste(q4_t, collapse=" "))

removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
twitter_all_titles <- removeSpecialChars(twitter_all_titles)
twitter_all_titles <- removeWords(twitter_all_titles, stopword_list)

twitter_tdm <- twitter_all_titles %>% VectorSource %>% Corpus %>% TermDocumentMatrix %>% as.matrix
colnames(twitter_tdm) <- c ("Q1 <= 0", "0 < Q2 <= 1", "1 < Q3 <= 2", "2 < Q4")

png("C:/Users/Chouaib/Desktop/FinalReport/Graphs/Twitter_CitationsComparison_wordCloud.png")
comparison.cloud(twitter_tdm, max.words=100, title.size=1.5)
dev.off()