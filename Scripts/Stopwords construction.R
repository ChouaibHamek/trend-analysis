library(tm)

##############  Constructing the corpus for sto, STO, and TWITTER

# Loading the three sources of Data
load ("C:/Users/Chouaib/Desktop/FinalReport/Data/my_scopus_ci_data.RData")
scopus_articles = my_articles
load ("C:/Users/Chouaib/Desktop/FinalReport/Data/my_STO_ci_data.RData")
sto_articles = my_articles
load ("C:/Users/Chouaib/Desktop/FinalReport/Data/my_twitter_ci_data.RData")
twitter_articles = my_articles

#Summary of the data
summary(scopus_articles$Cites)
summary(scopus_articles$Date)
summary(sto_articles$Cites)
summary(sto_articles$Date)
summary(twitter_articles$Cites)
summary(twitter_articles$Date)

# remove NA dates and Abstracts, remove special characters, and construct the corpus
#SCOPUS
scopus_articles_2 <- scopus_articles[which(!is.na(scopus_articles$Abstract_clean)),]
scopus_articles_2 <- scopus_articles_2[which(!is.na(scopus_articles$Title)),]
scopus_title_abstract <- c ( paste (scopus_articles_2$Title, collapse=" "), paste (scopus_articles_2$Abstract_clean, collapse=" "))
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
scopus_title_abstract <- removeSpecialChars(scopus_title_abstract)
scopus_corpus <- Corpus(VectorSource(scopus_title_abstract))

#STO
sto_articles_2 <- sto_articles[which(!is.na(sto_articles$Abstract_clean)),]
sto_articles_2 <- sto_articles_2[which(!is.na(sto_articles$Title)),]
sto_title_abstract <- c ( paste (sto_articles_2$Title, collapse=" "), paste (sto_articles_2$Abstract_clean, collapse=" "))
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
sto_title_abstract <- removeSpecialChars(sto_title_abstract)
sto_corpus <- Corpus(VectorSource(sto_title_abstract))

#TWITTER
twitter_articles_2 <- twitter_articles[which(!is.na(twitter_articles$Abstract_clean)),]
twitter_articles_2 <- twitter_articles_2[which(!is.na(twitter_articles$Title)),]
twitter_title_abstract <- c ( paste (twitter_articles_2$Title, collapse=" "), paste (twitter_articles_2$Abstract_clean, collapse=" "))
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
twitter_title_abstract <- removeSpecialChars(twitter_title_abstract)
twitter_corpus <- Corpus(VectorSource(twitter_title_abstract))

# defining the function that applies the stopWords and update the global TDMs
apply_stopwords <- function(stopWordList) {
  #SCOPUS
  scopus_dtm <- TermDocumentMatrix(scopus_corpus, control = list(tolower=TRUE, stemming = FALSE, stopwords = stopWordList,
                                                                 removeNumbers = TRUE, removePunctuation = TRUE))
  scopus_tdm_matrix <<- as.matrix(scopus_dtm)
  scopus_tdm_matrix <<- cbind(Word_count=apply(scopus_tdm_matrix[,],1,sum))
  
  #sto
  sto_dtm <- TermDocumentMatrix(sto_corpus, control = list(tolower=TRUE, stemming = FALSE, stopwords = stopWordList,
                                                           removeNumbers = TRUE, removePunctuation = TRUE))
  sto_tdm_matrix <<- as.matrix(sto_dtm)
  sto_tdm_matrix <<- cbind(Word_count=apply(sto_tdm_matrix[,],1,sum))
  
  #TWITTER
  twitter_dtm <- TermDocumentMatrix(twitter_corpus, control = list(tolower=TRUE, stemming = FALSE, stopwords = stopWordList,
                                                                   removeNumbers = TRUE, removePunctuation = TRUE))
  twitter_tdm_matrix <<- as.matrix(twitter_dtm)
  twitter_tdm_matrix <<- cbind(Word_count=apply(twitter_tdm_matrix[,],1,sum)) 
}

############## First iteration

stopword_list = c(stopwords("english") )
apply_stopwords(stopword_list)

############## Second iteration
stopword_list = c(stopwords("english"), "microservice", "microservices", "service", "services", "use", "data", 
                  "application", "system", "systems", "architecture", "applications", "paper", "can", "based", 
                  "approach", "provide", "based", "using", "software", "twitter", "com", "architecture", "via", 
                  "new", "pic", "need", "want", "like", "one", "will", "development")
apply_stopwords(stopword_list)

############## Third iteration
stopword_list = c(stopwords("english"), "microservice", "microservices", "service", "services", "use", "data", 
                  "application", "system", "systems", "architecture", "applications", "paper", "can", "based", 
                  "approach", "provide", "based", "using", "software", "twitter", "com", "architecture", "via", 
                  "new", "pic", "need", "want", "like", "one", "will", "development", "model", "models", "also", 
                  "architectures", "used", "present", "environment", "way", "app", "get", "project", "request", 
                  "different", "code", "also", "create", "great")
apply_stopwords(stopword_list)

############## Fourth iteration
stopword_list = c(stopwords("english"), "microservice", "microservices", "service", "services", "use", "data", 
                  "application", "system", "systems", "architecture", "applications", "paper", "can", "based", 
                  "approach", "provide", "based", "using", "software", "twitter", "com", "architecture", "via", 
                  "new", "pic", "need", "want", "like", "one", "will", "development", "model", "models", "also", 
                  "architectures", "used", "present", "environment", "way", "app", "get", "project", "request", 
                  "different", "code", "also", "create", "great", "however", "run", "running", "multiple", "know", 
                  "now", "trying", "example", "micro", "two", "just", "file", "call", "make", "access", "ive", 
                  "another", "best", "question", "message", "don't", "dont", "build", "work", "thanks", "following",
                  "implementation", "enabling", "towards", "basic", "event", "projects", "check", "part", "aware")
apply_stopwords(final_stopword_list)
