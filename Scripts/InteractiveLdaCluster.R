#install.packages("text2vec", dependencies=TRUE)
install.packages("LDAvis", dependencies=TRUE)
library("text2vec")
#library(text2vec)
library (tm)
library(magrittr)
library(text2vec)

# Loading the three sources of Data
load ("C:/Users/Chouaib/Desktop/FinalReport/Data/my_scopus_ci_data.RData")
scopus_articles = my_articles
load ("C:/Users/Chouaib/Desktop/FinalReport/Data/my_STO_ci_data.RData")
sto_articles = my_articles
load ("C:/Users/Chouaib/Desktop/FinalReport/Data/my_twitter_ci_data.RData")
twitter_articles = my_articles

source("grouping_script.R")
source("StopwordList.R")
stopword_list = final_stopword_list

create_LDA_Cluster <- function(my_articles){
  my_text <- paste (my_articles$Title, my_articles$Abstract_clean)
  removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
  my_tokens = my_text %>% removeNumbers %>% removeSpecialChars %>% tolower %>% grouping %>% word_tokenizer # = tolower(removePunctuation(removeNumbers(my_text)))
  it = itoken(my_tokens)
  vocab <- create_vocabulary(it, stopwords = c("m", "s", "t", stopword_list))
  #Limit vocabulary to 40000 terms. Each word is only allowed to appear in 50% of the documents
  vocab <- prune_vocabulary(vocab, term_count_max = 40000, doc_proportion_max = 0.5)
  #Create DTM
  dtm = create_dtm(it, vocab_vectorizer(vocab), type="dgTMatrix", 'lda_c')
  #LDA model with 20 topics to provide an overview
  lda_model = LDA$new(n_topics = 20)
  #We run 200 iteration
  doc_topic_distr = lda_model$fit_transform(dtm, n_iter =200, check_convergence_every_n = 5)
  # save for fater access later. 
  save (lda_model, file="lda_model_TSE")
  #load("lda_model_TSE")
  #install.packages("LDAvis", dependencies=TRUE)
  # run LDAvis visualisation if needed (make sure LDAvis package installed)
  #Be patient this might take while before anything is plotted. See viewer tap on the right
  lda_model$plot() 
}

# Scopus LDA cluster 
create_LDA_Cluster(scopus_articles)

# STO LDA cluster 
create_LDA_Cluster(sto_articles)

# Twitter LDA cluster 
create_LDA_Cluster(twitter_articles)
