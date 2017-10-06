if (libPath %in% .libPaths() == FALSE)
{
  .libPaths(c(.libPaths(),libPath))
}
if("topicmodels" %in% rownames(installed.packages()) == FALSE) 
{ 
  install.packages("topicmodels", dependencies = TRUE, lib = libPath)
}

#Loading dependencies
library(tm)
library(magrittr)# Takes about 10s
library(slam)
library(topicmodels)

buildOptimalLDAModel = function (outputPath, RdataPath, stopWordsList) {
  
  #Load the data object 
  load (RdataPath)
  
  #Create the directory for the output data for analysis
  ouputAnalysis_dir = paste(outputPath,"/Analysis Output/",sep="")
  dir.create(ouputAnalysis_dir, showWarnings = TRUE, recursive = TRUE, mode = "0777")
  
  #Articles with NA dates cause false analysis 
  #later kick them out
  my_articles <- my_articles[which(!is.na(my_articles$Date)),]
  my_text <- paste (my_articles$Title, my_articles$Abstract_clean)
  
  removeSpecialChars <- function(x) gsub("[^a-zA-Z ]","",x)
  my_text <- removeSpecialChars(my_text)
  my_text <- removeWords(my_text, stopWordsList)
  
  #Terms must be more than 2 characters and appear 
  # in 3 or more times
  dtm <- my_text %>% VectorSource %>% Corpus %>% DocumentTermMatrix (.,  control=list(wordLengths = c(3, Inf), bounds=list(global = c(3,Inf)))) 
  
  #Clean up infrequent and meaningless terms TF/IDF------------------------------------
  #source: http://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf
  #Own experience: LDA is much faster 1.3s/iter -> 0.3s/iter or 0.2s/iter
  dim(dtm)#3457 documents and 6282 words
  
  #Word frequencies (col_sums) & Text lengths (text_length). 
  #Note previously we required min 3 
  #occurences of each word
  summary(col_sums(dtm))
  summary(row_sums(dtm))
  
  #compute tf_idf https://en.wikipedia.org/wiki/Tf%E2%80%93idf
  #TF How frequent a term is in a document diveded by document lenght
  #IDF How meaningfull a word is, i.e. common or rare a cross document
  # A high weight in tf-idf is reached by a high term frequency 
  #(in the given document) and a low document frequency of the 
  # term in the whole collection of documents
  term_tfidf <-tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
  #Lets kickout half of the terms that are less important 
  #(according to tf_idf), bit less than median
  dtm <- dtm[,term_tfidf >= median(term_tfidf)] 
  #Re-investigate dtm with 
  dim(dtm)
  # We notece that half of the terms are gone. 
  
  #--Remove documents that no have zero terms, i.e. they are made up of only meaningless general terms------------------------------
  zero_term_docs = row_sums(dtm) > 0
  dtm <- dtm[zero_term_docs,]
  my_articles2 <- my_articles[zero_term_docs,]
    
  #Correct number of topics---------------------------------------------------------
  #Other methods also exists. Once we have are from these sources
  #http://stackoverflow.com/questions/21355156/topic-models-cross-validation-with-loglikelihood-or-perplexity/21394092#21394092
  #http://epub.wu.ac.at/3558/1/main.pdf
  #------------How many topics are we going to need
  #Note the number of burnin iter and thin are smaller 
  #as we want the demo to go fast. 
  # Perform some iterations without samples (burnin)
  burnin = 100 #GS04 uses 800 or 1100 depdending on number of topics
  #burnin=1000
  iter = 100 #defaults to 2000. 1000 to make it go faster 
  #iter=1000
  thin = 10 # As with other MCMC algorithms, Gibbs sampling generates a Markov chain of samples, each of which is correlated with nearby samples. As a result, care must be taken if independent samples are desired (typically by thinning the resulting chain of samples by only taking every nth value, e.g. every 100th value). https://en.wikipedia.org/wiki/Gibbs_sampling
  #thin=100
  
  #sequ <- seq(20, 520, 100)
  sequ <- seq(50, 150, 10)
  sequ <- seq(80, 100, 1)
  #First line for testing with small data
  #Notice we subset the dtm dtm[21:200,]
  
  system.time(fitted_many <- lapply(sequ, function(k) LDA(dtm[21:121,], k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, thin=thin))))
  #82 seconds  
  
  #Use multi-core lapply (mclapply). Might make you machine freeze. 
  # But faster in overnight runs
  #system.time(fitted_many <- mclapply(sequ, function(k) LDA(dtm, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, thin=thin))))
  
  #system.time(fitted_many <- lapply(sequ, function(k) LDA(dtm, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, thin=thin))))
  #1100 seconds
  fitted_many_file = paste (ouputAnalysis_dir, 'LDAModelScript_fitted_many.RData')
  save(fitted_many, file=fitted_many_file)
  #load ("fitted_many_saved_for_demo.RData")
  #hm_many <- lapply(fitted_many, function(L)  L@loglikelihood)
  hm_many <- unlist(lapply(fitted_many, function(L)  L@loglikelihood))
  
  #Compute harmonic means
  plot(sequ, hm_many, type = "l")
  
  harmonic_means_file = paste (ouputAnalysis_dir, 'LDAModelScript_harmonic_means.png')
  png(filename=harmonic_means_file)
  plot(sequ, hm_many, type = "l")
  dev.off()
  
  #compute optimum number of topics
  opt_topics = sequ[which.max(hm_many)]
  sequ[order(hm_many)]
  sort(hm_many)
  
  #************************************************
  # Return to run another seq with different values 
  #************************************************
  
  #Once ready...
  #Lets pick the winner
  LDAWinner <- fitted_many[[which.max(hm_many)]]
  
  LDAWinner_file = paste (ouputAnalysis_dir, 'LDAModelScript_LDAWinner.RData')
  #You also want to save your hard computing efforts
  save(LDAWinner, file=LDAWinner_file)
}
