# Set The workingDir to your project's path
workingDir = "C:/Users/chamek/Desktop/trend-analysis"
# Change the libpath if you already have an R library in a different location
libPath = paste(workingDir, "/Library", sep="", collapse=" ")
setwd(workingDir)
output_path = workingDir
#Output files are in the format: './data/<twitter|STO|scopus>_<output_filename>_data.RData'
output_filename = "Iteration001"
get_tweets_java_path = paste(workingDir, "/GetOldTweets-java-master/", sep="", collapse=" ")

################################# SEARCH STRINGS #################################
query_string = 'Microservices'
scopus_query_string = paste('TITLE-ABS-KEY("',query_string,'")',sep="")
################################# SEARCH STRINGS #################################
  
source(paste(workingDir, "/Helpers/GetStackOverflowData.R", sep="", collapse=" "))
source(paste(workingDir, "/Helpers/GetTwitterData.R", sep="", collapse=" "))
source(paste(workingDir, "/Helpers/GetScopusData.R", sep="", collapse=" "))

#data = get_stackOverFlowData(output_path, query_string, output_filename)
#data = get_MyTwitterData(output_path, query_string, output_filename, get_tweets_java_path)

#To get the scopus data:
#  1 - Update API key in ./Helpers/Set_MyScopus_APIKey.R
#  2 - Make sure you are connected to a network that allows data retrival from Scopus.
data = get_ScopusData(output_path, scopus_query_string, output_filename)



