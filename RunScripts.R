# Set The workingDir to your project's path
workingDir = "C:/Users/chamek/Desktop/trend-analysis"
# Change the libpath if you already have an R library in a different location
libPath = paste(workingDir, "/Library", sep="", collapse=" ")
setwd(workingDir)
output_path = workingDir

source(paste(workingDir, "/Scripts/buildOptimalLDAModel.R", sep="", collapse=" "))

RdataPath = "C:/Users/chamek/Desktop/trend-analysis/data/STO_Iteration001_data.RData"
outputPath = "C:/Users/chamek/Desktop/trend-analysis"
my_stopwords = c(stopwords("english"), "software", "system", "systems", "paper", "can", "also", "ieee", "new", "one", "two", "using", "based", "number")

buildOptimalLDAModel(outputPath, RdataPath, my_stopwords)
