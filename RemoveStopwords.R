##################################################
#
# RemoveStopwords.R
#
# Executes RemoveStopwords against the documents within a corpus
# This script uses default english stopwords list.
#
# This script also defines the function "ProjectRS"
# that can be called through the PAX NINVOKE function
# to return a projection along a selected component
# that can be used in queries or as inputs to additional
# R scripts or predictive models
#
####################################################


## the Global RS object
MyGlobalRS<-NULL

## Get tm package
reposURL = "http://cran.cs.wwu.edu/"
if (!require("tm")) {install.packages("tm", dep = TRUE, repos=reposURL)}
library(tm)

## Function to be used in workbench to import result
## Package specific functino
ProjectRS <- function(InputData) 
{
  reposURL = "http://cran.cs.wwu.edu/"
  if (!require("tm")) {install.packages("tm", dep = TRUE, repos=reposURL)}
  library(tm)
  
  colNames<-names(InputData)
  ## data format convert
  dataframe <- as.data.frame(InputData)  
  datasource <- DataframeSource(dataframe)
  ## make input data into VCorpus, which is required for tm package
  corp <- Corpus(datasource)
  
  ## In tm, most functionalities are subsumed into the concept of a transformation. Transformations are
  ##done via the tm_map() function which applies (maps) a function to all elements of the corpus. Basically, all
  ##transformations work on single text documents and tm_map() just applies them to all documents in a corpus.
  
  ## convert all text into lower case
  modcorp <- tm_map(corp, content_transformer(tolower))
  ## remove stopwords, the stopwords list contains 100 common english stopwords
  modcorp <- tm_map(modcorp, removeWords, stopwords("english"))
  ## convert result into dataframe to be compatible with PAX NINVOKE function call
  result<-data.frame(text=unlist(sapply(modcorp, `[`, "content")),stringsAsFactors=F)
  return (result)
}

result <- ProjectRS(InputData)

result


