########################################
#
# Predixion R Helper functions
#
########################################

# General purpose data cleanup function
cleanNulls<-function(newData){
  colNames<-names(newData)
  
  #replace nulls
  for( i in 1:length(names(newData)) )
  {
    colName<-colNames[i]
    if( is.numeric(newData[[colName]])  )
    {
      newData[[colName]][is.na(newData[[colName]])]<-0
    }
  }
  # return the clean d.f
  newData
}

#function to test and install required library
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE,repos='http://cran.us.r-project.org')
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

pkgTestGithub <- function(x,y)
{
  if (!require(x,character.only = TRUE))
  {
    install_github(x,y)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# function to remove non-numeric columns 
extractNumericColumns<-function(DF){
  
  # Detect numeric columns, compute the PCA formula
  formulaStr <- paste("~")
  colNames<-names(DF)
  
  first<-TRUE
  for( i in 1:length(colNames) )
  {
    colName<-colNames[i]
    if( is.numeric(DF[[colName]])  )
    {
      if( !is.na(mean(DF[[colName]])) && (0 != sd(DF[[colName]])) )
      {
        sep <- "+"
        if( first )
        {
          sep <- " "
          first<-FALSE
        }
        formulaStr<-paste(formulaStr, sep, colNames[i])
      }
    }
  }
  
  # return the string as.formula
  as.formula(formulaStr)
}
