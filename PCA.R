##################################################
#
# PCA.R
#
# Executes PCA against the numeric training data
# creating a scree plot and a projection plot.
# The parameter "groupingColumn" colorizes the plot
# based on the values of that column.
#
# This script also defines the function "ProjectPC"
# that can be called through the PAX NINVOKE function
# to return a projection along a selected component
# that can be used in queries or as inputs to additional
# R scripts or predictive models
#
####################################################


# Load helper functions
source(file="PDXHelpers.R")

# the global PCA object 
MyGlobalPCA<-NULL
OriginalColCount<-0

# Declare the ProjectPC function to be used from NINVOKE
ProjectPC <- function(newData) 
{
 	# Clean input data
 	newData<-cleanNulls(newData[, names(newData) != groupingColumn])
	colNames<-names(newData)
 
	#hasArgument
	nPC <- 1
	if( length(colNames) > OriginalColCount )
		nPC<-newData[1, OriginalColCount+1]
	
	# Project, and select the parameter part of the result
  	predictions <- predict(MyGlobalPCA, newData)[,nPC:nPC]
}



# Load the main dataframe. Use the global InputData variable

# Clean the data for PCA processing
Group<-InputData[[groupingColumn]]
InputData<-cleanNulls(InputData[, names(InputData) != groupingColumn])
OriginalColCount<-length(names(InputData))
OriginalColNames<-names(InputData)
pcaFormula<-extractNumericColumns(InputData)



# Run PCA. Store result in a global variable
MyGlobalPCA<-prcomp( pcaFormula, center=TRUE, retx = TRUE, scale=TRUE, data=InputData)



#Extract Results
PCAResults<-summary(MyGlobalPCA)


#Generate plots
pkgTest("knitr")
pkgTest("devtools")
pkgTest("ggplot2")
pkgTestGithub("ggbiplot", "vqv")

variances <- data.frame(variances=MyGlobalPCA$sdev**2, pcomp=1:length(MyGlobalPCA$sdev))
varPlot <- ggplot(variances, aes(pcomp, variances)) + geom_bar(stat="identity", fill="gray") + geom_line()
pcaPlot <- ggbiplot(MyGlobalPCA,groups=Group,ellipse=F,circle=F,varname.size=5,
                    varname.adjust = 1.5)

#Generate Report
knit2html('PCA.Rmd')

# Clear values from memory that don't need to be archived
varPlot<-NULL
pcaPlot<-NULL
variances<-NULL
InputData<-NULL
MyGlobalPCA$x<-NULL
PCAResults$x<-NULL

# Return a result -- the summary of the PCA model
PCAResults