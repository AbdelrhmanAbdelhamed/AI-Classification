# File: problem2.R
# Auther: AAGOOGLE
# 
# Created on May 5, 2017


myMean <- function(sample) {
  ## Add up all the numbers, then divide by how many numbers there are.
  
  ### How many numbers there are?
  
  #### help("NROW")
  ##### https://stat.ethz.ch/R-manual/R-devel/library/base/html/nrow.html
  
  ##### nrow and ncol return the number of rows or columns present in x.
  ##### NCOL and NROW do the same. ((( treating a vector as 1-column matrix ))).
  
  numberOfValues <- NROW(sample) # calculate number of rows (values/entries/elements) from sample (vector) variable into numberOfValues variable.
  
  ### Add up all the numbers.(summation).
  
  ### 1:numberOfValues is An integer sequence from 1 to numberofValues.
  ### rowIndex is a variable that iterates the sequence from 1 to numberofValues. (Row number).
  
  sumOfValues = 0
  for (rowIndex in 1:numberOfValues) {
    value <- sample[rowIndex] # each value/entry/element of the sample
    sumOfValues <- sumOfValues + value
  }
  
  ## Finally calculate the mean.
  meanOfSample <- sumOfValues / numberOfValues
  
  return(meanOfSample)
}


############################################# MAIN ################################################################

 
# The covariance of two variables (matrix/vector/sample) x and y in a data set measures how the two are linearly related.
# A positive covariance would indicate a positive linear relationship between the variables,
# and a negative covariance would indicate the opposite.


# help("read.table")
## https://stat.ethz.ch/R-manual/R-devel/library/utils/html/read.table.html

## Reads a file in table format and creates a data frame from it.
## With cases corresponding to lines and variables to fields in the file.

## It uses a delimiter/separator (the default value is whitespace sep = "") to seprate each column entry.
## It uses each line breaks (new line) as an indication for a new row entry.

dataSet <- read.table("problem2.dat") # Read "problem2.dat" text file into DataSet variable.

## help("as.matrix")
## https://stat.ethz.ch/R-manual/R-devel/library/base/html/matrix.html

## Attempts to turn its argument into a matrix (numric vectors).
dataSetAsMatrix <- as.matrix(dataSet ) # convert the dataSet into numric vectors


# help("matrix")
## https://stat.ethz.ch/R-manual/R-devel/library/base/html/matrix.html

## creates a matrix from the given set of values.

## data: An optional data vector (including a list or expression vector).
## nrow: The desired number of rows.
## ncol: The desired number of columns.
## byrow: logical. If FALSE (the default) the matrix is filled by columns, otherwise the matrix is filled by rows.

## Note: every 300 consecutive readings from the file represent one row in X.
### So we have to enable the read byrow option. (Matrix is filled by rows first).

## Note: problem2.dat contains a 20 different features and 300 different samples for each feature.
### So we will build a 20x300 matrix. (rows = features = 20 , columns = samples = 300)

trainingSetMatrix = matrix(data = dataSetAsMatrix, nrow = 20, ncol = 300, byrow = TRUE)
numberOfFeatures = nrow(trainingSetMatrix)
numberOfSamples = ncol(trainingSetMatrix)

## The number of features is 20 So, the size of the covariance matrix will be (number of features)^2 = 20 x 20 = 400 elements.

## We will ended up with 20 rows and 20 columns.
### Because we are going to calcualte the covariance of every feature with each other.
### Including the feature itself also known as the variance of the feature.
#### The variances appear along the diagonal and covariances appear in the off-diagonal elements.
#### That's why it is sometimes called "The Variance-Covariance Matrix".

##Example: The Variance-Covariance Matrix looks like this:

### covariance(Feature1 and Feature1) #which is equal to variance(Feature1)#, covariance(Feature1 and Feature2), covariance(Feature1 and Feature3) ... covariance(Feature1 and Feature20)
### covariance(Feature2 and Feature1), covariance(Feature2 and Feature2) #which is equal to variance(Feature2)#, covariance(Feature2 and Feature3) ... covariance(Feature2 and Feature20)
### covariance(Feature3 and Feature1), covariance(Feature3 and Feature2), covariance(Feature3 and Feature3) #which is equal to variance(Feature3)# ... covariance(Feature3 and Feature20)
### .                                .                                .                                                                         .
### .                                .                                .                                                                         .
### .                                .                                .                                                                         .
### covariance(Feature20 and Feature1), covariance(Feature20 and Feature2), covariance(Feature20 and Feature3) ... covariance(Feature20 and Feature20) #which is equal to variance(Feature20)#


covarianceMatrixOfTrainingSetByStudent = matrix(nrow = 20, ncol = 20) # build a 20x20 matrix to hold the values (As the shape of the above example).

  for (i in 1:numberOfFeatures) {
    
    feature1 = trainingSetMatrix[i, ]
    feature1Mean = myMean(feature1)
    # Sample1Mean = mean(feature1)
    
    for (j in 1:numberOfFeatures) {
      
      feature2 = trainingSetMatrix[j, ]
      feature2Mean = myMean(feature2)
      # Sample2Mean = mean(feature2)
      
      sum = 0
      for (k in 1:numberOfSamples) {
        
        value1 = feature1[k]
        subtracted1 <- value1 - feature1Mean
        
        value2 = feature2[k]
        subtracted2 <- value2 - feature2Mean
        
        product = subtracted1 * subtracted2
        
        sum = sum + product
      }
      
      covarianceMatrixOfTrainingSetByStudent[i, j] = sum / (numberOfSamples - 1)
      
    }
  }

covarianceMatrixOfTrainingSetByRLanguage <- cov(t(trainingSetMatrix))
