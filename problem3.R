# File: problem3.R
# Auther: AAGOOGLE
# 
# Created on May 6, 2017


myMean <- function(sample) {
  
  numberOfValues <- NROW(sample)
  
  sumOfValues = 0
  for (rowIndex in 1:numberOfValues) {
    value <- sample[rowIndex]
    sumOfValues <- sumOfValues + value
  }
  
  meanOfSample <- sumOfValues / numberOfValues
  
  return(meanOfSample)
}

myVariance <- function(feature) {
  
  numberOfValues <- NROW(feature)
  meanOfFeature <- myMean(feature)
  
  SumOfSquaresOfSubtractedValuesFromTheMean <- 0
  for (rowIndex in 1:numberOfValues) {
    value <- feature[rowIndex]
    
    subtractedValueFromTheMean <- value - meanOfFeature
    squareOfSubtractedValueFromTheMean <- subtractedValueFromTheMean * subtractedValueFromTheMean
    SumOfSquaresOfSubtractedValuesFromTheMean <- SumOfSquaresOfSubtractedValuesFromTheMean + squareOfSubtractedValueFromTheMean
  }
  
  varianceOfFeature = SumOfSquaresOfSubtractedValuesFromTheMean / numberOfValues
  return(varianceOfFeature)
}

myCovariance <- function(feature1, feature2) {
  
      numberOfValues = NROW(feature1)
  
      feature1Mean = myMean(feature1)
      feature2Mean = myMean(feature2)
      
      sum = 0
      for (rowIndex in 1:numberOfValues) {
        
        value1 = feature1[rowIndex]
        subtracted1 <- value1 - feature1Mean
        
        value2 = feature2[rowIndex]
        subtracted2 <- value2 - feature2Mean
        
        product = subtracted1 * subtracted2
        
        sum = sum + product
      }
      
      covarianceOfFeature1AndFeature2 <- sum / (numberOfValues - 1)
      
      return(covarianceOfFeature1AndFeature2)
      
}

myCorrelation <- function(feature1, feature2) {
  
  correlationCoefficient <- myCovariance(feature1, feature2) / (sqrt(myVariance(feature1)) * sqrt(myVariance(feature2)))
  
  return(correlationCoefficient)
}


############################################# MAIN ################################################################


# help("read.table")
## https://stat.ethz.ch/R-manual/R-devel/library/utils/html/read.table.html

## Reads a file in table format and creates a data frame from it.
## With cases corresponding to lines and variables to fields in the file.

## It uses a delimiter/separator (the default value is whitespace sep = "") to seprate each column entry.
## It uses each line breaks (new line) as an indication for a new row entry.

dataSet <- read.table("problem3_appendix.dat") # Read "problem3.dat" text file into DataSet variable.

## help("as.matrix")
## https://stat.ethz.ch/R-manual/R-devel/library/base/html/matrix.html

## Attempts to turn its argument into a matrix (numric vectors).
dataSetAsMatrix <- as.matrix(dataSet) # convert the dataSet into numric vectors


# help("matrix")
## https://stat.ethz.ch/R-manual/R-devel/library/base/html/matrix.html

## creates a matrix from the given set of values.

## data: An optional data vector (including a list or expression vector).
## nrow: The desired number of rows.
## ncol: The desired number of columns.
## byrow: logical. If FALSE (the default) the matrix is filled by columns, otherwise the matrix is filled by rows.

## Note: every 300 consecutive readings from the file represent one row in X.
### So we have to enable the read byrow option. (Matrix is filled by rows first).

## Note: problem3.dat contains a 5 different features and 300 different samples for each feature.
### So we will build a 5x300 matrix. (rows = features = 5 , columns = samples = 300)

trainingSetMatrix = matrix(data = dataSetAsMatrix, nrow = 5, ncol = 300, byrow = TRUE)
numberOfSamples = ncol(trainingSetMatrix)
numberOfFeatures = nrow(trainingSetMatrix)

layoutMatrix <- rbind(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10), c(11, 12, 13, 14, 15),
           c(16, 17, 18, 19, 20), c(21, 22, 23, 24, 25))
layout(mat = layoutMatrix)

# par(mar = c(3, 3, 0, 0), oma = c(3, 3, 3, 3))

highestCorrelation <- 0
for (i in 1 : (numberOfFeatures)) {
  
  Feature1 = trainingSetMatrix[i, ]
  
  for (j in 1 : (numberOfFeatures)) {
    
    if (i == j) {
      plot(0, type='n', axes = FALSE, ann = FALSE)
      next
    }
    Feature2 = trainingSetMatrix[j, ]
    
    plot(Feature2, Feature1, xlab = paste("Feature", j), ylab = paste("Feature", i), las = 1, col="blue")

    correlationCoefficient <- myCorrelation(Feature2, Feature1)
    
    if (abs(correlationCoefficient) > highestCorrelation) {
      highestCorrelation <- abs(correlationCoefficient)
      linePair <-  c(paste("Feature", j), paste("Feature", i)) 
    }
    
    print(paste("myCorrelation(Feature", j, ", Feature", i, ") = ", correlationCoefficient))
    
  }
}

# Which pair, of the 10 pairs, can be represented by a line?
## Is the pair that has the highest correlation coefficient rho magnitude

print(paste("Highest Correlation = ", highestCorrelation))
print(paste("Best pair to represent a line is ", linePair[1], "and", linePair[2]))