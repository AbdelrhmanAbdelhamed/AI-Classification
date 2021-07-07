# File: problem1.R
# Auther: AAGOOGLE
# 
# Created on May 4, 2017


# help("read.table")
## https://stat.ethz.ch/R-manual/R-devel/library/utils/html/read.table.html

## Reads a file in table format and creates a data frame from it.
## With cases corresponding to lines and variables to fields in the file.

## It uses a delimiter/separator (the default value is whitespace sep = "") to seprate each column entry.
## It uses each line breaks (new line) as an indication for a new row entry.

## col.names = a vector of optional names for the variables. The default is to use "V" followed by the column number.

dataSet <- read.table("problem1.dat") # Read "problem1.dat" text file into dataSet variable.


# Find the mean
## Add up all the numbers, then divide by how many numbers there are.

### How many numbers there are?

#### help("nrow")
##### https://stat.ethz.ch/R-manual/R-devel/library/base/html/nrow.html

##### nrow and ncol return the number of rows or columns present in dataSet.
##### nrow in this case returns 10000L. (L specifies an integer type, rather than a double that the standard numeric class)

numberOfValues <- nrow(dataSet) # calculate number of rows (values/entries/elements) from dataSet variable into numberOfValues variable.

### Add up all the numbers.(summation).

### 1:numberOfValues is An integer sequence from 1 to numberofValues.
### rowIndex is a variable that iterates the sequence from 1 to numberofValues. (Row number).
#### colnames(dataSet) = "V1" is the name of the column/vector (problem1.dat contains one column of data)

sumOfValues <- 0
for (rowIndex in 1:numberOfValues) {
  value <- dataSet[rowIndex, colnames(dataSet)] # each value/entry/element of the dataSet
  sumOfValues <- sumOfValues + value
}

## Finally calculate the mean.
meanOfDataSetByStudent <- sumOfValues / numberOfValues


## help("as.matrix")
## https://stat.ethz.ch/R-manual/R-devel/library/base/html/matrix.html

## Attempts to turn its argument into a matrix (numric vectors).
dataSetAsMatrix <- as.matrix(dataSet) # convert the dataSet into numric vectors

meanOfDataSetByRLanguange <- mean(dataSetAsMatrix) # calculate the mean of the dataSetAsMatrix into meanOfDataSetByRLanguange variable

print(paste("The mean my student method = ", meanOfDataSetByStudent))
print(paste("The mean my R language method = ", meanOfDataSetByRLanguange))


# Find the variance
## The variance is a measure of how far each value in the data set is from the mean. Here is how it is defined:

### Subtract the mean from each value in the data. This gives you a measure of the distance of each value from the mean.
### Square each of these distances (so that they are all positive values), and add all of the squares together.
### Divide the sum of the squares by the number of values in the data set.

SumOfSquaresOfSubtractedValuesFromTheMean <- 0
for (rowIndex in 1:numberOfValues) {
  value <- dataSet[rowIndex, ] # Each value/entry/element of the dataSet.
  
  subtractedValueFromTheMean <- value - meanOfDataSetByStudent # Subtract the mean from each value in the data.
  squareOfSubtractedValueFromTheMean <- subtractedValueFromTheMean * subtractedValueFromTheMean # Square each of these distances. (subtractedValueFromTheMean^2).
  SumOfSquaresOfSubtractedValuesFromTheMean <- SumOfSquaresOfSubtractedValuesFromTheMean + squareOfSubtractedValueFromTheMean # Add all of the squares together.
}

## Finally calculate the variance.
varianceOfDataSetByStudent = SumOfSquaresOfSubtractedValuesFromTheMean / numberOfValues # Divide the sum of the squares by the number of values in the data set.

varianceOfDataSetByRLanguage = var(dataSetAsMatrix) # Calculate the variance of the dataSetAsMatrix into varianceOfDataSetByRLanguage variable

print(paste("The variance by student method = ", varianceOfDataSetByStudent))
print(paste("The variance by R language method = ", varianceOfDataSetByRLanguage))

# Find the standard deviation
## By far the most common measure of variation for numerical data in statistics is the standard deviation.
## The standard deviation measures how concentrated the data are around the mean. (Exactly as the variance)
## The variance is another way to measure variation in a data set.
## Its downside is that it's in in square units.
## If your data are in dollars, for example, the variance would be in square dollars, which makes no sense.
## That's why we Take the square root of the variance to get the standard deviation.

standardDeviationOfDataSetByStudent = sqrt(varianceOfDataSetByStudent)
standardDeviationOfDataSetByStudent = sd(dataSetAsMatrix)

print(paste("The standard deviation By student method = ", standardDeviationOfDataSetByStudent))
print(paste("The standard deviation By R language method = ", standardDeviationOfDataSetByStudent))
