# File: problem5.R
# Auther: AAGOOGLE
# 
# Created on May 10, 2017

library("MASS")

############################################# MAIN ################################################################

  dataSet <- read.table("problem3_appendix.dat")
  trainingSetMatrix = matrix(data = as.matrix( dataSet ), nrow = 5, ncol = 300, byrow = TRUE)

  labelSet <- read.table("problem4.dat")
  labelSetMatrix <- matrix(data = as.matrix( labelSet ), nrow = 1, ncol = 300)


  trainingSetMatrix <- rbind(labelSetMatrix, trainingSetMatrix)
  trainingSet <- as.data.frame(t(trainingSetMatrix), stringsAsFactors = FALSE) # http://stackoverflow.com/questions/20481772/r-error-some-group-is-too-small-for-qda
  labelSetMatrix <- t(labelSetMatrix)
  
  numberOfSamples = ncol(trainingSetMatrix)
  numberOfFeatures = nrow(trainingSetMatrix)
  
  qdaModel = qda(trainingSet[, -1], grouping = trainingSet[, 1])
  
  testSet <- read.table("problem5.dat")
  
  testSetMatrix <- matrix(data = as.matrix( testSet ), nrow = 5, ncol = 5, byrow = TRUE)
  colnames(testSetMatrix) <-  paste("t" , 1:ncol(testSetMatrix), sep = "")
  
  results <- predict(qdaModel, t(testSetMatrix))
  
  for(i in 1:nrow(testSetMatrix)) {
    currentVector = paste("t" , i, sep = "")
    print(paste(currentVector, "->", predict(qdaModel, t(testSetMatrix)[i, ])$class))
  }


  # Why Bayes classifier is the best ?!
  ## Bayes classifier is the one that produces minimum error/loss (proven)
  ## Bayes classifier is fast and space efficient
  ## Bayes classifier is NOT sensitive to irrelevant features

  # What is the problem of Bayes classifier ?!
  ## Bayes classifier assumes independence of features