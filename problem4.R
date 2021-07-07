# File: problem4.R
# Auther: AAGOOGLE
# 
# Created on May 7, 2017

library("MASS")

############################################# MAIN ################################################################

  dataSet <- read.table("problem3_appendix.dat")
  trainingSetMatrix = matrix(data = as.matrix( dataSet ), nrow = 5, ncol = 300, byrow = TRUE)

  labelSet <- read.table("problem4.dat") # Read "problem4.dat" text file into labelSet variable.
  labelSetMatrix <- matrix(data = as.matrix( labelSet ), nrow = 1, ncol = 300)


  trainingSetMatrix <- rbind(labelSetMatrix, trainingSetMatrix)
  trainingSet <- as.data.frame(t(trainingSetMatrix), stringsAsFactors = FALSE) # http://stackoverflow.com/questions/20481772/r-error-some-group-is-too-small-for-qda
  labelSetMatrix <- t(labelSetMatrix)
  
  numberOfSamples = ncol(trainingSetMatrix)
  numberOfFeatures = nrow(trainingSetMatrix)
  
  
  layoutMatrix <- rbind(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10), c(11, 12, 13, 14, 15),
                        c(16, 17, 18, 19, 20), c(21, 22, 23, 24, 25))
  layout(mat = layoutMatrix)
  
  print("=========================== Begin: QDA on each pair ======================================")
  
  for (i in 2 : numberOfFeatures) {

    Feature1 = trainingSetMatrix[i, ]

    for (j in 2 : numberOfFeatures) {
      
      if (i == j) {
        plot(0, type='n', axes = FALSE, ann = FALSE)
        next
      }

      Feature2 = trainingSetMatrix[j, ] 
      
      qdaModel = qda(trainingSet[, c(i, j)], grouping = trainingSet[, 1])
      
      print(qdaModel)
      
      
      plot(Feature2, Feature1, xlab = paste("Feature", (j - 1)), ylab = paste("Feature", (i - 1)), col = ifelse(trainingSetMatrix[1, ] == "R", "red", "blue"), las = 1, pch = 20)
      
      
    }

  }
  
  print("=========================== End: QDA on each pair ======================================")
  

  
  ######################################################################################################################
  
  print("=========================== Begin: QDA on all features (5 features) ======================================")
  
  qdaModel = qda(trainingSet[, -1], grouping = trainingSet[, 1])
  
  print(qdaModel)
  
  print("=========================== End: QDA on all features (5 features) ======================================")
  
  ######################################################################################################################
