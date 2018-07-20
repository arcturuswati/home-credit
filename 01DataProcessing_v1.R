# Data processing script
# 
# assign working directory path
wd.path <- 'C:/Users/swati/Desktop/Everything else/Kaggle/HomeCreditDefaultRisk/input' 
setwd(wd.path)
set.seed(1234)
library(dplyr)
library(foreach)
library(caret)
test <- read.csv('application_test.csv') # applications test data
train <- read.csv('application_train.csv') # applications train data

# storing column names for later
saveNames <- names(train)

# columns with missing values
missingCols <-  names(train)[apply(train, 2, function(x) sum(is.na(x))) > 0]

#Sample mode function from Source: https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
Mode <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

imputationFunction <- function(imputeToData, imputeFromData, FUN, missingCols, suffix){
  # imputeToData -  Imputation to be done on this data
  # imputeFromData - Imputations calculation from this data
  # FUN - imputation function
  # missingCols - missing value column names
  # suffix - suffix to add after column name
  
  imputeToData <- imputeToData[, names(imputeToData) %in% missingCols]
  imputeFromData <- imputeFromData[, names(imputeFromData) %in% missingCols]
  imputeVec <- apply(imputeFromData, 2, function(x) FUN(x, na.rm = T))
  
  for (i in 1:length(missingCols)) {
    imputeToData[is.na(imputeToData[, names(imputeToData) %in% missingCols[i]])
               , names(imputeToData) %in% missingCols[i]] <- imputeVec[names(imputeVec) %in% missingCols[i]]
  }
  
  names(imputeToData) <- paste0(names(imputeToData), suffix)
  return(imputeToData)
}

meanDatatr <- imputationFunction(imputeToData = train, imputeFromData = train, FUN = mean
                               , missingCols = missingCols, suffix = '.trmean')

medianDatatr <- imputationFunction(imputeToData = train, imputeFromData = train, FUN = median
                                 , missingCols = missingCols, suffix = '.trmedian')

modeDatatr <- imputationFunction(imputeToData = train, imputeFromData = train, FUN = Mode
                                 , missingCols = missingCols, suffix = '.trmode')


meanDatatst <- imputationFunction(imputeToData = test, imputeFromData = train, FUN = mean
                               , missingCols = missingCols, suffix = '.trmean')

medianDatatst <- imputationFunction(imputeToData = test, imputeFromData = train, FUN = median
                                 , missingCols = missingCols, suffix = '.trmedian')

modeDatatst <- imputationFunction(imputeToData = test, imputeFromData = train, FUN = Mode
                               , missingCols = missingCols, suffix = '.trmode')

weights <- read.csv('Training_Weight_V1.csv') %>% select(-X)

train <- cbind(train, meanDatatr, medianDatatr, modeDatatr) %>% left_join(weights)

rm(meanDatatr, medianDatatr, modeDatatr, weights)

test <- cbind(test, meanDatatst, medianDatatst, modeDatatst) 

rm(meanDatatst, medianDatatst, modeDatatst)

# add variable specific imputation
# add cross validation - 

train$fold <- caret::createFolds(train$TARGET, 5, FALSE)
