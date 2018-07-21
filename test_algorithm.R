printStatistics <- function(data_x){
  str(data_x)
}

# This script checks the effectiveness of the algorithm..
testAlgotithm1 <- function(debug=FALSE){
  # ________START_OF_UNMODIFIABLE_AREA____________
  # Read the test data
  data_x <- read.csv("test_1.csv",header=TRUE,sep = ';')
  stopifnot(is.data.frame(data_x))
  stopifnot(dim(data_x)[2]==2)
  # remove output column
  if(debug){
    printStatistics(data_x)
  }
  # _________END_OF_UNMODIFIABLE_AREA_____________
  
  
  # __________START_OF_MODIFIABLE_AREA____________
  # Example: Replace the following line by calling your algorithm
  # Use "data_x" as your algorithm input
  predicted_values <- rbind(data.frame(Y_pred=data_x$Y),data.frame(Y_pred=runif(10, 0, 1000)))
  # ___________END_OF_MODIFIABLE_AREA_____________
  
  
  # ________START_OF_UNMODIFIABLE_AREA____________
  # Save the output vecotr
  stopifnot(is.data.frame(predicted_values))
  stopifnot(dim(predicted_values)[1]==dim(data_x)[1]+10)
  stopifnot(dim(predicted_values)[2]==1)
  write.table(predicted_values, file = "test_predicted_1.csv",row.names=FALSE,col.names=FALSE)
  # _________END_OF_UNMODIFIABLE_AREA_____________
}

# This script checks the effectiveness of the algorithm
testAlgotithm2 <- function(debug=FALSE){
  # ________START_OF_UNMODIFIABLE_AREA____________
  # Read the test data
  input <- read.csv("test_2.csv",header=TRUE,sep = ',')
  stopifnot(is.data.frame(input))
  stopifnot(dim(input)[2]==8)
  drop <- c("default")
  data_x <- input[,!(names(input) %in% drop)]
  # remove output column
  if(debug){
    printStatistics(data_x)
  }
  # _________END_OF_UNMODIFIABLE_AREA_____________
  
  
  # __________START_OF_MODIFIABLE_AREA____________
  # Example: Replace the following line by calling your algorithm
  # Use "data_x" as your algorithm input
  labels <- data.frame(sample(c(TRUE,FALSE), size=dim(data_x)[1], replace=TRUE))
  # ___________END_OF_MODIFIABLE_AREA_____________
  
  
  # ________START_OF_UNMODIFIABLE_AREA____________
  # Save the output vecotr
  stopifnot(is.data.frame(labels))
  stopifnot(dim(labels)[1]==dim(data_x)[1])
  stopifnot(dim(labels)[2]==1)
  write.table(labels, file = "test_predicted_2.csv",row.names=FALSE,col.names=FALSE)
  # _________END_OF_UNMODIFIABLE_AREA_____________
}

# This script checks the effectiveness of the algorithm
testAlgotithm3_1 <- function(debug=FALSE){
  # ________START_OF_UNMODIFIABLE_AREA____________
  # Read the test data
  input <- read.csv("test_3_1.csv",header=TRUE,sep = ';')
  stopifnot(is.data.frame(input))
  stopifnot(dim(input)[2]==15)
  # remove output column
  drop <- c("y")
  data_x <- input[,!(names(input) %in% drop)]
  if(debug){
    printStatistics(data_x)
  }
  # _________END_OF_UNMODIFIABLE_AREA_____________
  
  
  # __________START_OF_MODIFIABLE_AREA____________
  # Example: Replace the following line by calling your algorithm
  # Use "data_x" as your algorithm input
  labels <- data.frame(replicate(1,sample(0:1,dim(data_x)[1],rep=TRUE)))
  # ___________END_OF_MODIFIABLE_AREA_____________
  
  
  # ________START_OF_UNMODIFIABLE_AREA____________
  # Save the output vecotr
  stopifnot(is.data.frame(labels))
  stopifnot(dim(labels)[1]==dim(input)[1])
  stopifnot(dim(labels)[2]==1)
  write.table(labels, file = "test_predicted_3_1.csv",row.names=FALSE,col.names=FALSE)
  # _________END_OF_UNMODIFIABLE_AREA_____________
}

# This script checks the effectiveness of the algorithm
testAlgotithm3_2 <- function(debug=FALSE){
  # ________START_OF_UNMODIFIABLE_AREA____________
  # Read the test data
  input <- read.csv("test_3_2.csv",header=TRUE,sep = ',')
  stopifnot(is.data.frame(input))
  stopifnot(dim(input)[2]==21)
  # remove output column
  drop <- c("Creditability")
  data_x <- input[,!(names(input) %in% drop)]
  if(debug){
    printStatistics(data_x)
  }
  # _________END_OF_UNMODIFIABLE_AREA_____________
  
  
  # __________START_OF_MODIFIABLE_AREA____________
  # Example: Replace the following line by calling your algorithm
  # Use "data_x" as your algorithm input
  labels <- data.frame(replicate(1,sample(0:1,dim(data_x)[1],rep=TRUE)))
  # ___________END_OF_MODIFIABLE_AREA_____________
  
  
  # ________START_OF_UNMODIFIABLE_AREA____________
  # Save the output vecotr
  stopifnot(is.data.frame(labels))
  stopifnot(dim(labels)[1]==dim(input)[1])
  stopifnot(dim(labels)[2]==1)
  write.table(labels, file = "test_predicted_3_2.csv",row.names=FALSE,col.names=FALSE)
  # _________END_OF_UNMODIFIABLE_AREA_____________
}


# This script checks the effectiveness of the algorithm
testAlgotithm4_1 <- function(debug=FALSE){
  # ________START_OF_UNMODIFIABLE_AREA____________
  # Read the test data
  input <- read.csv("test_4_1.csv",header=TRUE,sep = ',',stringsAsFactors = FALSE)
  stopifnot(is.data.frame(input))
  stopifnot(dim(input)[2]==4)
  # remove output column
  drop <- c("oferta","windykacja")
  data_x <- input[,!(names(input) %in% drop)]
  if(debug){
    printStatistics(data_x)
  }
  # _________END_OF_UNMODIFIABLE_AREA_____________
  
  
  # __________START_OF_MODIFIABLE_AREA____________
  # Example: Replace the following line by calling your algorithm
  # Use "data_x" as your algorithm input
  labels <- data.frame(replicate(1,sample(0:1,dim(data_x)[1],rep=TRUE)))
  # ___________END_OF_MODIFIABLE_AREA_____________
  
  
  # ________START_OF_UNMODIFIABLE_AREA____________
  # Save the output vecotr
  stopifnot(is.data.frame(labels))
  stopifnot(dim(labels)[1]==dim(input)[1])
  stopifnot(dim(labels)[2]==1)
  write.table(labels, file = "test_predicted_4_1.csv",row.names=FALSE,col.names=FALSE)
  # _________END_OF_UNMODIFIABLE_AREA_____________
}


# This script checks the effectiveness of the algorithm
testAlgotithm4_2 <- function(debug=FALSE){
  # ________START_OF_UNMODIFIABLE_AREA____________
  # Read the test data
  input <- read.csv("test_4_2.csv",header=TRUE,sep = ',',stringsAsFactors = FALSE)
  stopifnot(is.data.frame(input))
  stopifnot(dim(input)[2]==4)
  # remove output column
  drop <- c("oferta","windykacja")
  data_x <- input[,!(names(input) %in% drop)]
  if(debug){
    printStatistics(data_x)
  }
  # _________END_OF_UNMODIFIABLE_AREA_____________
  
  
  # __________START_OF_MODIFIABLE_AREA____________
  # Example: Replace the following line by calling your algorithm
  # Use "data_x" as your algorithm input
  labels <- data.frame(replicate(1,sample(0:1,dim(data_x)[1],rep=TRUE)))
  # ___________END_OF_MODIFIABLE_AREA_____________
  
  
  # ________START_OF_UNMODIFIABLE_AREA____________
  # Save the output vecotr
  stopifnot(is.data.frame(labels))
  stopifnot(dim(labels)[1]==dim(input)[1])
  stopifnot(dim(labels)[2]==1)
  write.table(labels, file = "test_predicted_4_2.csv",row.names=FALSE,col.names=FALSE)
  # _________END_OF_UNMODIFIABLE_AREA_____________..
}


args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  print("At least one argument must be supplied (input file)", call.=FALSE)
} else if(args[1]=='1') {
  testAlgotithm1()
}else if(args[1]=='2') {
  testAlgotithm2()
}else if(args[1]=='3_1') {
  testAlgotithm3_1()
}else if(args[1]=='3_2') {
  testAlgotithm3_2()
}else if(args[1]=='4_1') {
  testAlgotithm4_1()
}else if(args[1]=='4_2') {
  testAlgotithm4_2()
}else{
  print("Wrong argument", call.=FALSE)
}
