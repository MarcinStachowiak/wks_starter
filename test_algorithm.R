# This script checks the effectiveness of the algorithm
testAlgotithm4_1 <- function(){
  # ________START_OF_UNMODIFIABLE_AREA____________
  # Read the test data
  input = read.csv("test_x_4_1.csv",header=TRUE,sep = ',')
  stopifnot(is.data.frame(input))
  stopifnot(dim(input)[2]==2)
  # _________END_OF_UNMODIFIABLE_AREA_____________
  
  
  # __________START_OF_MODIFIABLE_AREA____________
  # Example: Replace the following line by calling your algorithm
  labels = data.frame(replicate(1,sample(0:2,dim(input)[1],rep=TRUE)))
  # ___________END_OF_MODIFIABLE_AREA_____________
  
  
  # ________START_OF_UNMODIFIABLE_AREA____________
  # Save the output vecotr
  stopifnot(is.data.frame(labels))
  stopifnot(dim(labels)[1]==dim(input)[1])
  stopifnot(dim(labels)[2]==1)
  write.table(labels, file = "test_predicted_4_1.csv",row.names=FALSE,col.names=FALSE)
  # _________END_OF_UNMODIFIABLE_AREA_____________
}

testAlgotithm4_1()

