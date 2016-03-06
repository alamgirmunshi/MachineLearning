library(RColorBrewer)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
set.seed(33125)
#trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-Training.csv"
#testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-Testing.csv"

Training <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))
Testing <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))
inTrain <- createDataPartition(y=Training$classe, p=0.6, list=FALSE)
datTraining <- Training[inTrain, ]; datTesting <- Training[-inTrain, ]
dim(datTraining)
dim(datTesting)
datDataNearZeroVariable <- nearZeroVar(datTraining, saveMetrics=TRUE)
datNearZeroVariables <- names(datTraining) %in% c("new_window", "kurtosis_roll_belt", 
                                                  "kurtosis_picth_belt","kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", 
                                                  "skewness_yaw_belt","max_yaw_belt", "min_yaw_belt", "amplitude_yaw_belt", "avg_roll_arm", 
                                                  "stddev_roll_arm","var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", 
                                                  "avg_yaw_arm","stddev_yaw_arm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm",
                                                  "kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm",
                                                  "max_roll_arm", "min_roll_arm", "min_pitch_arm", "amplitude_roll_arm", "amplitude_pitch_arm",
                                                  "kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", 
                                                  "skewness_roll_dumbbell","skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_yaw_dumbbell", 
                                                  "min_yaw_dumbbell","amplitude_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", 
                                                  "kurtosis_yaw_forearm","skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", 
                                                  "max_roll_forearm","max_yaw_forearm", "min_roll_forearm", "min_yaw_forearm", "amplitude_roll_forearm",
                                                  "amplitude_yaw_forearm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm",
                                                  "avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm",
                                                  "stddev_yaw_forearm", "var_yaw_forearm")
datTraining <- datTraining[!datNearZeroVariables]
dim(datTraining)
datTraining <- datTraining[c(-1)]
TrainingVer3 <- datTraining #creating another subset to iterate in loop
for(i in 1:length(datTraining)) { #for every column in the Training dataset
  if( sum( is.na( datTraining[, i] ) ) /nrow(datTraining) >= .6 ) { #if n?? NAs > 60% of total observations
    for(j in 1:length(TrainingVer3)) {
      if( length( grep(names(datTraining[i]), names(TrainingVer3)[j]) ) ==1)  { #if the columns are the same:
        TrainingVer3 <- TrainingVer3[ , -j] #Remove that column
      }
    }
  }
}
dim(TrainingVer3)
datTraining <- TrainingVer3
rm(TrainingVer3)
DataCleanV1 <- colnames(datTraining)
DataCleanV2 <- colnames(datTraining[, -58])
datTesting <- datTesting[DataCleanV1]
Testing <- Testing[DataCleanV2]
dim(datTesting)
dim(Testing)
for (i in 1:length(Testing) ) {
  for(j in 1:length(datTraining)) {
    if( length( grep(names(datTraining[i]), names(Testing)[j]) ) ==1)  {
      class(Testing[j]) <- class(datTraining[i])
    }
  }
}
Testing <- rbind(datTraining[2, -58] , Testing)
Testing <- Testing[-1,]
modFitV1 <- rpart(classe ~ ., data=datTraining, method="class")
fancyRpartPlot(modFitV1)
predictionsV1 <- predict(modFitV1, datTesting, type = "class")
confusionMatrix(predictionsV1, datTesting$classe)
modFitV2 <- randomForest(classe ~. , data=datTraining)
predictionsV2 <- predict(modFitV2, datTesting, type = "class")
confusionMatrix(predictionsV2, datTesting$classe)
predictionsV3 <- predict(modFitV2, Testing, type = "class")
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("question_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(predictionsV3)

