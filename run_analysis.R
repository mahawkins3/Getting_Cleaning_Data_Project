#Load dependent packages
library(data.table)
library(dplyr)
library(sqldf)

#Load data
x_test <- as.data.frame(fread("test/X_test.txt"))
y_test <- fread("test/Y_test.txt")

x_train <- as.data.frame(fread("train/X_train.txt"))
y_train <- fread("train/Y_train.txt")

subject_test <- fread("test/subject_test.txt")
subject_train <- fread("train/subject_train.txt")

#Activities
activities <- read.table("activity_labels.txt")

#Feature Names
featureNames <- as.data.frame(fread("features.txt"))
featureNames <- as.character(as.vector(featureNames[, 2]))

#Use feature names as column headings
colnames(x_test) <- featureNames
colnames(x_train) <- featureNames

#List of columns to include (those with mean or std)
mean_std <- grep("mean|std", featureNames)

#Select only columns from the above list
x_test <- x_test[, mean_std]
x_train <- x_train[, mean_std]

#Before merging, create column to determine whether observation is test or training
x_test <- mutate(x_test, Type = "Test")
x_train <- mutate(x_train, Type = "Training")

#Name all activities
y_test <- sqldf("SELECT *
                FROM y_test
                JOIN activities USING(V1)")
y_train <- sqldf("SELECT *
                FROM y_train
                JOIN activities USING(V1)")
colnames(y_test) <- c("Code", "Activity")
colnames(y_train) <- c("Code", "Activity")

#Bind observations with activities
x_test <- cbind(y_test$Activity, x_test)
x_train <- cbind(y_train$Activity, x_train)
colnames(x_test)[1] <- "Activity"
colnames(x_train)[1] <- "Activity"

#Bind subjects with datasets
x_test <- cbind(subject_test$V1, x_test)
x_train <- cbind(subject_train$V1, x_train)
colnames(x_test)[1] <- "Subject"
colnames(x_train)[1] <- "Subject"

#Merge all data into single table
data <- rbind(x_test, x_train)

#Make variable names more descriptive
varNames <- colnames(data[3:81])
for (i in 1:length(varNames))
{
  varNames[i] = gsub("\\()","",varNames[i])
  varNames[i] = gsub("-std$","StdDev",varNames[i])
  varNames[i] = gsub("-mean","Mean",varNames[i])
  varNames[i] = gsub("^(t)","time",varNames[i])
  varNames[i] = gsub("^(f)","freq",varNames[i])
  varNames[i] = gsub("([Gg]ravity)","Gravity",varNames[i])
  varNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",varNames[i])
  varNames[i] = gsub("[Gg]yro","Gyro",varNames[i])
  varNames[i] = gsub("AccMag","AccMagnitude",varNames[i])
  varNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",varNames[i])
  varNames[i] = gsub("JerkMag","JerkMagnitude",varNames[i])
  varNames[i] = gsub("GyroMag","GyroMagnitude",varNames[i])
}
varNames <- c("Subject", "Activity", varNames, "Type")
colnames(data) <- varNames 

#Calculate average of each variable for each activity and each subject
avgs <- aggregate(data[, 3:81], by = list(data$Subject, data$Activity), mean)
colnames(avgs)[1:2] <- c("Subject", "Activity")

#Create new data set containing only the aggregations
write.table(avgs, "./aggregated.txt", row.names = FALSE, sep= '\t')
