#Getting and cleaning Data Final Project
#by Kate Wang

install.packages("reshape2")

library("reshape2")
library("dplyr")

#Get data from web
path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path, "data.zip")) #download from this url to my working directory and named it "data.zip" 
unzip(zipfile = "data.zip")

#Get activity labels and features
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

#Merges train & test data set
x_data <- rbind(x_train, x_test)
y_data <- rbind(y_train, y_test)
subject_data <- rbind(subject_train, subject_test)
merged_data <- cbind(subject_data, y_data, x_data)

#Extracts only the measurements on the mean and standard deviation for each measurement.
avg_sd <- merged_data %>%
  select(subject, code, contains("mean"), contains("std"))

#Uses descriptive activity names to name the activities in the data set
avg_sd$code <- activityLabels[avg_sd$code, 2]

#Appropriately labels the data set with descriptive variable names. 
allData <- avg_sd
names(allData)[2] = "activity"
names(allData)<-gsub("Acc", "Accelerometer", names(allData))
names(allData)<-gsub("Gyro", "Gyroscope", names(allData))
names(allData)<-gsub("BodyBody", "Body", names(allData))
names(allData)<-gsub("Mag", "Magnitude", names(allData))
names(allData)<-gsub("^t", "Time", names(allData))
names(allData)<-gsub("^f", "Frequency", names(allData))
names(allData)<-gsub("tBody", "TimeBody", names(allData))
names(allData)<-gsub("-mean()", "Mean", names(allData), ignore.case = TRUE)
names(allData)<-gsub("-std()", "STD", names(allData), ignore.case = TRUE)
names(allData)<-gsub("-freq()", "Frequency", names(allData), ignore.case = TRUE)
names(allData)<-gsub("angle", "Angle", names(allData))
names(allData)<-gsub("gravity", "Gravity", names(allData))

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
FinalData <- allData %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)


