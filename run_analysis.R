# This is a peer-graded assignment that was completed during my Coursera program

# load dplyr!

library(dplyr)

#download the data

filename <- "Coursera_DS3_Final.zip"

run_analysis <- function(){
  
# Checking if archive already exists.
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  

# Checking if folder exists

if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

# Assigning data frames to variables regarding specific data

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
test_x <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
test_y <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
train_x <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
train_y <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

## Step 1: Merge the training and test sets to create one data set!

X <- rbind(train_x, test_x)
Y <- rbind(train_y, test_y)
Subject <- rbind(train_subject, test_subject)
data_merged <- cbind(Subject, Y, X)

## Step 2: extract the mean and standard deviation for each measurement

Tidy_Data <- data_merged %>% select(subject, code, contains("mean"), contains("std"))

## Step 3: Use descriptive names to refer to specific activity measurements

Tidy_Data$code <- activities[Tidy_Data$code, 2]

## Step 4: appropriately label the data set with descriptive names

names(Tidy_Data)[2] = "activity"
names(Tidy_Data)<-gsub("Acc", "Accelerometer", names(Tidy_Data))
names(Tidy_Data)<-gsub("Gyro", "Gyroscope", names(Tidy_Data))
names(Tidy_Data)<-gsub("BodyBody", "Body", names(Tidy_Data))
names(Tidy_Data)<-gsub("Mag", "Magnitude", names(Tidy_Data))
names(Tidy_Data)<-gsub("^t", "Time", names(Tidy_Data))
names(Tidy_Data)<-gsub("^f", "Frequency", names(Tidy_Data))
names(Tidy_Data)<-gsub("tBody", "TimeBody", names(Tidy_Data))
names(Tidy_Data)<-gsub("-mean()", "Mean", names(Tidy_Data), ignore.case = TRUE)
names(Tidy_Data)<-gsub("-std()", "STD", names(Tidy_Data), ignore.case = TRUE)
names(Tidy_Data)<-gsub("-freq()", "Frequency", names(Tidy_Data), ignore.case = TRUE)
names(Tidy_Data)<-gsub("angle", "Angle", names(Tidy_Data))
names(Tidy_Data)<-gsub("gravity", "Gravity", names(Tidy_Data))

## Final Step: From the data set I just created, create a second data set with 
## the mean of each variable for each activity and subject

FinalData <- Tidy_Data %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)
}

## end of run_analysis

## check final data

FinalData
