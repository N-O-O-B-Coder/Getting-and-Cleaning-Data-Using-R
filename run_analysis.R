library(dplyr)

#read features
features<-read.table("F:/data/UCI HAR Dataset/features.txt", col.names = c("n","functions"))

#read activities
activities <- read.table("F:/data/UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

#read train data
subject_train <- read.table("F:/data/UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("F:/data/UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("F:/data/UCI HAR Dataset/train/y_train.txt", col.names = "code")

#read test data
subject_test <- read.table("F:/data/UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("F:/data/UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("F:/data/UCI HAR Dataset/test/y_test.txt", col.names = "code")

#1.Merges the training and the test sets to create one data set.
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)

#2.Extracts only the measurements on the mean and standard deviation for each measurement.
TidyData <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))

# 3. Uses descriptive activity names to name the activities in the data set
TidyData$code <- factor(TidyData$code,levels = activities[, 1], labels = activities[, 2])

#4.Appropriately labels the data set with descriptive variable names.
col_names <-colnames(TidyData)
col_names <-colnames(TidyData)
col_names <- gsub("[\\(\\)-]", "", col_names)
col_names <- gsub("[.]", "", col_names)
col_names <- gsub("^f", "frequencyDomain", col_names)
col_names <- gsub("^t", "timeDomain", col_names)
col_names <- gsub("Acc", "Accelerometer", col_names)
col_names <- gsub("Gyro", "Gyroscope", col_names)
col_names <- gsub("Mag", "Magnitude", col_names)
col_names <- gsub("Freq", "Frequency", col_names,ignore.case = TRUE)
col_names <- gsub("mean", "Mean", col_names)
col_names <- gsub("std", "StandardDeviation", col_names)
col_names[2] <-"activity"
col_names <- gsub("BodyBody", "Body", col_names)
colnames(TidyData) <- col_names

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
FinalData <- TidyData %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(FinalData, "F:/R_basics/getting and cleaning data Week 4 assignment/TidyData.txt", row.name=FALSE)












