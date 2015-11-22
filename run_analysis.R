# run_analysis.R does the following: 
# 1. Merges the training and the test sets to create one data set.
getwd()
setwd("C:/Users/lukas/Documents/UCI HAR Dataset")

#read features and activities
features<-read.table("~/UCI HAR Dataset/features.txt")
names(features) <- c("order", "name")

activities <- read.table('~/UCI HAR Dataset/activity_labels.txt')
names(activities) <- c("order", "name")

# read train.files
subjecttrain<-read.table("~/UCI HAR Dataset/train/subject_train.txt")
names(subjecttrain)<-"subject.ID"

x_train<-read.table("~/UCI HAR Dataset/train/X_train.txt")
names(x_train)<-features$name

ytrain<-read.table("~/UCI HAR Dataset/train/y_train.txt")
names(ytrain)<-"Activity"

# read test.files
subjecttest<-read.table("~/UCI HAR Dataset/test/subject_test.txt")
names(subjecttest)<-"subject.ID"

x_test<-read.table("~/UCI HAR Dataset/test/X_test.txt")
names(x_test)<-features$name

ytest<-read.table("~/UCI HAR Dataset/test/y_test.txt")
names(ytest)<-"Activity"

#combind x+y columns 
train_data_xy <- cbind(x_train,ytrain)
test_data_xy <- cbind(x_test,ytest)

#combind subject columns
train_data<-cbind(train_data_xy,subjecttrain)
test_data<-cbind(test_data_xy,subjecttest)

#combind rows get the full data
fulldata<-rbind(train_data,test_data)

# 2. Extracts only the measurements on the mean and standard deviation for 
#    each measurement.

# select features containing "mean" and "std"
featuresSubSet <- c(grep("mean[(]|std[(]", features[,2]), 
                    ncol(fulldata)-1, ncol(fulldata))
# select data with subset features + subject + activities
subdata <- fulldata[, featuresSubSet]

# 3. Uses descriptive activity names to name the activities in the data set
# descriptive activity names to name the activities
subdata$activities <- as.character(factor(subdata$activities, 
                                        labels=activities$name))

# 4. Appropriately labels the data set with descriptive variable names.
colNames <- names(subdata)
colNames <-gsub("^t", "Time", colNames)
colNames <-gsub("^f", "Freq", colNames)
colNames <-gsub("std", "StdDevi", colNames)
colNames <-gsub("(|)", "", colNames)
names(subdata) <- make.names(colNames)
head(subdata)
write.table(subdata, "~/UCI HAR Dataset/tidydata.txt")

# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.

newdata <- subdata[,-c(ncol(subdata)-1, ncol(subdata))] 
newdata<-aggregate(newdata, by = list(subdata$Activity, subdata$subject.ID), mean)
write.table(newdata, "~/UCI HAR Dataset/tidydata_mean.txt")
