# Assignment: Getting and Cleaning Data Course Project
#1.   Merges the training and the test sets to create one data set.
#2.   Extracts only the measurements on the mean and standard deviation for each measurement.
#3.   Uses descriptive activity names to name the activities in the data set
#4.   Appropriately labels the data set with descriptive variable names.
#5.   From the data set in step 4, creates a second, independent tidy data set with 
#       the average of each variable for each activity and each subject.

#load required libraries
library(plyr); #library(dplyr)

# reading all the data to the workspace
activityLabels <- read.table('activity_labels.txt',header = FALSE) # read activity names
names(activityLabels)<- c("activityId", "activityName")


features <- read.table('features.txt', header = FALSE) #read the feature list
names(features) <- c("featureId","featureName")

#read train data
trainX <- read.table('train/X_train.txt', header = FALSE)
trainY <- read.table('train/y_train.txt')
trainSubject <- read.table("./train/subject_train.txt", header = FALSE)

# assign required names to col names
names(trainSubject) <- c("subjectId") 
names(trainY) <- c("activityId")
names(trainX) <- features[,2]

# make the combined train data
trainData <- cbind(trainY,trainSubject,trainX)

#read test data
testX <- read.table('test/X_test.txt', header = FALSE)
testY <- read.table('test/y_test.txt')
testSubject <- read.table("./test/subject_test.txt", header = FALSE)

#assign required names to cols
names(testSubject) <- c("subjectId")
names(testY) <- c("activityId")
names(testX) <- features[,2]

# make the combined test data
testData <- cbind(testY,testSubject,testX) 

#merge test and train data
mergedData <- rbind(trainData, testData)


# extract the mean and std cols from merge data
mergeData_mean_std <- mergedData[,grepl("mean|std|subjectId|activityId",names(mergedData))]

#use the descriptive activity names in the merged data set
mergeDataMeanStd_descriptive <- join(activityLabels,mergeData_mean_std, by = "activityId")
mergeDataMeanStd_descriptive <- mergeDataMeanStd_descriptive[,-1]


#clean the column names 
names(mergeDataMeanStd_descriptive) <- gsub("\\(|\\)","",names(mergeDataMeanStd_descriptive),perl = TRUE)
names(mergeDataMeanStd_descriptive) <- make.names(names(mergeDataMeanStd_descriptive))

names(mergeDataMeanStd_descriptive) <- gsub('Acc',".Acceleration",names(mergeDataMeanStd_descriptive))
names(mergeDataMeanStd_descriptive) <- gsub('Mag',"Magnitude",names(mergeDataMeanStd_descriptive))
names(mergeDataMeanStd_descriptive) <- gsub('^t',"TimeDomain.",names(mergeDataMeanStd_descriptive))
names(mergeDataMeanStd_descriptive) <- gsub('^f',"FrequencyDomain.",names(mergeDataMeanStd_descriptive))
names(mergeDataMeanStd_descriptive) <- gsub('\\.mean',".Mean",names(mergeDataMeanStd_descriptive))
names(mergeDataMeanStd_descriptive) <- gsub('\\.std',".StandardDeviation",names(mergeDataMeanStd_descriptive))
names(mergeDataMeanStd_descriptive) <- gsub('Freq\\.',"Frequency.",names(mergeDataMeanStd_descriptive))
names(mergeDataMeanStd_descriptive) <- gsub('Freq$',"Frequency",names(mergeDataMeanStd_descriptive))

# create a new copy of the final merged data 
mergeDataMeanStd_descriptive2 <- mergeDataMeanStd_descriptive

#creates a second, independent tidy data set with the average of each variable for each 
#activity and each subject.
tidy<- ddply(mergeDataMeanStd_descriptive2,c("subjectId","activityName"),numcolwise(mean))

#save it to a file
write.table(tidy, file = "tidydata_mean.txt")

