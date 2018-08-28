# The script executes the following steps:
# - Load the individual data files of the training and test sets and merge them 
# into one data table
# - Filter the data table to select only mean and standard deviation columns
# - Load labels and levels to the activity column and convert it into a factor 
#   variable
# - Assign cleaned feature strings as column names for the feature measurements
# - Merge test and training subsets into one final cleaned data set
# - Create another data set with average of each variable for each activity and 
#   each subject

# load required libraries
library(data.table)
library(dplyr)
library(reshape2)
dataset.dir<-"C:/User/Arun Chaudhary/Desktop/jyoti/R coursera videos/week 5/getdata_2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset"

# Import training data from files & Name the columns 
features <- read.table('C:/Users/Arun Chaudhary/Desktop/jyoti/R coursera videos/week 5/getdata_2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/features.txt',header=FALSE)
activityLabels <- read.table('C:/Users/Arun Chaudhary/Desktop/jyoti/R coursera videos/week 5/getdata_2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/activity_labels.txt',header=FALSE)
colnames(activityLabels) <- c("activityId","activityType")
subjectTrain <- read.table('C:/Users/Arun Chaudhary/Desktop/jyoti/R coursera videos/week 5/getdata_2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt',header=FALSE)
colnames(subjectTrain) <- "subjectId"
xTrain <- read.table('C:/Users/Arun Chaudhary/Desktop/jyoti/R coursera videos/week 5/getdata_2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/train/x_train.txt',header=FALSE); colnames(xTrain) <- 
  features[,2]
yTrain <- read.table('C:/Users/Arun Chaudhary/Desktop/jyoti/R coursera videos/week 5/getdata_2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/train/y_train.txt',header=FALSE); colnames(yTrain) <- 
  "activityId"

# Merge Data into complete training set
trainingSet = cbind(yTrain,subjectTrain,xTrain)

# Import test data from files & Name columns
subjectTest <- read.table('C:/Users/Arun Chaudhary/Desktop/jyoti/R coursera videos/week 5/getdata_2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt',header=FALSE)
colnames(subjectTest) <- "subjectId"
xTest <- read.table('C:/Users/Arun Chaudhary/Desktop/jyoti/R coursera videos/week 5/getdata_2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/test/x_test.txt',header=FALSE); colnames(xTest) <- 
  features[,2]
yTest <- read.table('C:/Users/Arun Chaudhary/Desktop/jyoti/R coursera videos/week 5/getdata_2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/test/y_test.txt',header=FALSE); colnames(yTest) <- 
  "activityId"

# Merge Data into complete test set
testSet = cbind(yTest,subjectTest,xTest)

# Combine Training Data Set and Test Data Set into one Merged Data Set
MergedDataSet = rbind(trainingSet,testSet)

# Create columns vector to prepare data for subsetting
columns <- colnames(MergedDataSet)

###### 2. Extract only the measurements on the mean and standard deviation for each measurement

# Create a vector that indentifies the ID, mean & stddev columns as TRUE
vector <- (grepl("activity..",columns) | grepl("subject..",columns) | grepl("-mean..",columns) &
             !grepl("-meanFreq..",columns) & !grepl("mean..-",columns) | 
             grepl("-std..",columns) & !grepl("-std()..-",columns))

# Update MergedDataSet based on previously identified columns
MergedDataSet <- MergedDataSet[vector==TRUE]

###### 3. Use descriptive activity names to name the activities in the data set

# Add in descriptive activity names to MergedDataSet & update columns vector
MergedDataSet <- merge(MergedDataSet,activityLabels,by='activityId',all.x=TRUE)
MergedDataSet$activityId <-activityLabels[,2][match(MergedDataSet$activityId, activityLabels[,1])] 

columns <- colnames(MergedDataSet)

###### 4. Appropriately label the data set with descriptive activity names.

# Tidy column names
for (i in 1:length(columns)) 
{
  columns[i] <- gsub("\\()","",columns[i])
  columns[i] <- gsub("-std$","StdDev",columns[i])
  columns[i] <- gsub("-mean","Mean",columns[i])
  columns[i] <- gsub("^(t)","time",columns[i])
  columns[i] <- gsub("^(f)","freq",columns[i])
  columns[i] <- gsub("([Gg]ravity)","Gravity",columns[i])
  columns[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columns[i])
  columns[i] <- gsub("[Gg]yro","Gyro",columns[i])
  columns[i] <- gsub("AccMag","AccMagnitude",columns[i])
  columns[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columns[i])
  columns[i] <- gsub("JerkMag","JerkMagnitude",columns[i])
  columns[i] <- gsub("GyroMag","GyroMagnitude",columns[i])
}

# Update MergedDataSet with new descriptive column names
colnames(MergedDataSet) <- columns

# Remove activityType column
MergedDataSet <- MergedDataSet[,names(MergedDataSet) != 'activityType']

###### 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Averaging each activity and each subject as Tidy Data
tidyData <- aggregate(MergedDataSet[,names(MergedDataSet) 
                                    != c('activityId','subjectId')],by=list
                      (activityId=MergedDataSet$activityId,
                        subjectId=MergedDataSet$subjectId),mean);

# Export tidyData set 
write.table(tidyData, 'C:/Users/Arun Chaudhary/Desktop/jyoti/R coursera videos/week 5/getdata_2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/FinalTidyData.txt',row.names=FALSE,sep='\t')
