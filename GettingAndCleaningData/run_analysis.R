#1)Merges the training and the test sets to create one data set.
trainData1 <- read.table("./train/X_train.txt")
dim(trainData1)
head(trainData1)
trainData2 <- read.table("./train/y_train.txt")
table(trainData2)
trainSubject <- read.table("./train/subject_train.txt")
testData1 <- read.table("./test/X_test.txt")
dim(testData1)
testData2 <- read.table("./test/y_test.txt") 
table(testData2) 
testSubject <- read.table("./test/subject_test.txt")
joinData1 <- rbind(trainData1, testData1)
dim(joinData1)
joinData2 <- rbind(trainData2, testData2)
dim(joinData2)
joinSubject <- rbind(trainSubject, testSubject)
dim(joinSubject)

#2)Extracts only the measurements on the mean and standard deviation for each 
#  measurement. 
features <- read.table("./features.txt")
dim(features) 
meanstd <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanstd) 
joinData <- joinData1[, meanstd]
dim(joinData) 
names(joinData) <- gsub("\\(\\)", "", features[meanstd, 2]) # remove "()"
names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names 

#3)Uses descriptive activity names to name the activities in the data set
activity <- read.table("./activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinData2[, 1], 2]
joinData2[, 1] <- activityLabel
names(joinData2) <- "activity"

#4)Appropriately labels the data set with descriptive variable names. 
names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinData2, joinData1)
dim(cleanedData) 
write.table(cleanedData, "merged_data.txt") 

#5)From the data set in step 4, creates a second, independent tidy
#data set with the average of each variable for each activity and each subject. 
subjectLen <- length(table(joinSubject)) # 30
activityLen <- dim(activity)[1] # 6
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
        for(j in 1:activityLen) {
                result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
                result[row, 2] <- activity[j, 2]
                bool1 <- i == cleanedData$subject
                bool2 <- activity[j, 2] == cleanedData$activity
                result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
                row <- row + 1
        }
}
head(result)
write.table(result, "AvgData.txt") 
