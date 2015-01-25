
##Assiment 1 DATA Cleaning
##Author:Ignacio Ferrero
##Description: This Assignmentextracts de data from multiple sources and cleans it to prepare it for analyzing


## Reading DATA from different source
testData <- read.table("UCI HAR Dataset/test/X_test.txt")
trainData <- read.table("UCI HAR Dataset/train/X_train.txt")
train <- rbind(testData, trainData)

testSubject <- read.table("UCI HAR Dataset/test/subject_test.txt")
trainSubject <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject <- rbind(testSubject, trainSubject)

testLabel <- read.table("UCI HAR Dataset/test/y_test.txt")
trainLabel <- read.table("UCI HAR Dataset/train/y_train.txt")
label <- rbind(testLabel, trainLabel)

## Read Features List --> Column Names
featuresList <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors=FALSE)
features <- featuresList$V2

activities <- read.table("UCI HAR Dataset/activity_labels.txt")
activities[,2] = gsub("_", "", tolower(as.character(activities[,2])))

columnsName <- grepl("(std|mean[^F])", features, perl=TRUE)
##Data needed for assignment 
train<- train[, columnsName]
names(train) <- features[columnsName]
names(train) <- gsub("\\(|\\)", "", names(train))
names(train) <- tolower(names(train))

##adding labels
label[,1] = activities[label[,1], 2]
names(label) <- "activity" 
names(subject) <- "subject"
tidyData <- cbind(subject, label, train)
write.table(tidyData, "tidyData.txt")


##Second part of the assignement
##Average

uS = unique(sibject)[,1]
nS = length(uS)
nA = length(activities[,1])
nC = length(names(tidyData))
td = tidyData[ 1:(nS*nA), ]
row = 1
for (s in 1:nS) {
  for (a in 1:nA) {
    td[row,1] = uS[s]
    td[row,2] = activities[a, 2]
    tmp <- tidyData[tidyData$subject==s & tidyData$activity==activities[a,2],]
    td[row, 3:nC] <- colMeans(tmp[, 3:nC])
    row = row + 1
  }
}
write.table(td, "tidyData2.txt")