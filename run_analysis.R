library(dplyr)
library(data.table)

## 1. Get and merge the data
#read all data
X_test <- read.table("C:/Users/Tori Tori/Downloads/coursera/UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt", quote="\"", comment.char="")
y_test <- read.table("C:/Users/Tori Tori/Downloads/coursera/UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt", quote="\"", comment.char="")

X_train <- read.table("C:/Users/Tori Tori/Downloads/coursera/UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt", quote="\"", comment.char="")
y_train <- read.table("C:/Users/Tori Tori/Downloads/coursera/UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt", quote="\"", comment.char="")

subject_train <- read.table("C:/Users/Tori Tori/Downloads/coursera/UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt", quote="\"", comment.char="")
subject_test <- read.table("C:/Users/Tori Tori/Downloads/coursera/UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt", quote="\"", comment.char="")

features <- read.table("C:/Users/Tori Tori/Downloads/coursera/UCI HAR Dataset/UCI HAR Dataset/features.txt", quote="\"", comment.char="")
activities <- read.table("C:/Users/Tori Tori/Downloads/coursera/UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt", quote="\"", comment.char="")

#Merge
dataSubject <- rbind(subject_train, subject_test)
dataY <- rbind(y_train, y_test)
dataX <- rbind(X_train, X_test)
#Set names to variables
names(dataSubject) <- c("subject")
names(dataY) <- c("activity")
dataXNames <- features
names(dataX) <- dataXNames$V2
#Merge
dataCombine <- cbind(dataSubject, dataY)
Data <- cbind(dataX, dataCombine)

## 2. Extract  only the measurements on the mean and standard deviation for each measurement.

sapply(dataSubject, mean, na.rm=TRUE)  
sapply(dataSubject, sd, na.rm=TRUE)
sapply(dataX, mean, na.rm=TRUE)  
sapply(dataX, sd, na.rm=TRUE)  
sapply(dataY, mean, na.rm=TRUE)  
sapply(dataY, sd, na.rm=TRUE)

##3. Uses descriptive activity names to name the activities in the data set

Data$activity[Data$activity==1] <- "WALKING"
Data$activity[Data$activity==2] <- "WALKING_UPSTAIRS"
Data$activity[Data$activity==3] <- "WALKING_DOWNSTAIRS"
Data$activity[Data$activity==4] <- "SITTING"
Data$activity[Data$activity==5] <- "STANDING"
Data$activity[Data$activity==6] <- "LAYING"

##4.  Appropriately label the data set with descriptive activity names.

names(Data)<-gsub("^t", "time", names(Data))
names(Data)<-gsub("^f", "frequency", names(Data))
names(Data)<-gsub("Acc", "Accelerometer", names(Data))
names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
names(Data)<-gsub("Mag", "Magnitude", names(Data))
names(Data)<-gsub("BodyBody", "Body", names(Data))

## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidyData<-aggregate(. ~subject + activity, Data, mean)
tidyData<-tidyData[order(tidyData$subject,tidyData$activity),]
write.table(tidyData, file = "tidydata.txt",row.name=FALSE)
tidyData