# r script to get and clean UCI HAR dataset
# getting and cleaning data, peer assessment 2
# this script assumes you want the data in the working directory

# per instructions, this script accomplishes the following:
# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3) Uses descriptive activity names to name the activities in the data set
# 4) Appropriately labels the data set with descriptive variable names. 
# 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(RCurl)

# download the file to the working directory and extract
if (!file.exists("./data")) {dir.create("./data")}
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, destfile="./data/UCI-HAR-dataset.zip", method = "curl")
  unzip("./data/UCI-HAR-dataset.zip", exdir = "./data")

# combine test and train sets into single set
# unintuitively, observations are labeled 'X' and the labels 'Y'

# observations first
obs.train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
obs.test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
obs.data <- rbind(obs.train, obs.test)
# then labels
label.train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
label.test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
label.data <- rbind(label.train, label.test)
# then subjects
subj.train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
subj.test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
subj <- rbind(subj.train,subj.test)

# we want only the standard deviation and mean for each measurement
desired.features <- read.table("./data/UCI HAR Dataset/features.txt")
mean.sd.features <- grep("-mean\\(\\)|-std\\(\\)", desired.features[,2])
x.mean.sd <- obs.data[, mean.sd.features]

# rename the measurement names something useful
names(x.mean.sd) <- desired.features[mean.sd.features, 2]
names(x.mean.sd) <- tolower(names(x.mean.sd))
names(x.mean.sd) <- gsub("\\(|\\)", "", names(x.mean.sd))

# get activity names
activities <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
activities[,2] <- tolower(as.character(activities[,2]))
activities[,2] <- gsub("_", "", activities[,2])

# rename the labels according to activity names
label.data[, 1] <- activities[label.data[,1], 2]
colnames(label.data) <- "activity"
# add subject
colnames(subj) <- "subject"

# construct and write out merged set
data <- cbind(subj, x.mean.sd, label.data)
write.table(data, "./data/UCI HAR Dataset/merged_data.txt", row.names = FALSE)
str(data)

# write out second smaller, tidy set for average of each activity and each subject
average.set <- aggregate(x=data, by=list(activities=data$activity, subj=data$subject), FUN=mean)
average.set <- average.set[, !(colnames(average.set) %in% c("subj", "activity"))]
write.table(average.set, "./data/UCI HAR Dataset/average.txt", row.names = FALSE)
str(average.set)

