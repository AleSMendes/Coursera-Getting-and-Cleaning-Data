# ------------------------------------------------------------------------------
# Process the "Human Activity Recognition Using Smartphones" dataset
## Create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Author: Alessandro de Souza Mendes
# ------------------------------------------------------------------------------
#Load libraries
#install.packages("dplyr");
#install.packages("reshape2")
library(dplyr);
library(reshape2);

# If the dataset is not present in the current working directory then download it
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if (!file.exists("UCI HAR Dataset")) {
  if (!file.exists("data")) {
    dir.create("data")
  }
  download.file(fileUrl, destfile="data/data.zip", method="curl")
  unzip("data/data.zip", exdir="./")
  file.remove("data.zip")
}

# ------------------------------------------------------------------------------
# Step 1 - Merges the training and the test sets to create one data set.
# ------------------------------------------------------------------------------
# training data
Xtrain    <- read.table("UCI HAR Dataset//train//X_train.txt", sep="")
Ytrain    <- read.table("UCI HAR Dataset/train//y_train.txt", col.names=c("activity_id"))
trainSubjects  <- read.table("UCI HAR Dataset//train//subject_train.txt", col.names=c("subject"))
train_data <- cbind(Xtrain, trainSubjects, Ytrain)

# test data
Xtest    <- read.table("UCI HAR Dataset//test/X_test.txt", nrows=2947, comment.char="")
Ytest    <- read.table("UCI HAR Dataset/test//y_test.txt", col.names=c("activity_id"))
testSubjects  <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names=c("subject"))
test_data <- cbind(Xtest, testSubjects, Ytest)

# merge both train and test data
data <- rbind(train_data, test_data)

#remove dfs
rm(train_data, test_data, Xtrain, Ytrain, testSubjects, trainSubjects, Xtest, Ytest)


# ------------------------------------------------------------------------------
# Step 2 - Extracts only the measurements on the mean and standard deviation for each measurement.
# ------------------------------------------------------------------------------
# read features
# read features
features <- read.table("UCI HAR Dataset//features.txt")[,2]

# filter only features that has mean or std in the name
filtered_feature_ids <- grepl("mean|std", features)
filtered_data = data[, filtered_feature_ids]
filtered_feature_names <- features[filtered_feature_ids]
filtered_feature_names <- c(as.vector(filtered_feature_names), "subject", "Activity_Label")
#remove original data
rm(data)

# ------------------------------------------------------------------------------
# Step 3 - Uses descriptive activity names to name the activities in the data set
# ------------------------------------------------------------------------------
# Load: activity labels
activities <- read.table("UCI HAR Dataset//activity_labels.txt", col.names=c("activity_id", "Activity_Label"))
filtered_data <- merge(filtered_data, activities, by.x ="activity_id" , by.y="activity_id")



# ------------------------------------------------------------------------------
# step 4 - Appropriately labels the data set with descriptive variable names.
# ------------------------------------------------------------------------------
#remove first field (activity)
filtered_data <- filtered_data[2:82]
# assign names to features
names(filtered_data) <- filtered_feature_names


# ------------------------------------------------------------------------------
# step 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# ------------------------------------------------------------------------------
id_labels = c("subject",  "Activity_Label")
data_labels = setdiff(colnames(filtered_data), id_labels)
melt_data = melt(filtered_data, id = id_labels, measure.vars = data_labels)
# Apply mean function to dataset using dcast function
tidy_data = dcast(melt_data, subject + Activity_Label ~ variable, mean)
write.table(tidy_data, file = "tidy_data.txt")
