### INTRODUCTION ###
# The following R file contains the R code for the 5 exercises in the Data Cleaning Assigment
# It gives response to the 5 exercices included in the assignment
# There is an output for each exercise that is always:
  # A data.table named "tidydata%" where % is the exercise number 
  # A txt file named "tidy_data_set%.txt" where % is the exercise number
###

### Initial settings ###
# First of all is to download dataset and unzip it in the working directory (or the desired one)
# To do this we use the following commands from the desired directory:
#
setwd("~/coursera/DCleaning") # Setting the working directory to where we want to unzip the files
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", temp, method = "libcurl", mode = "wb")
unzip(temp)

## Packages to be used
library(data.table)

## Setting dataset file names variables for reading
testdata <- "~/coursera/DCleaning/UCI HAR Dataset/test/X_test.txt"
testsubj <- "~/coursera/DCleaning/UCI HAR Dataset/test/subject_test.txt"
testact <- "~/coursera/DCleaning/UCI HAR Dataset/test/y_test.txt"

traindata <- "~/coursera/DCleaning/UCI HAR Dataset/train/X_train.txt"
trainsubj <- "~/coursera/DCleaning/UCI HAR Dataset/train/subject_train.txt"
trainact <- "~/coursera/DCleaning/UCI HAR Dataset/train/y_train.txt"
#
### END OF iNITIAL SETTINGS ###

### Exercise 1. Merges the training and the test sets to create one data set. ###
# Subject codes (between 1 and 30) for each record are obtained in subject_xxx.txt file
# Activity codes (between 1 and 6) for each record are obtained in y_xxx.txt file
# Measurement values are obtained in X_xxx.txt file
# Measurement variable names are obtained in features.txt file
# Function getTinyData unifies all input files into one unique data table
  # if names are available, it creates the variables names too
# Output is a data table object called "tidydata1" with the following columns:
  # Subject <- Who perfomed the test (values 1 to 30)
  # Activity_code <- Activity that the subject was performing during the test (values from 1 to 6)
  # Measurements <- A total of 561 variables acording to features.txt
#
# The following function creates a tidydata object with all measurements inside the input files parameters:
  # datafile <- Measurements data txt file name
  # subjectfile <- Subject data txt file name
  # activityfile <- Activity data txt file name
  # datanames <- Vector with the measurement names
#
getTidyData <- function(datafile, subjectfile = NULL, activityfile = NULL, datanames = NULL) {
  tidydata <- as.data.table(read.table(datafile))
  if(is.null(datanames) == FALSE) names(tidydata) <- datanames
  if(is.null(activityfile) == FALSE) {
    activities <- as.data.table(read.table(activityfile)) # read the activities of each record
    names(activities) <- "Activity_code"
    tidydata <- cbind(activities,tidydata)
  }
  if(is.null(subjectfile) == FALSE) {
    subjects <- as.data.table(read.table(subjectfile)) # Read subjects who performed each record
    names(subjects) <- "Subject"
    tidydata <- cbind(subjects,tidydata)
  }
  tidydata
}

# Start "test" & "train" data merging process using getTidyData function
datanames <- read.table("~/coursera/DCleaning/UCI HAR Dataset/features.txt", colClasses = "character")
datanames <- datanames$V2
tidytest <- getTidyData(testdata, testsubj, testact, datanames) # Generates one data table with all subject, activity and measurement "test" set
tidytrain <- getTidyData(traindata, trainsubj, trainact, datanames) # Generates one data table with all subject, activity and measurement "train" set
#
# Rbind "test" & "train" tidy data
tidydata1 <- rbind(tidytest,tidytrain)
#
# Finally write object tidydata into a txt file
write.table(tidydata1, file = "tidy_data_set1.txt", row.names = FALSE)
### END OF Exercise 1 ###

### Exercise 2. Extracts only the measurements on the mean and standard deviation for each measurement ###
# Subsets tidydata data table removing non necessary columns:
# Output is a data table object called "tidydata2" with the following columns:
  # Subject <- Who perfomed the test (values 1 to 30)
  # Activity_code <- Activity that the subject was performing during the test (values from 1 to 6)
  # Measurements <- mean and sd of all 3 axis, a total of 6 columns
#
tidydata2 <- tidydata1[,1:8]
#
# Finally write object tidydata into a txt file
write.table(tidydata2, file = "tidy_data_set2.txt", row.names = FALSE)
### END OF Exercise 2 ###

### Exercise 3. Uses descriptive activity names to name the activities in the data set
# Inserts column Activity_name related to and just after Activity_code one
# Output is a data table object called "tidydata3" with the following columns:
  # Subject <- Who perfomed the test (values 1 to 30)
  # Activity_code <- Activity that the subject was performing during the test (values from 1 to 6)
  # Activity_name <- Name acording to activity_label.txt file
  # Measurements <- mean and sd of all 3 axis corresponding to the first 6 features
#
actlabels <- as.data.table(read.table("~/coursera/DCleaning/UCI HAR Dataset/activity_labels.txt")) # read general activities names
#
## Create data object by inserting activity name after activity code in the tidydata2 object
tidydata3 <- data.table(tidydata2[,1:2],
                       ActivityName=actlabels[as.data.table(tidydata2$Activity_code),.(V2), on="V1"],
                       tidydata2[,3:8])
setnames(tidydata3,"ActivityName.V2","Activity_name") # Rename the new column
#
# Finally write object tidydata into a txt file
write.table(tidydata3, file = "tidy_data_set3.txt", row.names = FALSE)
### END OF Exercise 3 ###

### Exercise 4. Appropriately labels the data set with descriptive variable names ###
# This exercise is already done as we have labeled all variables in previous exercises
# Just replaces '-' by '_' and removes '()' in names for better usage
# Output is a data table object called "tidydata4" with the same columns as before
#
tidydata4 <- tidydata3
names(tidydata4) <- gsub("-","_",names(tidydata4),fixed = TRUE)
names(tidydata4) <- gsub("()","",names(tidydata4),fixed = TRUE)
#
# Finally write object tidydata into a txt file
write.table(tidydata4, file = "tidy_data_set4.txt", row.names = FALSE)
### END OF Exercise 4 ###

### Exercise 5. From the data set in step 4, creates a second, independent tidy data set 
  ### with the average of each variable for each activity and each subject ###
# Output is a data table object called "tidydata5" with the following columns:
  # Activity_name <- Name acording to activity_label.txt file
  # Subject <- Who perfomed the test (values 1 to 30)
  # Measurements <- Mean of each measurement variable grouped by Activity_name and Subject 
#
tidydata5 <- tidydata4[,.(mean(tBodyAcc_mean_X),
                          mean(tBodyAcc_mean_Y),
                          mean(tBodyAcc_mean_Z),
                          mean(tBodyAcc_std_X),
                          mean(tBodyAcc_std_Y),
                          mean(tBodyAcc_std_Z)), 
                       by=c("Activity_name","Subject")]
tidydata5 <- tidydata5[order(Activity_name,Subject),]
colnames(tidydata5)[3:8] <- as.character(names(tidydata4[,4:9]))
#
# Finally write object tidydata into a txt file
write.table(tidydata5, file = "tidy_data_set5.txt", row.names = FALSE)
### END OF Exercise 5 ###

