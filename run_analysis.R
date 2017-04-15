rm(list = ls())
pwd <- getwd()

if (dir.exists("HAR")) {
  unlink("HAR", recursive = TRUE)
}
dir.create("HAR")
setwd("HAR")

# Download dataset
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile = "projectiles FUCI HAR Dataset.zip",method = "curl")
unzip("projectiles FUCI HAR Dataset.zip")
setwd("UCI HAR Dataset")

# Load packages
library(dplyr)
library(tidyr)

# Data files
LabelURL    <- "activity_labels.txt"
ColNamesURL <- "features.txt"
XtestURL    <- "test/X_test.txt"
YtestURL    <- "test/y_test.txt"
SubTestURL  <- "test/subject_test.txt"
XtrainURL   <- "train/X_train.txt"
YtrainURL   <- "train/y_train.txt"
SubTrainURL <- "train/subject_train.txt"

# Read data files
ColNames    <- read.table(ColNamesURL)
# Replace all punctuation characters with ":"
ColNames$V2 <- gsub("[-()]", ":", ColNames$V2)
# Remove repeating ":", f.e. "::" -> ":"
ColNames$V2 <- gsub("(:)+", ":", ColNames$V2)
# Remove ":" at the end of the line
ColNames$V2 <- gsub(":$", "", ColNames$V2)
# For range numbers, replace , with a C, so they can be easily manipulated when needed
ColNames$V2 <- gsub(",", "C", ColNames$V2)

LabelDesc   <-
  read.table(LabelURL    , col.names = c("act_id", "activity"))
Ytrain      <- read.table(YtrainURL   , col.names = c("act_id"))
Ytest       <- read.table(YtestURL    , col.names = c("act_id"))
Xtrainsub   <- read.table(SubTrainURL , col.names = c("sub_id"))
Xtestsub    <- read.table(SubTestURL  , col.names = c("sub_id"))
Xtrain      <-
  read.table(XtrainURL   , header = FALSE  , col.names = ColNames[, 2])
Xtest       <-
  read.table(XtestURL    , header = FALSE  , col.names = ColNames[, 2])

# Merge Activity ID + Subject ID
TestActSub  <- data.frame(Ytest  , Xtestsub)
TrainActSub <- data.frame(Ytrain , Xtrainsub)

# Merge ActSub + Data Set
XTestData   <- cbind(TestActSub,  Xtest)
XTrainData  <- cbind(TrainActSub, Xtrain)

# Merges the training and the test sets to create one data set. (Step 1)
RawData     <- rbind(XTestData, XTrainData)

setwd(pwd)
write.csv(RawData, "RawData.csv", row.names = FALSE)

# remove variables no longer used
rm(list = ls(pattern = "^X(.*)|^Y(.*)|^C(.*)|^S(.*)|^T(.*)"))

# From the data set in step 4, creates a second, independent tidy data set
# with the average of each variable for each activity and each subject
TidyData <- RawData %>%
  select (1:556) %>%
  mutate(activity = LabelDesc$activity[RawData$act_id]) %>%
  gather (test, value, 3:556) %>%
  separate(test, c("domain", "funct", "axis", "other"), fill = "right") %>%
  filter(funct == "mean" | funct == "std") %>%
  group_by(activity, sub_id, domain, funct) %>%
  summarise(value = mean(value)) %>%
  spread (funct, value) %>%
  arrange(activity, sub_id) %>%
  rename(
    subject_id = sub_id,
    average_mean = mean,
    average_std_dev = std
  )

write.csv(TidyData, "TidyData.csv", row.names = FALSE)
write.table(TidyData, "TidyData.txt", row.names = FALSE)
