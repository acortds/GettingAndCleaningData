---
Title: "Getting and Cleaning Data Codebook"
Author: "Adrian Cortinas"
---

# Description

The code is composed of 4 sections.
1. Download File
2. Read Files into Variables
3. Merge Raw Data
4. Transform Raw Data into Tidy Data

# Variables

## Work variables
| Variable Name | Description|
|---------------|--------|
| pwd | Directory where the analysis_run.R file was executed.|
| fileURL | URL of the compressed data file|

## Data File Location
| Variable Name | Description|
|---------------|--------|
| LabelURL      | URL to activity description file.|
| ColNamesURL   | URL to "features.txt" file.|
| XtestURL      | URL to "test/X_test.txt" file. This is the Test data set.|
| YtestURL      | URL to "test/y_test.txt" file. This is the Test activity id.|
| SubTestURL    | URL to "test/subject_test.txt" file. This identifies the subject that performed the Test activity.|
| XtrainURL     | URL to "train/X_train.txt" file. This is the Training data set.|
| YtrainURL     | URL to "train/y_train.txt" file. This is the Training activity id.|
| SubTrainURL   | URL to "train/subject_train.txt" file. This identifies the subject that performed the Training activity.|

## Raw Data Variables
| Variable Name | Description|
|---------------|--------|
| ColNames      | Data from ColNamesURL. Contains two columns, V1 is the sequential id, V2 is the name of the function.|
| LabelDesc     | Data from LabelURL. Contains two columns, act_id which is the id of the activity, and activity, which is the description.|
| Ytrain        | Data from YtrainURL. Contains one column, the activity id.|
| Ytest         | Data from YtestURL. Contains one column, the activity id.|
| Xtrainsub     | Data from SubTrainURL. Contains one column, the subject id.|
| Xtestsub      | Data from SubTestURL. Contains one column, the subject id.|
| Xtrain        | Data from XtrainURL. Contains 561 columns. This is actual the Training data set. |
| Xtest         | Data from XtestURL. Contains 561 columns. This is actual the Test data set.|

## Intermediate Data Variables
| Data Set      | Description|
|---------------|--------|
| TestActSub    | Merged data from Ytest and Xtestsub data sets. Data represents activity id done by subject id.|
| TrainActSub   | Merged data from Ytrain and Xtrainsub data sets. Data represents activity id done by subject id.|
| XTestData     | Column level Merged data from TestActSub and Xtest data sets.|
| XTrainData    | Column level Merged data from TrainActSub and Xtrain data sets.|

## Data Variables
| Data Set     | Description|
|--------------|--------|
| RawData      | Row level Merged data from XTestData and XTrainData data sets.|
| TidyData     | Tidied up data from RawData representing the average of each variable for each activity and each subject|

# Transformation

## Section 1 - Download dataset

Once you source or execute run_analysis.R script, it will look for a directory named HAR (Human Activity Recognition), if it does exists it will remove the directory to discard any old data. Once directory is removed, it will create HAR directory again, it will download the zip file, unzip it and change working directory to where the data files are present

```r
rm(list = ls())
pwd <- getwd()

if (dir.exists("HAR")) {
  unlink("HAR", recursive = TRUE)
}
dir.create("HAR")
setwd("HAR")

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile = "projectiles FUCI HAR Dataset.zip",method = "curl")
unzip("projectiles FUCI HAR Dataset.zip")
setwd("UCI HAR Dataset")
```

Load required Libraries
```r
# Load packages
library(dplyr)
library(tidyr)
```
## Section 2 - Read Files into Variables

Assign data files to variables

```r
# Data files
LabelURL    <- "activity_labels.txt"
ColNamesURL <- "features.txt"
XtestURL    <- "test/X_test.txt"
YtestURL    <- "test/y_test.txt"
SubTestURL  <- "test/subject_test.txt"
XtrainURL   <- "train/X_train.txt"
YtrainURL   <- "train/y_train.txt"
SubTrainURL <- "train/subject_train.txt"
```

## Section 3 - Merge Raw Data

Before we do anything with the names of the columns, it's easier if we manipulate the names of the columns. One way of doing this
is by removing any punctuation character such as parenthesis and dashes. We do this with the use of gsub function.

```r
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
```

Also, it's easier if we use common names for similar data. When loading the following data sets we specify the name of their columns

```r
LabelDesc   <- read.table(LabelURL    , col.names = c("act_id", "activity"))
Ytrain      <- read.table(YtrainURL   , col.names = c("act_id"))
Ytest       <- read.table(YtestURL    , col.names = c("act_id"))
Xtrainsub   <- read.table(SubTrainURL , col.names = c("sub_id"))
Xtestsub    <- read.table(SubTestURL  , col.names = c("sub_id"))
```

Because the data column's names are already cleaned up, we use them when loading the following data sets.

```r
Xtrain      <- read.table(XtrainURL   , header = FALSE  , col.names = ColNames[, 2])
Xtest       <- read.table(XtestURL    , header = FALSE  , col.names = ColNames[, 2])
```

This is the first set of data merging. Basically we are joining the activity id with the subject id.

```r
# Merge Activity ID + Subject ID
TestActSub  <- data.frame(Ytest  , Xtestsub)
TrainActSub <- data.frame(Ytrain , Xtrainsub)
```

Once we have activity and subject ids merged, we join them to the actual test/train data. We have two raw data sets, one for test and another for training data.

```r
# Merge ActSub + Data Set
XTestData   <- cbind(TestActSub  , Xtest)
XTrainData  <- cbind(TrainActSub , Xtrain)
```

Finally, we can merge the test and train data sets into one single data set. This is the first task specified in the assignment. We save this data into disk for later analysis.

```r
# Merges the training and the test sets to create one data set. (Step 1)
RawData     <- rbind(XTestData, XTrainData)

setwd(pwd)
write.csv(RawData, "RawData.csv")
```

## Section 4 - Transform Raw Data into Tidy Data

Now that we have all the data saved into disk, we can remove all variables that are no longer needed.

```r
rm(list = ls(pattern = "^X(.*)|^Y(.*)|^C(.*)|^S(.*)|^T(.*)"))
```

In order to produce the tidy data set, we make use of the chain command. This has the advantage of making all required transformations
in a single (large) action. The assigment asks to produce a tidy data file with the average of each variable for each activity and each subject.
In order to accomplish that we do the following.


Transform the data and saves the result into TidyData. This would be Step 5 of the assignment:
```r
TidyData <- RawData %>%
```

Select all columns except all the angle ones. angle is a function like mean and stddev:
```r
   select (1:556) %>%
```

Add a new column named activity based on the act_id column, this adds a meaningful activity description (Step 3):
```r
   mutate(activity = LabelDesc$activity[RawData$act_id]) %>%
```

Transforms all columns to rows and place them into two variables, test and value. Test represents the features and values the actual data.
```r
   gather (test, value, 3:556) %>%
```

Splits test column into 4 columns. 
| Variable Name | Description|
|---------------|--------|
| domain        | is the feature, either the time domain signal or frequency domain signal.|
| funct         | is the function applied to the Domain.|
| axis          | describes to what axis or other element was the function applied to.|
| other         | a catch-all column.|

```r
separate(test,c("domain","funct","axis","other"),fill="right") %>%
```

Select only those rows that contains the words "mean" and "std" (Step 2):
```r
   filter(funct == "mean" | funct == "std") %>%
```

Groups the resulting data set by these columns:
```r
group_by(activity, sub_id, domain, funct) %>% 
```

Gets the mean of the value column by the previously grouped columns:
```r
   summarise(value=mean(value)) %>% 
```

Summarise function provides the mean and std means values in the same column, different rows. We need to put the mean and std rows into their own column:
```r
   spread (funct, value) %>% 
```

We now sort the resulting data set by activity and subject id:
```r
   arrange(activity, sub_id) %>% 
```

Finally we rename the columns to something meaningful (Step 4):
```r
   rename(subject_id=sub_id,average_mean=mean,average_std_dev=std)
```

This is the full command:
```r
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
```

To conclude the analysis, we save the data into a csv file.

```r
write.csv(TidyData, "TidyData.csv")
```
