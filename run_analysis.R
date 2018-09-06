##Coursera Course 3 - Getting and Cleaning Data 
##Course Project
##9/4/2018 

##====================================================================================
#PART 1 - merges the training and test sets to create one data set 
#read the data files 
#read the train data
traindat <- read.table("train/X_train.txt")
#read training data labels
trainlabs <- read.table("train/y_train.txt")
#read the training data subject IDs 
trainsubs <- read.table("train/subject_train.txt") 
#read the test data 
testdat <- read.table("test/X_test.txt")
#read test data labels 
testlabs <- read.table("test/y_test.txt") 
#read the test data subject IDs 
testsubs <- read.table("test/subject_test.txt") 
#now we combine the data sets with the activity labels 
traindat <- cbind(trainlabs, trainsubs, traindat) 
testdat <- cbind(testlabs, testsubs, testdat) 

#combind the training and testing data sets into a new data table 
dat <- rbind(traindat, testdat) 

#the data sets above don't have column names 
#variable names are saved in the "features" file 
#open the features file 
feats <- read.table("features.txt") 
#save the variable names in a new vector 
feats <- as.vector(feats[,2])
#add a label fo the activity label column we added at the beginning of the table 
feats <- c("activity", "subjectID",feats) 
#now add column names to the data set using the values in feats 
colnames(dat) <- feats 

##====================================================================================
#PART 2 - extracts only the measurements on the mean and standard deviation of each measurement 
#we need to find which columns have data on the mean and standard deviation 
meanLabs <- which(grepl("mean",feats)) 
sdLabs <- which(grepl("std", feats))
rightLabs <- sort(c(1,2, meanLabs, sdLabs)) #need to add a one to make sure the activity labels in the first column are saved 
#these are the indexes that have the mean and std of the variables 
#extract the mean and standard deviation measurements in a new subset data set 
dat2 <- dat[,rightLabs]

##====================================================================================
#PART 3 - use descriptive activity names to name the activities in the data set 
#the activity codes are saved in the first column of the data table
#the activity names are saved in the "activity_labels" file 
#open the activity names file 
act_names <- read.table("activity_labels.txt")
#make a new vector of activity labels that match the activity numbers in column 1
act_labels <- c()
for(i in 1:10299){
	if (dat[i,1]==1) {act_labels<- c(act_labels,as.character(act_names[1,2]))}
	else if (dat[i,1]==2){act_labels <- c(act_labels, as.character(act_names[2,2]))}
	else if (dat[i,1]==3){act_labels <- c(act_labels, as.character(act_names[3,2]))}
	else if (dat[i,1]==4){act_labels <- c(act_labels, as.character(act_names[4,2]))}
	else if (dat[i,1]==5){act_labels <- c(act_labels, as.character(act_names[5,2]))}
	else if (dat[i,1]==6){act_labels <- c(act_labels, as.character(act_names[6,2]))}
}
#there was probably an easier way to do that 
#add a new column to the data table with the activity names 
dat2 <- cbind(act_labels, dat2) 

##====================================================================================
#PART 4 - appropriately label the data set with descriptive variable names 
#remove the dashes and parenthesis
names(dat2) <- gsub("[()]", "", names(dat2))
names(dat2) <- gsub("-","", names(dat2)) 

##====================================================================================
#PART 5 - create a second, independently tidy data set with the average of each variable for each activity and each subject
#load dplyr library 
library(dplyr) 
#group data set by subject and activity type 
grouping <- group_by(dat2, subjectID, act_labels) 
#get the means of all variables by the grouping 
finTable <- summarize_all(grouping, funs(mean)) 