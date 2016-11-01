###################Preliminaries###############
#Load packages
library(data.table)
library(reshape2)
library(dplyr)
#Set path
path = getwd()
path 

################# Get the data###################

#Download the zip file, and put it into directory
url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipfile = "Dataset.zip"
if (!file.exists(path)) {dir.create(path)}
download.file(url, file.path(path,zipfile))

#Unzip the file 
unzipfiles=unzip(zipfile="Dataset.zip",exdir="./")

#Set this folder as input path, and check files in the folder
pathInput = file.path(path, "UCI HAR Dataset")
list.files(pathInput,recursive = TRUE) # go inside folders

############### Read files#############################
dtSubjectTrain <- fread(file.path(pathInput, "train", "subject_train.txt"))
dtSubjectTest <- fread(file.path(pathInput, "test" , "subject_test.txt" ))
dtActivityTrain <- fread(file.path(pathInput, "train", "Y_train.txt"))
dtActivityTest <- fread(file.path(pathInput, "test" , "Y_test.txt" ))
dtDataTrain <- fread(file.path(pathInput, "train", "X_train.txt"))
dtDataTest <- fread(file.path(pathInput, "test" , "X_test.txt" ))

############## Q1. Merge data ###########################
#Merge Train and Test
dtSubject = rbind(dtSubjectTrain, dtSubjectTest)
dtActivity = rbind(dtActivityTrain,dtActivityTest)
dtData = rbind(dtDataTrain, dtDataTest)

#Rename variables
dtSubject = rename(dtSubject, subject=V1)
dtActivity = rename(dtActivity, activityCode=V1)

#Merge Columns 
dtData = cbind(dtSubject,dtActivity,dtData)

#set Key
setkey(dtData,subject,activityCode)

############# Q2. Extract Mean and SD ################
# Read features.txt 
dtFeatures = fread(file.path(pathInput, "features.txt"))
setnames(dtFeatures,colnames(dtFeatures),c("featureCode","featureName") )

# find required features
dtRequireFeatures = dtFeatures[grepl("mean\\(\\)|std\\(\\)",featureName)]

#Add V in front of featureCode to match with dtData
dtRequireFeatures$featureCode = dtRequireFeatures[,paste0("V",featureCode)]
head(dtRequireFeatures)

#subset dtData with requirement 
dtData = as.data.frame(dtData)
dtData = dtData[,c("subject", "activityCode",dtRequireFeatures$featureCode)]
   # didn't find a way to select by colnames in datatable, so convert back to dataframe
dtData = as.data.table(dtData)

############# Q3. Use descriptive activity names##############
#Read activity_lable.txt, and set names 
dtActivityNames = fread(file.path(pathInput,"activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityCode", "activityName"))

#Merge 
dtData = merge(dtData, dtActivityNames, by = "activityCode", all.x=TRUE)

############## Q4. Label dataset with descriptive variable names###########
#Setnames 
setnames(dtData, colnames(dtData),c("subject", "activityCode",dtRequireFeatures$featureName,"activityName"))

#name it human readable 
names(dtData) = gsub("\\(|\\)", "", names(dtData))
names(dtData) = tolower(names(dtData))

############## Q5. Tidy dataset with the average of each variable for each activity and each subject ########
dtTidy = aggregate(x=dtData, by=list(activities=dtData$activityname, subj=dtData$subject), FUN=mean)
dtTidy = select(dtTidy,-subject,-activitycode,-activityname)   
rename(dtTidy,subject=subj)

write.table(dtData,'Tidydata1.txt',row.names=FALSE)
write.table(dtTidy,'Tidydata2.txt',row.names=FALSE)
