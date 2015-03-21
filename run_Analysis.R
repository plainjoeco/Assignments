##Setting the working directory. Please change as mentioned in README
setwd("C:\\Users\\bimehta\\Desktop\\R_Prog\\DataClean")
##<----------Reading and creating the Initial Datasets----------->

##Reading the Subject Data as 1st Step
subject_data_test <- read.table("./Assignment/test/subject_test.txt", header=F, col.names=c("subjectId"))
subject_data_train <- read.table("./Assignment/train/subject_train.txt", header=F, col.names=c("subjectId"))

##Reading the activity Id data from y_test file and y_train file
y_data_test <- read.table("./Assignment/test/y_test.txt", header=F, col.names=c("activityId"))
y_data_train <- read.table("./Assignment/train/y_train.txt", header=F, col.names=c("activityId"))

##Reading the features from features file.
feat_data<-read.table("./Assignment/features.txt", header=F, as.is=T, col.names=c("measureId", "measureName"))

##Reading the test and train data file and assigning column headers from Fetures dataframe
x_data_test<- read.table("./Assignment/test/x_test.txt", header=F, col.names=feat_data$measureName)
x_data_train<- read.table("./Assignment/train/x_train.txt", header=F, col.names=feat_data$measureName)

##Activity Labels Read
activity_labels <- read.table("./Assignment/activity_labels.txt", header=F, as.is=T, col.names=c("activityId", "activityName"))
activity_labels$activityName <- as.factor(activity_labels$activityName)

##<----------Extracting Mean and Standard Deviation and than merging the dataset----------->
##<----------Subsetting is done before mergin to save memory and have fast processing------->

##Names of subset columns required. subset_data stores column id's of columns where mean() and std() appear
subset_data <- grep(".*mean\\(\\)|.*std\\(\\)", feat_data$measureName)

##Subsetting Data. Filtering the data based on column id's above
x_data1 <- x_data_test[,subset_data]
x_data2 <- x_data_train[,subset_data]

##Merging the two X Data Sets into one single dataset
x_data<-rbind(x_data1, x_data2)

##Merging the two Y Data Sets into one single dataset
y_data<-rbind(y_data_test, y_data_train)

##Merging the subject data sets
subject_data<-rbind(subject_data_test, subject_data_train)

##<----------Using descriptive activity names to name the activities in the data set----------->

##Creating a table with the Activity and Subject ID assigned to X dataset
x_data$activityId <- y_data$activityId
x_data$subjectId <- subject_data$subjectId

##Assigning Activity Labels to final dataset
x_final <- merge(x_data, activity_labels)

##<----------Appropriately label the data set with descriptive activity names----------->

##Cleaning Column Names to remove special characters and creating new headers
cnames <- colnames(x_final)
cnames <- gsub("\\.+mean\\.+", cnames, replacement="Mean")
cnames <- gsub("\\.+std\\.+",  cnames, replacement="Std")
cnames <- gsub(".*BodyBody",  cnames, replacement="Body")
colnames(x_final) <- make.names(cnames, unique=TRUE)
names(x_final)
##<----------Creating anindependent tidy data set with the average of each variable for each activity and each subject----------->

##Seperating the ID and Measure Columns for Melting Data
id_data = c("activityId", "activityName", "subjectId")
measure_data= setdiff(colnames(x_final), id_data)

## Melting the dataset for calculating mean on Activity~Subject
library(reshape2)
melted_data <- melt(x_final, id=id_data, measure.vars=measure_data)
        
## Recast by calculating mean for each Subject~Activity combination
final_file<-dcast(melted_data, activityName + subjectId ~ variable, mean)    

## Creating and saving the tidy data file in the working directory
write.table(final_file, "new_tidy_data.txt")

