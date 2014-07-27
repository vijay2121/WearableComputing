
# You should create one R script called run_analysis.R that does the following. 
# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names. 
# 5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

#The function run_analysis() needs to be run to get the output which performs the 
#above mentioned five steps.

run_analysis<-function()
{
        # Merge the training and the test sets to create one data set.
        trainDF<-constructDF("train") 
        cat("Dimension of the trainDF is ",dim(trainDF),"\n")
        testDF<-constructDF("test") 
        cat("Dimension of the testDF is ",dim(testDF),"\n")
        CombinedDF<-rbind(trainDF,testDF)
        cat("Dimension of the CombinedDF is ",dim(CombinedDF),"\n")
        
        
        #Add column names i.e.All the features in features.txt
        features<-read.table(".//UCI HAR Dataset//features.txt", as.is=TRUE)
        fList<-features[,2]
        fListWithSubAndActivity<-c("Subject","Activities",fList)
        colnames(CombinedDF)<-fListWithSubAndActivity
        
        
        #Extract mean and std deviation of each measurement
        x<-extract(CombinedDF)
        resultantDF<-CombinedDF[,c(1,2,x)]
        tempDF1<-resultantDF
        
        
        #Set descriptive activity names to name the activities in the data set
        resultantDF$Activities = setNames(resultantDF$Activities)
        
         
        #Creates a second, independent tidy data set with the 
        #average of each variable for each activity and each subject.
        tempDF2<-split(tempDF1,list(tempDF1$Subject,tempDF1$Activities))
        tempDF3<-sapply(tempDF2[][],function(e){sapply(e,mean)})
        write.table(tempDF3,file="./finalOutput.txt")
        tempDF3
}

extract<-function(combinedDF)
{
        colIndices <- vector()
        index=1
        n1<-colnames(combinedDF)
        for(i in 1:length(n1))
        {
                cat("i is ",i," n1[i] is ", n1[i], "\n")
                x<-n1[i]
                cat("value is",grep("mean", n1[i]),"\n")
                if((length(grep("mean", n1[i], ignore.case=TRUE)) != 0) | (length(grep("std", n1[i], ignore.case=TRUE)) != 0))
                {
                        colIndices[index] = i;
                        index = index+1                       
                }
        }
        colIndices
}

#This function constructs the test or training set data.frame based on whether input is atomic string "test" or "train"

constructDF<-function(type)
{
        if(type %in% "train")
        {
                fileName1=".//UCI HAR Dataset//train//X_train.txt"
                fileName2=".//UCI HAR Dataset//train//y_train.txt"
                fileName3=".//UCI HAR Dataset//train//subject_train.txt"
        }
        else if(type %in% "test")
        {
                fileName1=".//UCI HAR Dataset//test//X_test.txt"
                fileName2=".//UCI HAR Dataset//test//y_test.txt"  
                fileName3=".//UCI HAR Dataset//test//subject_test.txt"
        }
        tDF<-read.table(fileName1, as.is=TRUE)        
        tActivity<-read.table(fileName2, as.is=TRUE)
        subjectColumn<-read.table(fileName3, as.is=TRUE)
        resultDF<-cbind(subjectColumn,tActivity,tDF)
        cat("Dimension of the data.frame is ",dim(resultDF),"\n")
        resultDF
}

#This function sets descriptive activity names
setNames<-function(tActivity)
{
        activityList<-read.table(".//UCI HAR Dataset//activity_labels.txt", as.is=TRUE)
        for(i in 1:length(tActivity))
        {
                for(j in 1:nrow(activityList))
                        if(tActivity[i] == activityList[j,1])
                        {
                                tActivity[i] <- activityList[j,2]  
                        }
        }
        tActivity
}