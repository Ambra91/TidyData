#load and merge train and test measumerements
testUrl<-(paste0(getwd(),"/test/X_test.txt"))
trainUrl<-(paste0(getwd(),"/train/X_train.txt"))
train<-read.table(trainUrl)
test<-read.table(testUrl)
all<-rbind(train,test)
#load and merge subjects
s_train<-read.table(paste0(getwd(),"/train/subject_train.txt"))
s_test<-read.table(paste0(getwd(),"/test/subject_test.txt"))
subj<-rbind(s_train,s_test)
names(subj)<-c("Subject")

#load and merge activities

act_train<-read.table(paste0(getwd(),"/train/y_train.txt"))
act_test<-read.table(paste0(getwd(),"/test/y_test.txt"))
act <- rbind(act_train,act_test)
# Rename activities


names(act)<-c("Activity")
act[act$Activity == 1,1] = "walking"
act[act$Activity == 2,1] = "walking_upstairs"
act[act$Activity == 3,1] = "walking_downstairs"
act[act$Activity == 4,1] = "sitting"
act[act$Activity == 5,1] = "standing"
act[act$Activity == 6,1] = "laying"

#rename columns with features
features<-read.table(paste0(getwd(),"/features.txt"))
names(all)<-features[,2]

#retain only means and standard deviations
Tidy<-all[,(grepl("mean",names(all)))|(grepl("std",names(all)))]
#Add columns corresponding to the Activity and Subjects
Tidy$Activity=act$Activity
Tidy$Subject=subj[,]
write.table(Tidy,"./Tidy1.txt", row.name=FALSE)
#END FIRST PART


#Begin second part, computation of the means
#create a new column, in order to apply the function tapply
Tidy$mes<-paste0(Tidy$Subject,Tidy$Activity)
#The first column of the new data frame is computed
c1<-tapply(Tidy[,1],Tidy$mes,mean)
#A matrix that will then trasform into the final data frame is created and inizialized with right dimensions
m<-matrix(ncol=ncol(Tidy)-1,nrow=length(c1))
m[,1]=c1
#a loop, to compute the means of each column through tapply function
for (i in 2:(ncol(Tidy)-3)) {
  m[,i]<-tapply(Tidy[,i],Tidy$mes,mean)
}
#Subjects and activities are added
m[,80]<-tapply(Tidy$Activity,Tidy$mes, head,1)
m[,81]<-tapply(Tidy$Subject,Tidy$mes, head,1)
#convert to a data frame
x<-data.frame(m)
#rename columns
names(x)<-names(Tidy[,1:(ncol(Tidy)-1)])
#write table
write.table(x,"./Tidy2.txt", row.name=FALSE)

