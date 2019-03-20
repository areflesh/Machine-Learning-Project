library(caret)
library(randomForest)
a_names <- read.table("./UCI HAR Dataset/activity_labels.txt", stringsAsFactors=F)
v_names <- read.table("./UCI HAR Dataset/features.txt", stringsAsFactors=F)
## train data
tr_data <- read.table("./UCI HAR Dataset/train/X_train.txt")
tr_activity <- read.table("./UCI HAR Dataset/train/y_train.txt")
## test data
tt_data <- read.table("./UCI HAR Dataset/test/X_test.txt")
tt_activity <- read.table("./UCI HAR Dataset/test/y_test.txt")
editN <- function(x) {
y <- v_names[x,2]
y <- sub("BodyBody", "Body", y) #subs duplicate names
y <- gsub("-", "", y) # global subs for dash
y <- gsub(",", "_", y) # global subs for comma
y <- sub("\\()", "", y) # subs for ()
y <- gsub("\\)", "", y) # global subs for
y <- sub("\\(", "_", y) # subs for (
y <- paste0("v",v_names[x,1], "_",y) #add number, prevent duplicat.
return(y)
}
## edit names
new_names <- sapply(1:nrow(v_names), editN)
## work with training data
names(tr_data)<-new_names
tr_data <- cbind(tr_activity[,1], tr_data)
names(tr_data)[1]<-"Activity"
## work with test data
names(tt_data)<-new_names
tt_data <- cbind(tt_activity[,1], tt_data)
names(tt_data)[1]<-"Activity"
a_names[2,2] <- substr(a_names[2,2], 1, 10) #cut long names
a_names[3,2] <- substr(a_names[3,2], 1, 12)
tr_data <- transform(tr_data, Activity=factor(Activity))
tt_data <- transform(tt_data, Activity=factor(Activity))
levels(tr_data[,1])<-a_names[,2]
levels(tt_data[,1])<-a_names[,2]
rang <- sapply(new_names, function(x){
range(tr_data[,x])
})
min(rang)
max(rang)
fitControl <- trainControl(method="cv", number=5)
set.seed(123)
tstart <- Sys.time()
forest_full <- train(Activity~., data=tr_data,
method="rf", do.trace=10, ntree=100,
trControl = fitControl)
tend <- Sys.time()
print(tend-tstart)
prediction <- predict(forest_full, newdata=tt_data)
cm <- confusionMatrix(prediction, tt_data$Activity)
print(cm)
fitControl <- trainControl(method="cv", number=5)
tstart <- Sys.time()
svm_full <- train(Activity~., data=tr_data,
method="svmRadial",
tend <- Sys.time()
print(tend-tstart)
prediction <- predict(svm_full, newdata=tt_data)
cm <- confusionMatrix(prediction, tt_data$Activity)
print(cm)
plot(varImp(forest_full),20, scales=list(cex=1.1))

