#San Diego State University
#MIS 720 - E-Business and Big Data Infrastructures
#Term Project
#Predicting Length of stay in hospital
#Team: Aztec Data Analytics
#Max Gueniau, Akshaya Viswanathan, Najah Izquierdo, Naveen Reddy Sama, Tobias Kleinhansl

#---------------------------------

#1 Packages
library(ggplot2)
library(caret)
library(ROCR)
library(pROC)
require(caret) 
library(rpart.plot)

#2 Data Preparation

#Import an first look
data_raw <- read.csv('/Users/naveenreddysama/Desktop/Naveen/BDA/Semester1/MIS 720/Project/train.csv')
nrow(data_raw)
head(data_raw)
View(data_raw)
summary(data_raw)


#Data types
str(data_raw)

# changing characters to factors
data_raw$Hospital_type_code <- as.factor(data_raw$Hospital_type_code)
data_raw$Department <- as.factor(data_raw$Department)
data_raw$Ward_Type <- as.factor(data_raw$Ward_Type)
data_raw$Ward_Facility_Code <- as.factor(data_raw$Ward_Facility_Code)
data_raw$Type.of.Admission <- as.factor(data_raw$Type.of.Admission)
data_raw$Severity.of.Illness <- as.factor(data_raw$Severity.of.Illness)
data_raw$Age <- as.factor(data_raw$Age)

#predictors with data type character should be transformed using one hot encoding at a later point


#Unique values for target variable
table(data_raw$Stay)

#Unique values for categorical predictors

summary(data_raw)
table(data_raw$Hospital_code)
table(data_raw$Hospital_type_code)
table(data_raw$City_Code_Hospital)
table(data_raw$Hospital_region_code)
table(data_raw$Department)
table(data_raw$Ward_Type)
table(data_raw$Ward_Facility_Code)
table(data_raw$Bed.Grade)
table(data_raw$City_Code_Patient)
table(data_raw$Type.of.Admission)
table(data_raw$Severity.of.Illness)
table(data_raw$Age)


#Drop row case_id since its just there for numbering the rows and holds no value for predicting the stay
data_red1 <- subset(data_raw, select = -c(case_id))
data_red2 <- subset(data_red1, select = -c(patientid))
data_red2 <- subset(data_red1, select = -c(Hospital_region_code))
head(data_red2)


#A constraint for the project was to use datasets with a maximum of 100,000 rows.
#Therefore 218438 rows are dropped randomly
#For reproducability, a seed is set
set.seed(12345)
data_red3 <- data_red2[sample(1:nrow(data_red1), 100000),]
nrow(data_red3)
#Check the first lines of data_red2 vs data_red3
head(data_red2)
head(data_red3)


#The hospital wants to be able to predict if a patient stays longer than 30 days
#The target variable 'Stay' has therefore to be transformed into binary, where 0 = less or equal than 30 days and 1= more than 30 days
data_red3$Stay <- gsub('0-10', 0, data_red3$Stay)
data_red3$Stay <- gsub('11-20', 0, data_red3$Stay)
data_red3$Stay <- gsub('21-30', 0, data_red3$Stay)
data_red3$Stay <- gsub('31-40', 1, data_red3$Stay)
data_red3$Stay <- gsub('41-50', 1, data_red3$Stay)
data_red3$Stay <- gsub('51-60', 1, data_red3$Stay)
data_red3$Stay <- gsub('61-70', 1, data_red3$Stay)
data_red3$Stay <- gsub('71-80', 1, data_red3$Stay)
data_red3$Stay <- gsub('81-90', 1, data_red3$Stay)
data_red3$Stay <- gsub('91-100', 1, data_red3$Stay)
data_red3$Stay <- gsub('More than 100 Days', 1, data_red3$Stay)
View(data_red3)


#Check if non-categorical data (Available Extra Rooms in Hospital, Visitors with Patient, Admission_Deposit) is skewed and transform accordingly
summary(data_red3$Available.Extra.Rooms.in.Hospital)
hist(data_red3$Available.Extra.Rooms.in.Hospital)
#Skewed, a cutoff at 8 seems reasonable
data_skew1 <- subset(data_red3, data_red3$Available.Extra.Rooms.in.Hospital<=8)
View(data_skew1)
hist(data_skew1$Available.Extra.Rooms.in.Hospital)
summary(data_skew1$Available.Extra.Rooms.in.Hospital)

summary(data_skew1$Visitors.with.Patient)
hist(data_skew1$Visitors.with.Patient)
#Skewed, a cutoff at 11 seems reasonable
data_skew2 <- subset(data_skew1, data_skew1$Visitors.with.Patient<=11)
hist(data_skew2$Visitors.with.Patient)
summary(data_skew2$Visitors.with.Patient)

summary(data_skew2$Admission_Deposit)
hist(data_skew2$Admission_Deposit)
#A little skewed, a cutoff at 9000 seems reasonable
data_skew3 <- subset(data_skew2, data_skew2$Admission_Deposit<=9000)
hist(data_skew3$Admission_Deposit)
summary(data_skew3$Admission_Deposit)

#Check how many rows were dropped
nrow(data_red3)
nrow(data_skew3)
#1106 rows were dropped, so about 1%

# Age with respect to length of stay Bar Plot

ggplot(data_skew3, aes(x=Age, fill=Stay))+
  scale_fill_manual(breaks = c("1", "0"), 
                    values=c("blue", "light blue"))+
  geom_bar(position="fill")+
  ggtitle("Length of Saty across different age categories")+
  ylab('Percentage')

# Hospital_type_code with respect to length of stay Bar Plot

ggplot(data_skew3, aes(x=Hospital_type_code, fill=Stay))+
  scale_fill_manual(breaks = c("1", "0"), 
                    values=c("blue", "light blue"))+
  geom_bar(position="fill")+
  ggtitle("Length of Saty across different Hospital_type_codes")+
  ylab('Percentage')

# Department with respect to length of stay Bar Plot

ggplot(data_skew3, aes(x=Department, fill=Stay))+
  scale_fill_manual(breaks = c("1", "0"), 
                    values=c("blue", "light blue"))+
  geom_bar(position="fill")+
  ggtitle("Length of Saty across different Departments")+
  ylab('Percentage')

# Severity.of.Illness with respect to length of stay Bar Plot

ggplot(data_skew3, aes(x=Severity.of.Illness, fill=Stay))+
  scale_fill_manual(breaks = c("1", "0"), 
                    values=c("blue", "light blue"))+
  geom_bar(position="fill")+
  ggtitle("Length of Saty across different severities")+
  ylab('Percentage')


# splitting the data into 75/25 

sample <- sample.split(data_skew3$Stay, SplitRatio = 0.75)
train_data  <- subset(data_skew3, sample == TRUE)
test_data <- subset(data_skew3, sample == FALSE)


#check distribution of test train split
prop.table(table(train_data$Stay)) * 100
prop.table(table(test_data$Stay)) * 100
prop.table(table(data_skew3$Stay)) * 100

#Preprocessing for Logistic Reg

# splitting the data to X and Y

training_dataX <- train_data[,names(train_data) != "Stay"]

dummy <- dummyVars( ~ ., data=training_dataX)

# Applying one hot encoding to data for logistic regression
dummy_train <- data.frame(predict(dummy, newdata = train_data))

# splitting the data to test and train for logistic regression
dummy_test <- data.frame(predict(dummy, newdata = test_data))
dummy_test <- na.omit(dummy_test)
dummy_train <- na.omit(dummy_train)

# eliminating the null values
train_data <- na.omit(train_data)
test_data <- na.omit(test_data)

# converting the response to a factor of 0 and 1
train_data$Stay <- as.factor(as.numeric(train_data$Stay))
test_data$Stay <- as.factor(test_data$Stay)
train_data$Stay <- as.factor(train_data$Stay)
set.seed(400)

# Logistic Regression


logistic_model <- glm(train_data$Stay ~ ., 
                      data = dummy_train, 
                      family = "binomial")

summary(logistic_model)

# observing the best variables in the model
(varImp(logistic_model))

par(pty= "s")

# confusion matrix and accuracy

# training 

p_train_log <- predict(logistic_model, newdata = dummy_train, type = "response")
# converting probability to labels
p_train_log.cat <- ifelse(p_train_log > 0.5,"1","0")
p_train_log.cat <- as.factor(p_train_log.cat)
cm_log_train<-confusionMatrix(data=p_train_log.cat,reference=train_data$Stay)
cm_log_train

Accuracy_log_test<-round(cm_log_train$overall[1],2)
Accuracy_log_test

# testing

p_test_log <- predict(logistic_model, newdata = dummy_test, type = "response")

p_test_log.cat <- ifelse(p_test_log > 0.5,"1","0")
p_test_log.cat<-as.factor(p_test_log.cat)

cm_log_test<-confusionMatrix(data=p_test_log.cat,reference=test_data$Stay)
cm_log_test

Accuracy_log_test<-round(cm_log_test$overall[1],2)
Accuracy_log_test

#roc 

# taining 
roc(train_data$Stay, p_train_log, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab = "False Positive percentage", ylab = "True Positive Percentage",
    main= "ROC of Logistic Regression",
    col="#377eb8", lwd =1, print.auc = TRUE)


# testing
plot.roc(test_data$Stay, p_test_log, percent = TRUE,lwd = 1,
         col="#4daf4a", print.auc.y = 45, print.auc = TRUE, add = TRUE)
legend("bottomright",c("Training", "Testing"), col = c("#377eb8","#4daf4a"), lwd =3)


#Decision Trees

ctrl_dtree <- trainControl(method="repeatedcv",repeats = 3)
dtree_model <- train(Stay ~., 
                     data = train_data, 
                     method = "rpart",
                     parms = list(split = "information"),
                     trControl=ctrl_dtree,
                     tuneLength = 10,
                     na.action = na.pass)

dtree_model # model summary

# plotting the tree
prp(dtree_model$finalModel, box.palette = "Reds", tweak = 1.2)
plot(dtree_model, col="red")


# observing the best performing variables in the model
plot(varImp(dtree_model))

# confusion matrix and accuracy

# training data
p_train_dt <- predict(dtree_model, newdata = train_data, type = "raw")
p_train_dt.cat <- as.factor(p_train_dt)

cm_dt_train<-confusionMatrix(data=p_train_dt.cat,reference=train_data$Stay)
cm_dt_train

Accuracy_dt_test<-round(cm_dt_train$overall[1],2)
Accuracy_dt_test

# testing data
p_test_dt <- predict(dtree_model, newdata = test_data, type = "raw")
p_test_dt.cat<-as.factor(p_test_dt)

cm_dt_test<-confusionMatrix(data=p_test_dt.cat,reference=test_data$Stay)
cm_dt_test

Accuracy_dt_test<-round(cm_dt_test$overall[1],2)
Accuracy_dt_test

# roc

# training data

p_train_dt_roc <- predict(dtree_model, train_data, type = "prob")

roc(train_data$Stay, p_train_dt_roc[ ,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab = "False Positive percentage", ylab = "True Positive Percentage",
    main= "ROC of Decision Tree",
    col="#377eb8", lwd =1, print.auc = TRUE)

# testing data

p_test_dt_roc <- predict(dtree_model, test_data, type = "prob")

plot.roc(test_data$Stay, p_test_dt_roc[ ,2], percent = TRUE,lwd = 1,
         col="#4daf4a", print.auc.y = 45, print.auc = TRUE, add = TRUE)
legend("bottomright",c("Training", "Testing"), col = c("#377eb8","#4daf4a"), lwd =3)



# KNN

ctrl_knn <- trainControl(method = "cv",
                         summaryFunction = defaultSummary,
                         number = 5)
set.seed(2)
knn <- train(Stay ~ ., 
             data = train_data,
             method = "knn",
             trControl=ctrl_knn,
             metric = "Accuracy",
             tuneLength = 2,
             na.action = na.pass
)


print(knn)

# confusion matrix and accuracy

# training data
p_train_knn <- predict(knn, newdata = train_data, type = "raw")
p_train_knn.cat <- as.factor(p_train_knn)
cm_knn_train<-confusionMatrix(data=p_train_knn.cat,reference=train_data$Stay)
cm_knn_train

Accuracy_knn_test<-round(cm_knn_train$overall[1],2)
Accuracy_knn_test

# testing data
p_test_knn <- predict(knn, newdata = test_data, type = "raw")
p_test_knn.cat<-as.factor(p_test_knn)
cm_knn_test<-confusionMatrix(data=p_test_knn.cat,reference=test_data$Stay)
cm_knn_test

Accuracy_knn_test<-round(cm_knn_test$overall[1],2)
Accuracy_knn_test

# roc

# training data

p_train_knn_roc <- predict(knn, train_data, type = "prob")

roc(train_data$Stay, p_train_knn_roc[ ,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab = "False Positive percentage", ylab = "True Positive Percentage",
    main= "ROC of KNN",
    col="#377eb8", lwd =1, print.auc = TRUE)

# testing data

p_test_knn_roc <- predict(knn, test_data, type = "prob")

plot.roc(test_data$Stay, p_test_knn_roc[ ,2], percent = TRUE,lwd = 1,
         col="#4daf4a", print.auc.y = 45, print.auc = TRUE, add = TRUE)
legend("bottomright",c("Training", "Testing"), col = c("#377eb8","#4daf4a"), lwd =3)
# naive bayes

naive_bayes <- train(Stay ~ ., 
                     data = train_data,
                     method = "naive_bayes",
                     usepoisson = TRUE,
                     na.action = na.pass)

# Viewing the model
naive_bayes

# confusion matrix and accuracy

# training data
p_train_nb <- predict(naive_bayes, newdata = train_data, type = "raw")
p_train_nb.cat <- as.factor(p_train_nb)
cm_nb_train<-confusionMatrix(data=p_train_nb.cat,reference=train_data$Stay)
cm_nb_train

Accuracy_nb_test<-round(cm_nb_train$overall[1],2)
Accuracy_nb_test

# testing data
p_test_nb <- predict(naive_bayes, newdata = test_data, type = "raw")
p_test_nb.cat<-as.factor(p_test_nb)

cm_nb_test<-confusionMatrix(data=p_test_nb.cat,reference=test_data$Stay)
cm_nb_test

Accuracy_nb_test<-round(cm_nb_test$overall[1],2)
Accuracy_nb_test

# roc

# training data

p_train_nb_roc <- predict(naive_bayes, train_data, type = "prob")

roc(train_data$Stay, p_train_nb_roc[ ,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab = "False Positive percentage", ylab = "True Positive Percentage",
    main= "ROC of Naive Bayes",
    col="#377eb8", lwd =1, print.auc = TRUE)

# testing data

p_test_nb_roc <- predict(naive_bayes, test_data, type = "prob")

plot.roc(test_data$Stay, p_test_nb_roc[ ,2], percent = TRUE,lwd = 1,
         col="#4daf4a", print.auc.y = 45, print.auc = TRUE, add = TRUE)
legend("bottomright",c("Training", "Testing"), col = c("#377eb8","#4daf4a"), lwd =3)


# out of all models decision tree has the highest accuracy of 79%
