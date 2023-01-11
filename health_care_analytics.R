#San Diego State University
#MIS 720 - E-Business Infrastructures: Data Science and Big Data
#Term Project
#Length of hospital stay prediction
#Team: Aztec Data Analytics
#Max Gueniau, Akshaya Viswanathan, Najah Izquierdo, Naveen Reddy Sama, Tobias Kleinhansl

#---------------------------------

#1 Packages
library(ggplot2)
library(caret)
install.packages(rsample)

#2 Data Preparation

#Import an first look
data_raw <- read.csv('train.csv')
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


#Drop row case_id, patientid since its just there for numbering the rows and holds no value for predicting the stay
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

