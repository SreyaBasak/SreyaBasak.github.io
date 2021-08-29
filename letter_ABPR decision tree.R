#SGN
rm(list = ls())
#-----------------------------------Decision Trees in R------------------------------#

library(caret)
library(ggplot2)
library(rpart.plot)
library(pROC)
library(ROCR)
library(rpart)
library(randomForest)
library(caTools)
library(e1071)


#-------------------------Setting the working directory and Reading the dataset--------------------------------------------#

Path<-"D:/IVY/R/Assignments"
setwd(Path)
getwd()

letters=read.csv("letters_ABPR.csv",stringsAsFactors = TRUE)
head(letters)

#-----------------------Basic Exploration of the Data Set----------------------------------------------------------#

dim(letters)
str(letters)

summary(letters)
sapply(letters, function(x) sum(is.na(x)))
colSums(is.na(letters))
letters$letter<-as.factor(letters$letter)
str(letters$letter)

#------------------------------Splitting the dataset into train and test data-----------------------#


set.seed(1000)
spl = sample.split(letters$letter, SplitRatio = 0.7)
Train = subset(letters, spl==TRUE)
dim(Train)
str(Train)

Test = subset(letters, spl==FALSE)
dim(Test)
str(Test)

#-------------------------------------------Building the CART model----------------------------------------------#

CART1<-rpart(letter~.,data=Train, method = "class")
prp(CART1)
CART1


#Using the Complexity Parameter
CART2<-rpart(letter~.,data=Train, method = "class",minbucket = 100,cp = 0.001)
prp(CART2)
CART2



#-------------------------Checking the accuracy of the model in the test data------------------------------#
predictCART1<-predict(CART1, newdata=Test, type = "class")
table(Test$letter,predictCART1)
(9117+1676)/(9117+596+1402+1676)

(6933+1214)/(6933+352+1095+1214)

#ConfusionMatrix
confusionMatrix(predictCART1,Test$letter)

#----------------Reciever Operating Characterstics Curve for CART------------------------------------------#

predictCART2<-predict(CART1, newdata=Test)#To predict the probabilities for the observations in the test data set


CARTroc<-roc(response=Test$letter, predictor = predictCART2[,2],
             level = rev(levels(Test$letter)))
CARTroc
plot(CARTroc)
# Area under the curve is 0.84


1402+1676
832+2246


#---------------------------End of the CART model---------------------------------------------------------#



#------------------------------------A Random Forest Model-------------------------------------------------#

#Reducing the train data sample size, to limit the train data set to contain randomly chosen 2000 data points


set.seed(1000)
spl = sample.split(letters$letter, SplitRatio = 0.7)
Train_1 = subset(letters, spl==TRUE)
dim(Train_1)
str(Train_1)
#Train_1$letter<-NULL

Test_1 = subset(letters, spl==FALSE)
dim(Test)
str(Test)
#Test_1$letter<-NULL


PredictForest1<-randomForest(letter~.,data = Train_1, ntree = 1000)
?randomForest
PredictForest1
nrow(Test)
(13577+2959)/(13577+993+1658+2959)

#--------------------Checking the accuracy of the model-------------------------------------------#
predForest1<-predict(PredictForest1, newdata=Test_1, type = "class")
Test_1$Y_pred<-predict(PredictForest1, newdata=Test_1, type = "class")

length(predForest1)
length(Test_1$letter)

table(Test_1$letter,predForest1)
#nrow(trainsmall)
(6819+1511)/(6819+466+798+1511)

#ConfusionMatrix
confusionMatrix(predForest1,Test_1$letter)
varImp(PredictForest1)

#---------------------------Variable Importance chart in Random Forest---------------------------------------#
vu = varUsed(PredictForest1, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(PredictForest1$forest$xlevels[vusorted$ix]), main = "Variable Importance Chart_Splitting")

#Interpretation: Here, 'Age' variable is most important in terms of number of splits

#each variable measures the number of times that variable was selected for splitting (the value on the x-axis)

#--------------------Measuring Impurity in the Random Forest Model-----------------------------------------#

#A different metric we can look at is related to "impurity", which measures how homogenous each bucket or leaf of the tree is. 
#In each tree in the forest, whenever we select a variable and perform a split, the impurity is decreased. 
#Therefore, one way to measure the importance of a variable is to average the reduction in impurity, 
#taken over all the times that variable is selected for splitting in all of the trees in the forest. 
varImpPlot(PredictForest1, main = "Variable Importance Chart_Impurity Red")

#Interpretation: Here, 'Capitalgain' variable is most important in terms of mean reduction in impurity




