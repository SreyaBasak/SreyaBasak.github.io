library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(caTools)
library(dplyr)

#--------------------------------Setting the Working Directory-----------------------------------------#
Path<-"D:/IVY/R/Assignments"
setwd(Path)
getwd()

data=read.csv("elantra.csv")
data1=data

#------------------------------------Basic Exploration of the data--------------------------------------------# 
str(data1)
summary(data1)
dim(data1)


#Renaming the Dependent var
colnames(data1)[which(names(data)=="ElantraSales")]="esales"



#-------------->Outlier Treatment through quantile method

quantile(data1$esales,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))

options(scipen = 999)

library(pastecs)
stat.desc(data1)

#-------------->Missing values Identification and Treatment

as.data.frame(colSums(is.na(data1)))

#data2=subset(data1,select = -c(Month,Year))


#Exporting the treated data into csv file
write.csv(data1,"Elantradata1.csv",row.names = FALSE)
data1$Month <- as.factor(data1$Month)

#--------------------------Splitting the data into training and test data set------------------------#
set.seed(123)#This is used to produce reproducible results, everytime we run the model

#Split the data set into training and testing sets as follows: 
#place all observations for 2012 and earlier in the training set
# all observations for 2013 and 2014 into the testing set

original.data = data1[data1$Year<=2012,]
str(original.data)
dim(original.data)

#36 obs are in the training dataset (72%)

test.data = data1[data1$Year>2012,]
str(test.data)
dim(test.data)

#14 obs are in the test dataset (28%)

#------------------------------------------Fitting the model---------------------------------------#
#Iteration.1 We start with testing all variables
options(scipen = 999)
options(digits=2)

#Q2
LinearModel1=lm(esales~Unemployment+CPI_energy +CPI_all+Queries, data=original.data)
summary(LinearModel1)
#Multiple R-squared:  0.428,	Adjusted R-squared:  0.354
#only 1 value is significant i.e. Queries with a p-value of 0.10

LinearModel1$coefficients

# coefficient of unemployment = -3180

#Q5
#Iteration.2
LinearModel2=lm(esales~Unemployment+Queries+CPI_energy +CPI_all+Month, data=original.data)
summary(LinearModel2)

#Removing the first insignificant variable: Queries
#Iteration.3
LinearModel3=lm(esales~Unemployment+CPI_energy +CPI_all+Month, data=original.data)
summary(LinearModel3)

#Iteration.4 Removing Month2
LinearModel4=lm(esales~Unemployment+CPI_energy +CPI_all+I(Month==3)+I(Month==4)+I(Month==5)+I(Month==6)+I(Month==7)+I(Month==8)+I(Month==9)+I(Month==10)+I(Month==11)+I(Month==12), data=original.data)
summary(LinearModel4)

#Iteration.5 Removing Month11
LinearModel5=lm(esales~Unemployment+CPI_energy +CPI_all+I(Month==3)+I(Month==4)+I(Month==5)+I(Month==6)+I(Month==7)+I(Month==8)+I(Month==9)+I(Month==10)+I(Month==12), data=original.data)
summary(LinearModel5)

#Iteration.6 Removing Month10
LinearModel6=lm(esales~Unemployment+CPI_energy +CPI_all+I(Month==3)+I(Month==4)+I(Month==5)+I(Month==6)+I(Month==7)+I(Month==8)+I(Month==9)+I(Month==12), data=original.data)
summary(LinearModel6)

#Multiple R-squared:  0.788,	Adjusted R-squared:  0.69 

#Checking Multicollinearity in the model

## Get the predicted or fitted values
ModelVif<-as.data.frame(vif(LinearModel8))
LinearModel7=lm(esales~Unemployment+CPI_energy +I(Month==3)+I(Month==4)+I(Month==5)+I(Month==6)+I(Month==7)+I(Month==8)+I(Month==9)+I(Month==12), data=original.data)
summary(LinearModel7)

ncvTest(lm(esales~Unemployment+CPI_energy +I(Month==3)+I(Month==4)+I(Month==5)+I(Month==6)+I(Month==7)+I(Month==8)+I(Month==9)+I(Month==12), data=original.data))
LinearModel8=lm(esales~CPI_energy +I(Month==3)+I(Month==4)+I(Month==5)+I(Month==6)+I(Month==7)+I(Month==8)+I(Month==9)+I(Month==12), data=original.data)
summary(LinearModel8)

LinearModel9=lm(esales~CPI_energy +I(Month==3)+I(Month==4)+I(Month==5)+I(Month==6)+I(Month==7)+I(Month==8), data=original.data)
summary(LinearModel9)
durbinWatsonTest(LinearModel9)

bptest(LinearModel9)
## Get the predicted or fitted values
fitted(LinearModel9)

par(mfrow=c(2,2))
plot(LinearModel9)



## MAPE
original.data$pred <- fitted(LinearModel9)
Actual_Pred<-select(original.data,c(esales,pred))
Actual_Pred$error<-Actual_Pred$esales-Actual_Pred$pred
summary(Actual_Pred$error)
write.csv(original.data,"mape.csv")

#Multiple R-squared:  0.686,	Adjusted R-squared:  0.607 

#Calculating MAPE
#We do not use attach and detach together, Click on global environment to remove the original.data files use detach several times

attach(original.data)

MAPE<-print((sum((abs(esales-pred))/esales))/nrow(original.data))
#detach(original.data)


#Using the Iteration on the test data
LinearModelTest1=lm(esales~CPI_energy +I(Month==3)+I(Month==4)+I(Month==5)+I(Month==6)+I(Month==7)+I(Month==8), data=test.data)
summary(LinearModelTest1)

LinearModelTest2=lm(esales~I(Month==3)+I(Month==4)+I(Month==5)+I(Month==6)+I(Month==7)+I(Month==8), data=test.data)
summary(LinearModelTest2)

LinearModelTest3=lm(esales~I(Month==3)+I(Month==4)+I(Month==5)+I(Month==7)+I(Month==8), data=test.data)
summary(LinearModelTest3)

TestVif<-as.data.frame(vif(LinearModelTest3))
Pred_Sales<-predict(LinearModelTest3)
test.data$pred_Sales<-Pred_Sales

#test set R-squared:  0.685,	Adjusted R-squared:  0.488

#Calculating MAPE
test.data$error<-abs(test.data$esales-test.data$pred_Sales)
summary(test.data$error)

durbinWatsonTest(LinearModelTest3)


# Checking multicollinearity
vif(LinearModelTest3) # should be within 2. If it is greater than 10 then serious problem

################ Constant error variance ##########Heteroscedasticity


# Breusch-Pagan test
bptest(LinearModelTest3)  # Null hypothesis -> error is homogenious (p value should be more than 0.05)


#Cook-Weisberg test
# hypothesis of constant error variance against the alternative that the error variance changes with the level of the  response 
# p value should be more than 0.05
ncvTest(lm(esales~I(Month==3)+I(Month==4)+I(Month==5)+I(Month==7)+I(Month==8), data=test.data))


#max error = 5080 , Month=1, Year=2013
