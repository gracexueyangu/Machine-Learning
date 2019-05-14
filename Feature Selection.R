library(data.table)
library(caret)
library(dplyr)
library(stringr)
library(leaps)
library(ISLR)

train = fread('FETrain.csv')
test = fread('FETest.csv')

# Combine train and test data together
data = merge(train,test,all = TRUE)[,-'is_active']
str(data)

# Change categorical variables into factors. (Remove is_active since all of them are 1)
data$MEMBERSHIP_TYPE_CODE = as.factor(data$MEMBERSHIP_TYPE_CODE)
data$TENURE_GRP = as.factor(data$TENURE_GRP)
data$fav_area = as.factor(data$fav_area)
data$spendmost_ctg = as.factor(data$spendmost_ctg)
data$regular_gas = as.factor(data$regular_gas)
data$regular_pharmacy = as.factor(data$regular_pharmacy)
data$regular_fur = as.factor(data$regular_fur)
data$regular_opt = as.factor(data$regular_opt)
data$regular_heal = as.factor(data$regular_heal)
data$target_label = as.factor(data$target_label)


# Selection
regfit.full = regsubsets(target_label~.,data = data,really.big = T)
reg.summary = summary(regfit.full)

# Plot
par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",type="l")
plot(reg.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
# which.max(reg.summary$adjr2)
points(which.max(reg.summary$adjr2),
       reg.summary$adjr2[which.max(reg.summary$adjr2)], col="red",cex=2,pch=20)
plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
# which.min(reg.summary$cp )
points(which.min(reg.summary$cp ),
       reg.summary$cp [which.min(reg.summary$cp)],col="red",cex=2,pch=20)
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",type='l')
# which.min(reg.summary$bic )
points(which.min(reg.summary$bic ),
       reg.summary$bic [which.min(reg.summary$bic)],col="red",cex=2,pch=20)



