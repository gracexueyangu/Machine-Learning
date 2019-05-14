library(randomForest)
library(ISLR)
library(caret)
library(ggplot2)
library(pROC)

train_firstyr = read.csv('train_70_1styr.csv')
test_firstyr = read.csv('test_30_1styr.csv')
train_firstyr$target_label = as.factor(train_firstyr$target_label)
test_firstyr$target_label = as.factor(test_firstyr$target_label)
train_firstyr = train_firstyr %>% select(-X)
test_firstyr = test_firstyr %>% select(-X)

#model: randomForest
rf.train.firstyr = randomForest(target_label~.,data=train_firstyr,mtry=2,ntree=300)


yhat.rf.train = predict(rf.train.firstyr)
sum(yhat.rf.train==train_firstyr$target_label)/nrow(train_firstyr) #0.68

yhat.rf.test = predict(rf.train.firstyr, newdata = test_firstyr,type="response")
sum(yhat.rf.test==test_firstyr$target_label)/nrow(test_firstyr) #0.58

roc(train_firstyr$target_label,rf.train.firstyr$votes[,1],plot=TRUE)
plot.roc(train_firstyr$target_label,rf.train.firstyr$votes[,1],percent=TRUE,col='#4daf4a',lwd=4,print.auc=TRUE)
#model logistic
glm.fit = glm(target_label~., data = train_firstyr, family=binomial)
roc(train_firstyr$target_label,glm.fit$fitted.values,plot=TRUE)
glm.probs=predict(glm.fit, type='response')
glm.pred=rep("0",1926)
glm.pred[glm.probs>0.5]="1"
mean(glm.pred==train_firstyr$target_label) #0.7030114

glm.probs.test=predict(glm.fit, test_firstyr,type='response')
glm.pred.test=rep("0",826)
glm.pred.test[glm.probs.test>0.5]="1"
mean(glm.pred.test==test_firstyr$target_label) #0.6598063

coef(glm.fit)


#feature importance of RF
randomForest::importance(rf.train.firstyr)
varImpPlot(rf.train.firstyr)
library(ggplot2)

#data exloration
##60% of 1st member will renew
firstyr = rbind(train_firstyr,test_firstyr)
mean(firstyr$target_label==1) #0.59375

#total_itemslastm
firstyr %>%
  group_by(target_label) %>%
  summarise(avg = mean(total_itemslastm)) %>%
  

#target_label   avg
#0             6.73
#1            28.6 


#total_items
firstyr %>%
  group_by(target_label) %>%
  summarise(avg = mean(total_items)) 

#target_label   avg
#0             81.0
#1             187.

#most_ctg_ratio
firstyr %>%
  group_by(target_label) %>%
  summarise(avg = mean(most_ctg_ratio)) 
##target_label   avg
##     0 0.352
##     1 0.286
                                                                                                                 

#total_visitlast6m
firstyr %>%
  group_by(target_label) %>%
  summarise(avg = mean(total_visitlast6m)) 

#target_label   avg
#0             5.98
#1             15.6

#total_visit
firstyr %>%
  group_by(target_label) %>%
  summarise(avg = mean(total_visit)) 

#target_label   avg
#0             13.4
#1             28.7

#total_visitlastm
firstyr %>%
  group_by(target_label) %>%
  summarise(avg = mean(total_visitlastm)) 

#target_label   avg
#0             1.28
#1             4.55

library(dplyr)
train = read.csv('FETrain.csv')
test = read.csv('FETest.csv')

data=rbind(train,test)
data_firstyr = data %>%
  filter(TENURE_GRP == '1')
data_firstyr$target_label = as.factor(data_firstyr$target_label)
data$target_label = as.factor(data$target_label)

comparison=list()

total_itemslastm = data_firstyr %>%
  group_by(target_label) %>%
  summarise(avg_total_itemslastm = mean(total_itemslastm)) 

comparison['total_itemslastm'] = (total_itemslastm[2,'avg_total_itemslastm']-total_itemslastm[1,'avg_total_itemslastm'])/total_itemslastm[1,'avg_total_itemslastm']

total_visitlastm = data_firstyr %>%
  group_by(target_label) %>%
  summarise(avg_total_itemslastm = mean(total_visitlastm)) 

comparison['total_visitlastm'] = (total_visitlastm[2,'avg_total_itemslastm']-total_visitlastm[1,'avg_total_itemslastm'])/total_visitlastm[1,'avg_total_itemslastm']
comparison = data.frame(comparison)

data_firstyr %>%
  group_by(target_label) %>%
  summarise(avg_most_ctg_ratio = mean(most_ctg_ratio)) %>%
  ggplot(aes(x=target_label,y=avg_most_ctg_ratio)) +
  geom_col() +
  theme_classic()

data_firstyr %>%
  filter(target_label==1) %>%
  summarise(count = n()) 

data_firstyr %>%
  filter(target_label==1) %>%
  summarise(count = sum(regular_heal)) 
