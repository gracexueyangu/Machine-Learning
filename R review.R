#Review R

#1. dplyr
library(dplyr)
library(hflights)
library(ggplot2)

data = hflights

df2 = select(hflights, ActualElapsedTime, AirTime, ArrDelay, DepDelay)
head(df2)

df3 = data %>%
  select(Origin:Cancelled)

df4 = data %>%
  select(-(DepTime:AirTime))
df5 = data %>%
  select(ends_with("Delay"))

df6 = data %>%
  select(UniqueCarrier, FlightNum, TailNum, starts_with("Cancel"))

df7 = data %>%
  mutate(GroudTime = TaxiIn+TaxiOut)

df8 = data %>%
  filter(Distance >= 3000)

df9 = data %>%
  filter(UniqueCarrier %in% c("AA","AS","B6"))

df10 <- data %>%
  filter(TaxiIn+TaxiOut>AirTime)

df11 <- data %>%
  filter(Cancelled == 1 & DayOfWeek %in% c(6,7))

df12 <- data %>%
  arrange(UniqueCarrier, desc(DepDelay))

df13 <- data %>%
  arrange(ArrDelay+DepDelay)

df14 <- data %>%
  filter(Dest == "DFW" & ArrTime < 800) %>%
  arrange(desc(AirTime))


data %>%
  summarise(max_dist = max(Distance),
            min_dist = min(Distance))

data %>%
  filter(Diverted == 1) %>%
  summarise(max_div = max(Distance))

data %>%
  summarise(n_obs = n(),
            n_carrier = n_distinct(UniqueCarrier),
            n_dest = n_distinct(Dest),
            dest100 = nth(Dest, 100))

data %>%
  mutate(diff = TaxiIn-TaxiOut) %>%
  filter(!is.na(diff)) %>%
  summarise(avg = mean(diff, na.rm = T))

d <- data %>%
  mutate(RealTime = ActualElapsedTime + 100) %>%
  select(Dest, UniqueCarrier, Distance,ActualElapsedTime, RealTime)

df20 <- data %>%
  group_by(Dest) %>%
  summarise(count = n(),
            avg = mean(Distance, na.rm = T),
            avg2 = mean(ArrDelay, na.rm = T))

data %>%
  group_by(Dest) %>%
  summarise(count = n(),
            avg = mean(Distance, na.rm = T),
            avg2 = mean(ArrDelay, na.rm = T)) %>%
  filter(avg < 2000) %>%
  ggplot(aes(avg, avg2)) +
  geom_point() +
  geom_smooth()

df21 <- data %>%
  group_by(UniqueCarrier) %>%
  summarise(count = n(),
            n_canc = sum(Cancelled == 1),
            avg_delay = mean(ArrDelay, na.rm = T)) %>%
  arrange(avg_delay, n_canc)

head(df21)
str(hflights)
summary(hflights)


data2 = diamonds
str(data2)

data2 %>%
  group_by(cut) %>%
  summarise(avg = mean(price,na.rm = T))

#2. basic function
x <- c(4,5,6)
length(x)
ls()
x1 <- matrix(data = c(1,2,3,4), nrow = 2, ncol = 2)
sqrt(x)
x2 <- rnorm(50)
y = x2+rnorm(50, mean = 50, sd = 0.1)
cor(x2,y)

set.seed(1)
x = runif(50) #0~1

set.seed(1303)
rnorm(50)

set.seed(3)
y = rnorm(100)
mean(y)
var(y)
sqrt(var(y))


x = rnorm(100)
y = rnotm(100)
plot(x,y)

a = matrix(1:16,4,4)
a[1,]

data = read.csv('galton.csv', row.names = 1)


#3. linear regression
library(MASS)
library(ISLR)

fix(Boston)
names(Boston)
?Boston

# Part 1 Simple linear regression
lm.fit = lm(medv~lstat,data = Boston)


plot(medv, lstat)
lines(lstat, lm.fit$fitted.values, col = "blue",lwd = 4)
attach(Boston)
lm.fit = lm(medv~lstat)
lm.fit

summary(lm.fit )
names(lm.fit)
lm.fit$coefficients
confint(lm.fit)
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "confidence")
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "prediction")

plot(lstat, medv)
abline(lm.fit,lwd = 3, col = "red")

par(mfrow = c(2,2))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))


# Part 2 Multiple linear regression

lm.fit = lm(medv~lstat + age, data = Boston)
summary(lm.fit)

lm.fit = lm(medv~., data=Boston)
summary(lm.fit)

lm.fit = lm(medv~.-age, data = Boston)
summary(lm.fit)

fix(Carseats)
names(Carseats)

lm.fit2 = lm(Sales~.+Income:Advertising+Price:Age, data = Carseats)
summary(lm.fit2)

attach(Carseats)
contrasts(ShelveLoc)

# functions

load = function(){
  library(MASS)
  library(ISLR)
  print("All are loaded")
}

load()



# Exercise


lm.fit = lm(mpg~horsepower, data = Auto)
summary(lm.fit)

predict(lm.fit, data.frame(horsepower = 98))



attach(Auto)
plot(horsepower, mpg)
abline(lm.fit,col = "red")
lines(horsepower,lm.fit$fitted.values, col = "blue" )

par(mfrow = (c(2,2)))
plot(lm.fit)

pairs(Auto)
names(Auto)
cor(Auto[,-9])

lm.fit2 = lm(mpg~.-name, data = Auto)
summary(lm.fit2)

par(mfrow = c(2,2))
plot(lm.fit2)

lm.fit3 = lm(mpg~cylinders*displacement,data= Auto[1:8])
summary(lm.fit3)

par(mfrow = c(2,2))
plot(log(Auto$horsepower), Auto$mpg)

plot(sqrt(Auto$horsepower), Auto$mpg)

plot((Auto$horsepower)^2, Auto$mpg)

lm.fit3 = lm(Sales~Price+Urban+US, data = Carseats)
summary(lm.fit3)

lm.fit4 = lm(Sales~Price+US, data = Carseats)
summary(lm.fit4)
confint(lm.fit4)

par(mfrow = c(2,2))
plot(lm.fit4)

#4. Manipulate the data - for loop

df = read.csv("FirstCleaned.csv")

class(df$Merch.state)

temp = levels(as.factor(df$Merch.state))

l = list()
for (i in temp){
  if (!(i %in% state.abb)){l<-append(l,i)}
}

x <- c(2,5,3,9,8,11,6)
count <- 0
for (val in x) {
  if(val %% 2 == 0)  count = count+1
  else if (val %%2 == 1) count = count
}
print(count)


for (v in x){
  if (v %% 2 == 0) count = count+1
}
print(count)




# 5. train and test

library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket[,-9])

attach(Smarket)
train = (Year < 2005)

Smarket_train = Smarket[train,]
Smarket_test = Smarket[!train,]

lm_train = lm(Today~.,data = Smarket_train)
lm_train_2 = lm(Today~., data = Smarket, subset = train)
summary(lm_train_2)


predict = predict(lm_train, Smarket_test)
sqrt(mean((Smarket_test[,"Today"]-predict)^2))

lm_train$coefficients
summary(lm_train)

# 6. apply
m1 = matrix(C<-(1:10),nrow=5, ncol=6)
m1
a_m1 = apply(m1, 1, sum)
a_m1

#7. if

#if( condition ) statement