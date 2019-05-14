#Week 1
#rmd和导入的数据必须要放在一个文件夹里！！！！！

classes = c("DSO510","DSO545")
grades = c(80,95)
student = data.frame(classes,grades)
colnames(student)
length(classes)
nrow(student)
ncol(student)
seq(from = 1, to =10, by =2)
seq(1:10)
rep("hi",10)
data = read.csv("stateData.csv")
nrow(data[data$region == 1,])
data[data$illiteracy < 0.7,]
data[data$region == 1 & data$highSchoolGrad<=50,]
data[data$murder == max(data$murder),]


#Week 2
library(ggplot2)
head(mpg)
?(mpg)
ggplot(mpg, aes(x = displ,y = hwy)) +
  geom_point() +
  facet_wrap(~class,nrow = 2)
str(diamonds)

ggplot(diamonds, aes(x = carat)) +
  geom_bar()
library(gcookbook)
str(BOD)
ggplot(BOD, aes(x = as.factor(Time), y = demand)) +
  geom_col(fill = "lightblue", width = 0.5) +
  geom_text(aes(label = demand), vjust = 2, color = "red")

ggplot(BOD, aes(x = as.factor(Time), y = demand)) +
  geom_col(fill = "lightblue", width = 0.5) +
  coord_flip()
ggplot(diamonds, aes(x = cut, fill = clarity)) +
  geom_bar(position = "dodge",color = "black")
str(uspopchange)

a = uspopchange[order(-uspopchange$Change),]
b = a[1:10,]
ggplot(b, aes(x = reorder(Abb,-Change), y = Change, fill = Region)) +
  geom_col() +
  scale_fill_manual(values = c("pink","lightblue"))
str(climate)
subclimate = climate[climate$Year >= 1900 & climate$Source == "Berkeley",]
subclimate$Pos = ifelse(subclimate$Anomaly10y>=0,T,F)
ggplot(subclimate,aes(x = Year, y = Anomaly10y,fill = Pos)) +
  geom_col() +
  scale_fill_manual(values = c("lightblue","red"),guide = F)
str(tophitters2001)
c = tophitters2001[1:10,c("name","avg")]

ggplot(c,aes(x = avg, y = reorder(name,avg))) +
  geom_point(size = 3)


#Week 3
library(dplyr)
library(hflights)
df2 = select(hflights, -Year)
df7 = mutate(hflights, groundtime = TaxiIn+TaxiOut)
df8 = hflights %>%
  filter(Distance> 3000)
df9 = hflights %>%
  filter(UniqueCarrier %in% c("AA","AS", "B6"))
df10 = df7 %>%
  filter(groundtime > AirTime)
df11 = hflights %>%
  filter(DayOfWeek %in% c(6,7) & Cancelled == 1)
df12 = arrange(hflights, UniqueCarrier, desc(DepDelay))

View(hflights)

df14 = hflights %>%
  filter(Dest == "DFW" & DepTime < 800) %>%
  arrange(-AirTime)
head(df14)

df18 = hflights %>%
  mutate(diff = TaxiIn - TaxiOut) %>%
  filter(!is.na(diff)) %>%
  summarise(avg = mean(diff, na.rm = T))
head(df18)
df19 = hflights %>%
  select(Dest, UniqueCarrier, Distance, ActualElapsedTime) %>%
  mutate(Realtime = ActualElapsedTime + 100)
head(df19)

df20 = hflights %>%
  group_by(Dest) %>%
  summarise(count = n(),
            mean = mean(Distance, na.rm = T),
            mean1 = mean(ArrDelay, na.rm = T))
head(df20)

hflights %>%
  group_by(Dest) %>%
  summarise(count = n(),
            mean = mean(Distance, na.rm = T),
            mean1 = mean(ArrDelay, na.rm = T)) %>%
  filter(mean < 2000) %>%
  ggplot(aes(x = mean,y = mean1)) +
  geom_point() +
  geom_smooth()
df21 = hflights %>%
  group_by(UniqueCarrier) %>%
  summarise(count = n(),
            n_canc = sum(Cancelled == 1), 
            avg_delay = mean(ArrDelay, na.rm = TRUE)) %>% 
  arrange(avg_delay, n_canc)
head(df21)

#Week 4
data = diamonds %>%
  group_by(cut) %>%
  summarise(mean = mean(price))


diamonds %>%
  group_by(cut) %>%
  summarise(mean = mean(price)) %>%
  ggplot(aes(x = reorder(cut,-mean), y = mean)) +
  geom_col()


data = diamonds
data$pricecategory = ifelse(data$price <= 1000,"Low",
                            ifelse(data$price <= 2400,"Medium","High"))
ggplot(data, aes(x = pricecategory)) +
  geom_bar()

data$pricecategory = factor(data$pricecategory, levels = c("Low", "Medium", "High"))

View(faithful)
ggplot(faithful, aes(x = waiting)) +
  geom_histogram(binwidth = 10, fill = "lightblue",color = "black")

library(MASS)
View(birthwt)

data = birthwt
data$smoke = factor(data$smoke)
data$smoke = ifelse(data$smoke == 0,"No smoker","Smoker")
ggplot(data, aes(x = bwt)) +
  geom_histogram() +
  facet_wrap(~smoke)

data$race = factor(data$race)
data$race = ifelse(data$race == 1,"White",
                   ifelse(data$race == 2,"Black","Other"))
ggplot(data, aes(x = bwt)) +
  geom_histogram() +
  facet_wrap(~race)
data$race = factor(data$race, levels = c("White","Black","Other"))


ggplot(data, aes(x = bwt, fill = smoke)) +
  geom_histogram(position = "identity", alpha = 0.4,fill = "red")

ggplot(faithful, aes(x = waiting,y = ..density..)) +
  xlim(35,105)+
  geom_histogram(fill = "lightblue",color = "black",size = 0.2)+
  geom_line(stat = "density") 

ggplot(data, aes(x = race,y = bwt)) +
  geom_boxplot() +
  stat_summary(fun.y = "mean",geom = "point",shape = 23, fill = "red")

ggplot(data, aes(x = 1,y = bwt)) +
  geom_boxplot() +
  scale_x_continuous(breaks = NULL)

ggplot(BOD,aes(x = factor(Time), y = demand,group = 1)) +
  geom_line() +
  geom_point()+
  scale_x_discrete(name = "", breaks = 1:7,labels = paste("Min",1:7))+
  scale_y_continuous(name = "Demand",breaks = 8:20,labels = 8:20)+
  geom_vline(xintercept = 3, color = "red", linetype = "dashed")+
  theme_bw()

tg = ToothGrowth %>%
  group_by(supp, dose) %>%
  summarise(mean = mean(len))

ggplot(tg, aes(x = factor(dose), y = mean, linetype = supp, group = supp, color = supp)) +
  geom_line()

ggplot(uspopage,aes(x = Year, y = Thousands, fill = AgeGroup)) +
  geom_area()
a = uspopage %>%
  group_by(Year) %>%
  mutate(percent = Thousands/sum(Thousands)*100)
ggplot(a,aes(x = Year, y = percent, fill = AgeGroup)) +
  geom_area() +
  geom_line(position = "stack")

##Week 5
library(lubridate)

ymd(c("20180923","2018.08.23"))
data = lakers
data$date = ymd(data$date)
ggplot(data, aes(x = date, y = 0,color = game_type)) +
  geom_point()
date = now()
wday(date)
wday(date, label = T, abbr = F)
start = ymd_hms("2018.09.11 12:00:00")
start = start + minutes(1)

load("collisiondata.rda")
data = collision
times = paste(data$Collision.date,data$Collision.Time,sep = " ")
data$times = dmy_hms(times)

pede = filter(data, Involved.With == "Pedestrian")
ggplot(pede, aes(x = wday(times, label = T, abbr = F))) +
  geom_bar()

ggplot(pede, aes(x = month(times, label = T, abbr = T))) +
  geom_bar()

pede %>%
  mutate(day = wday(times, label = T, abbr = F),
         hour = hour(times)) %>%
  group_by(day, hour) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = day,y = hour, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white",
                      high = "darkred")
date = now()
round_date(date, "hour")
data = economics
data$date = ymd(data$date)
ggplot(data, aes(x = date, y = unemploy)) +
  geom_line() +
  scale_x_date(date_breaks = "10 years")


date = ymd("2018-11-01")
wday(date,label = T)
date = date + weeks(3)
date

###Week 6
library(maps)
library(ggplot2)
library(dplyr)
state = map_data("state")
ggplot(state, aes(x = long, y = lat,group = group)) +
  geom_polygon(fill = "white",color = "black")
world = map_data("world")
ggplot(world, aes(x = long, y = lat,group = group)) +
  geom_polygon(fill = "white",color = "black")

map_data("world") %>%
  filter(region == "China") %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white",color = "black")



View(USArrests)
crime = USArrests
crime$state = rownames(crime)
crime$state = tolower(crime$state)
crime_data = left_join(state, crime, by = c("region" = "state"))
ggplot(crime_data,aes(x = long, y = lat, group = group,fill = Assault)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "white",
                   high = "darkred") +
  theme_void()

###Week 7 shiny


###Week 8
library(stringr)
cat("\"")
cat(":-\\")
cat("(ˆ_ˆ\")")
cat("@_'-'")
cat("\\m/")
cat("\"")
?str_locate
?str_sub
str_locate_all("banana",pattern = "n")
str_sub("banana",start = 3, end = 4)
vec = c("great","fantastic","super")
str_locate_all(vec, "a")
str_sub("testing",start = 1, end = 3)
str_sub("testing",start = 4)
str_sub("testing",start = 1, end = 4)
str_sub("testing")
input = c("abc","defg")
str_sub(input, start = c(2, 3))

email = readRDS("email.rds")
str(email)
length(email)
email[1]
cat(email[1])
str_locate(email[1], pattern = "\n\n")
header1 = str_sub(email[1], end = 842)
body1 = str_sub(email[1],start = 843)
str_split(header1,"\n")

breaks = str_locate(email, pattern = "\n\n")
header = str_sub(email, end = breaks[,1])
body = str_sub(email, start = breaks[,2])


fruit = c("apple", "banana", "pear", "pinapple")
str_detect(fruit, pattern = "^a[a-z]*e$")

cat(body[10])
phone4 = c("213 740 4826", "213 740-4826","213-740-4826", 
           "2137404826", "(213) 740 4826")

str_detect(phone4, "[(]?[0-9]{3}[- )]*[0-9]{3}[- ]?[0-9]{4}")

str_detect(body[18], "[(]?[0-9]{3}[- )]*[0-9]{3}[- ]?[0-9]{4}")

zipcode = c("90028","90028-0809")
str_detect(zipcode, "[0-9]{5}(-[0-9]{4})?")

###Week 9
library(rvest)


url = "http://www.nfl.com/stats/categorystats?tabSeq=0&statisticCategory=PASSING&conference=null&season=2014&seasonType=PRE&d-447263-s=PASSING_YARDS&d-447263-o=2&d-447263-n=1"
table1 = url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="result"]') %>%
  html_table()
table1 = table1[[1]]


for (i in 1:5)
  print(i)

for (i in 1:10){
  print("DSO545")
  print(i)
}

url = "http://www.nfl.com/stats/categorystats?tabSeq=0&season=2014&seasonType=PRE&d-447263-n=1&d-447263-o=2&d-447263-p=2&conference=null&statisticCategory=PASSING&d-447263-s=PASSING_YARDS"
url_part1 = "http://www.nfl.com/stats/categorystats?tabSeq=0&season=2014&seasonType=PRE&d-447263-n=1&d-447263-o=2&d-447263-p="
url_part2 = "&conference=null&statisticCategory=PASSING&d-447263-s=PASSING_YARDS"
paste(url_part1, i, url_part2, sep = "")

data = NULL
for (i in 1:3){
  url = paste(url_part1,i , url_part2, sep = "")
  table = url %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="result"]') %>%
    html_table()
  table = table[[1]]
  data = rbind(data, table)
}

library(ggplot2)
data %>%
  arrange(-Rate) %>%
  head(10) %>%
  ggplot(aes(x = reorder(Player,Rate), y = Rate)) +
  geom_point() +
  coord_flip() +
  theme_bw( )


#2
url = "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
table = url %>% #specify the url
  read_html() %>% #
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table') %>%
  html_table() 
table = table[[1]]


library(stringr)
table$Population = str_replace_all(table$Population,",","")
sum(as.numeric(table$Population))

library(lubridate)
table$Date = mdy(table$Date)
