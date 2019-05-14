
##load the two packages

library(dplyr) #data manipulation: easier
library(hflights) #dataset

View(hflights)
?hflights
#1
#filter the data by American Airlines, and then
#summarise the data to get the mean of ArrDelay


#2
df2 = select(hflights, ActualElapsedTime, AirTime, ArrDelay, DepDelay)

#3
df3 = select(hflights, Origin:Cancelled)

#4
df4 = select(hflights, Year:DayOfWeek, ArrDelay:Diverted)
#or
df4 = select(hflights,-(DepTime:AirTime))#a vector

#5
df5 = select(hflights, ArrDelay,DepDelay)
#or with the helper function
df5 = select(hflights, ends_with("Delay"))

#6
df6 = select(hflights, UniqueCarrier, ends_with("Num"),
             starts_with("Cancel"))

#7
df7 = mutate(hflights, GroundTime = TaxiIn + TaxiOut)

#8
df8 = filter(hflights, Distance >= 3000)

#9
df9 = filter(hflights, UniqueCarrier %in% c("AA","AS","B6"))
#or
df9 = filter(hflights, UniqueCarrier =="AA" | UniqueCarrier == "AS"| UniqueCarrier == "B6")

####not correct
df9_a = df9 = filter(hflights, UniqueCarrier == c("AA","AS","B6"))
#== : compare the unique vector(AA AS B6) in circle

#10
df10 = filter(hflights, TaxiIn + TaxiOut > AirTime)
#or
df10 = filter(df7, GroundTime > AirTime)

#11
df11 = filter(hflights, DayOfWeek %in% c(6,7) & Cancelled == 1)
#or #in filter, & equal to , means and
df11 = filter(hflights, DayOfWeek %in% 6:7 & Cancelled == 1)

#12
df12 = arrange(hflights, UniqueCarrier, -DepDelay)

#13
df13 = arrange(hflights, ArrDelay + DepDelay)
#or
temp = mutate(hflights, TotalDelay = ArrDelay + DepDelay)
df13 = arrange(temp, TotalDelay)
#normal order means 1-10

#14
df14 = arrange(filter(hflights, Dest == "DFW" & DepTime < 800),-AirTime)
#or(piping operator)
df14 = hflights %>% filter(Dest == "DFW" & DepTime < 800) %>% arrange(-AirTime)

#15
min(hflights$Distance)
max(hflights$Distance)

#using dplyr 

df15 = summarise(hflights,
                 min_dist = min(Distance),
                 max_dist = max(Distance))

hflights %>%
  summarise(min_dist = min(Distance),
            max_dist = max(Distance))

#16
df16 = hflights %>%
  filter(Diverted == 1) %>%
  summarise(max_div = max(Distance))

#17
df17 = hflights %>%
  summarise(n_obs = n(),
            n_carrier = n_distinct(UniqueCarrier),
            n_dest = n_distinct(Dest),
            dest100 = nth(Dest,100))

#18

x = c(1,4,6,NA,7)
is.na(x)
!is.na(x)
mean(x, na.rm = T)

df18 = hflights %>%
  mutate(diff = TaxiOut - TaxiIn) %>%
  filter(!is.na(diff)) %>% 
  #select: select value
  summarise(avg = mean(diff))
#or
df18 = hflights %>%
  mutate(diff = TaxiOut - TaxiIn) %>%
  filter(diff != "NA") %>%
  #select: select value
  summarise(avg = mean(diff))

#19
df19 = hflights %>%
  select("Dest", "UniqueCarrier", "Distance", "ActualElapsedTime") %>%
  mutate(RealTime = ActualElapsedTime + 100)


#20
df20 = hflights %>%
  group_by(Dest) %>% #in the () should be categorial variable
  summarise(count = n(),
            avg_dist = mean(Distance, na.rm = T),
            avg_delay = mean(ArrDelay,na.rm = T))
#group_by->summarise

#21
library(ggplot2)
ggplot(df20, aes(x = avg_dist, y = avg_delay)) +
  geom_point()
#or
hflights %>%
  group_by(Dest) %>% #in the () should be categorial variable
  summarise(count = n(),
            avg_dist = mean(Distance, na.rm = T),
            avg_delay = mean(ArrDelay,na.rm = T)) %>%
  filter(avg_dist < 2000) %>%
  ggplot(aes(x = avg_dist, y = avg_delay)) +
  geom_point() +
  geom_smooth()

#22
hflights %>%
  group_by(UniqueCarrier) %>%
  #filter(Cancelled == 1) %>% wrong
  summarise(n_flights = n(),
            n_canc = sum(Cancelled == 1),
            avg_delay = mean(ArrDelay, na.rm = T)) %>%
  arrange(avg_delay,n_canc)
#US airline is the best
#n_canc = sum(Distance > 3000)  if is not 1/0

x = c(0,1,1,1,0,0,0)
sum(x)










