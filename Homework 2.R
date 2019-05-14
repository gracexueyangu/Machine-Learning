data = read.csv("NY property data.csv")
library(dplyr)
library(ggplot2)
library(scales)
#turn off scientific notation like 1e+06
options(scipen=999)

summary(data)
#判断有没有NA
data %>%
  filter(is.na(EASEMENT)) %>%
  filter(EASEMENT == "") %>%
  summarise(count = n())

#每个field + distribution

RECORD = data %>%
  group_by(RECORD) %>%
  summarise(count = n())

BBLE = data %>%
  group_by(BBLE) %>%
  summarise(count = n())
#######

B = data %>%
  group_by(B) %>%
  summarise(count = n())

ggplot(B, aes(x = B, y = count)) +
  geom_col(fill = "skyblue", color = "black",width = 0.5) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
######
BLOCK = data %>%
    group_by(BLOCK)%>%
  summarise(count = n()) %>% 
  arrange(-count)

data %>%
  group_by(BLOCK)%>%
  summarise(count = n()) %>% 
  arrange(-count) %>%
  head(15) %>%
  ggplot(aes(x = reorder(factor(BLOCK),-count), y = count)) +
  geom_col(fill = "skyblue", color = "black") +
  theme_classic() +
  xlab("BLOCK")+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

#######
LOT = data %>%
  group_by(LOT) %>%
  summarise(count = n()) %>%
  arrange(-count)

data %>%
  group_by(LOT)%>%
  summarise(count = n()) %>% 
  arrange(-count) %>%
  head(15) %>%
  ggplot(aes(x = reorder(factor(LOT),-count), y = count)) +
  geom_col(fill = "skyblue", color = "black") +
  theme_classic() +
  xlab("LOT")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

###########

EASEMENT = data %>%
  group_by(EASEMENT) %>%
  summarise(count = n())
sum(EASEMENT$count)
data %>%
  group_by(EASEMENT) %>%
  summarise(count = n())

data %>%
  group_by(EASEMENT)%>%
  summarise(count = n()) %>% 
  filter(EASEMENT != "") %>%
  arrange(-count) %>%
  ggplot(aes(x = reorder(factor(EASEMENT),-count), y = count)) +
  geom_col(fill = "skyblue", color = "black") +
  theme_classic() +
  xlab("EASEMENT")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
#########

OWNER = data %>%
  group_by(OWNER) %>%
  summarise(count = n()) %>%
  arrange(-count)

data %>%
  group_by(OWNER)%>%
  summarise(count = n()) %>% 
  filter(OWNER != "") %>%
  arrange(-count) %>%
  head(15) %>%
  ggplot(aes(x = reorder(factor(OWNER),count), y = count)) +
  geom_col(fill = "skyblue", color = "black") +
  theme_classic() +
  xlab("OWNER") +
  coord_flip()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

############
BLDGCL = data %>%
  group_by(BLDGCL) %>%
  summarise(count = n())%>%
  arrange(-count)

data %>%
  group_by(BLDGCL)%>%
  summarise(count = n()) %>% 
  arrange(-count) %>%
  head(15) %>%
  ggplot(aes(x = reorder(factor(BLDGCL),-count), y = count)) +
  geom_col(fill = "skyblue", color = "black") +
  theme_classic() +
  xlab("BLDGCL")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
########

TAXCLASS = data %>%
  group_by(TAXCLASS) %>%
  summarise(count = n())%>%
  arrange(-count)

data %>%
  group_by(TAXCLASS)%>%
  summarise(count = n()) %>% 
  arrange(-count) %>%
  head(15) %>%
  ggplot(aes(x = reorder(factor(TAXCLASS),-count), y = count)) +
  geom_col(fill = "skyblue", color = "black") +
  theme_classic() +
  xlab("TAXCLASS")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

########
LTFRONT = data %>%
  group_by(LTFRONT) %>%
  summarise(count = n()) %>%
  arrange(-count)

data %>%
  ggplot(aes(x = 1,y = LTFRONT)) +
  geom_boxplot()

data %>%
  filter(LTFRONT <= 300) %>%
  ggplot(aes(x = LTFRONT)) +
  geom_histogram(fill = "skyblue") +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 300, by = 50),
                     labels = c(0, 50,100,150,200,250,300))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
  

#########

LTDEPTH = data %>%
  group_by(LTDEPTH) %>%
  summarise(count = n()) %>%
  arrange(-count)

data %>%
  filter(LTDEPTH <= 300) %>%
  ggplot(aes(x = LTDEPTH)) +
  geom_histogram(fill = "skyblue") +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 300, by = 50),
                     labels = c(0, 50,100,150,200,250,300))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
######

EXT = data %>%
  group_by(EXT) %>%
  summarise(count = n())%>%
  arrange(-count)

data %>%
  group_by(EXT)%>%
  summarise(count = n()) %>% 
  filter(EXT != "") %>%
  arrange(-count)  %>%
  ggplot(aes(x = reorder(factor(EXT),-count), y = count)) +
  geom_col(fill = "skyblue", color = "black",width = 0.4) +
  theme_classic() +
  xlab("EXT")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

########

STORIES = data %>%
  group_by(STORIES) %>%
  summarise(count = n())

data %>%
  filter(STORIES <= 60) %>%
  ggplot(aes(x = STORIES)) +
  geom_histogram(fill = "skyblue") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
###########

FULLVAL = data %>%
  group_by(FULLVAL) %>%
  summarise(count = n())

data %>%
  group_by(FULLVAL) %>%
  summarise(count = n()) %>%
  arrange(-count)

data %>%
  filter(FULLVAL <= 2000000) %>%
  ggplot(aes(x = FULLVAL)) +
  geom_histogram(fill = "skyblue") +
  theme_classic()  +
  scale_y_continuous(breaks = seq(0, 200000, by = 50000),
                     labels = c(0,50000,100000,150000,200000))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

#########
AVLAND = data %>%
  group_by(AVLAND) %>%
  summarise(count = n())

data %>%
  group_by(AVLAND) %>%
  summarise(count = n()) %>%
  arrange(-count)

data %>%
  filter(AVLAND <= 200000) %>%
  ggplot(aes(x = AVLAND)) +
  geom_histogram(fill = "skyblue") +
  theme_classic()  +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

#######
AVTOT = data %>%
  group_by(AVTOT) %>%
  summarise(count = n())

data %>%
  filter(AVTOT <= 200000) %>%
  ggplot(aes(x = AVTOT)) +
  geom_histogram(fill = "skyblue") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

#######
EXLAND = data %>%
  group_by(EXLAND) %>%
  summarise(count = n())

data %>%
  filter(EXLAND <= 20000) %>%
  ggplot(aes(x = EXLAND)) +
  geom_histogram(fill = "skyblue") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
######

EXTOT = data %>%
  group_by(EXTOT) %>%
  summarise(count = n())

data %>%
  filter(EXTOT <= 20000) %>%
  ggplot(aes(x = EXTOT)) +
  geom_histogram(fill = "skyblue") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
######

EXCD1 = data %>%
  group_by(EXCD1) %>%
  summarise(count = n()) %>%
  arrange(-count)

data %>%
  group_by(EXCD1)%>%
  summarise(count = n()) %>%
  filter(!is.na(EXCD1)) %>%
  arrange(-count) %>%
  head(15) %>%
  ggplot(aes(x = reorder(factor(EXCD1),-count), y = count)) +
  geom_col(fill = "skyblue", color = "black") +
  theme_classic() +
  xlab("EXCD1")+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))



#######
STADDR = data %>%
  group_by(STADDR) %>%
  summarise(count = n())%>%
  arrange(-count)

data %>%
  group_by(STADDR)%>%
  summarise(count = n()) %>% 
  filter(STADDR != "") %>%
  arrange(-count) %>%
  head(15) %>%
  ggplot(aes(x = reorder(factor(STADDR),count), y = count)) +
  geom_col(fill = "skyblue", color = "black") +
  theme_classic() +
  xlab("STADDR") +
  coord_flip()+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
######

ZIP = data %>%
  group_by(ZIP) %>%
  summarise(count = n())%>%
  arrange(-count)

data %>%
  group_by(ZIP)%>%
  filter(!is.na(ZIP)) %>%
  summarise(count = n()) %>% 
  arrange(-count) %>%
  head(15) %>%
  ggplot(aes(x = reorder(factor(ZIP),-count), y = count)) +
  geom_col(fill = "skyblue", color = "black") +
  theme_classic() +
  xlab("ZIP")+
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

#######
EXMPTCL = data %>%
  group_by(EXMPTCL) %>%
  summarise(count = n())%>%
  arrange(-count)

data %>%
  group_by(EXMPTCL) %>%
  summarise(count = n()) %>% 
  filter(EXMPTCL != "") %>%
  arrange(-count) %>%
  head(15) %>%
  ggplot(aes(x = reorder(factor(EXMPTCL),-count), y = count)) +
  geom_col(fill = "skyblue", color = "black") +
  theme_classic() +
  xlab("EXMPTCL")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

######
BLDFRONT = data %>%
  group_by(BLDFRONT) %>%
  summarise(count = n()) %>%
  arrange(-count)

data %>%
  filter(BLDFRONT <= 300) %>%
  ggplot(aes(x = BLDFRONT)) +
  geom_histogram(fill = "skyblue") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
######

BLDDEPTH = data %>%
  group_by(BLDDEPTH) %>%
  summarise(count = n()) %>%
  arrange(-count)

data %>%
  filter(BLDDEPTH <= 300) %>%
  ggplot(aes(x = BLDDEPTH)) +
  geom_histogram(fill = "skyblue") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
#####
AVLAND2 = data %>%
  group_by(AVLAND2) %>%
  summarise(count = n())

data %>%
  filter(AVLAND2 <= 500000) %>%
  ggplot(aes(x = AVLAND2)) +
  geom_histogram(fill = "skyblue") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))


#####
AVTOT2 = data %>%
  group_by(AVTOT2) %>%
  summarise(count = n())

data %>%
  filter(AVTOT2 <= 1000000) %>%
  ggplot(aes(x = AVTOT2)) +
  geom_histogram(fill = "skyblue") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

######

EXLAND2 = data %>%
  group_by(EXLAND2) %>%
  summarise(count = n())

data %>%
  filter(EXLAND2 <= 50000) %>%
  ggplot(aes(x = EXLAND2)) +
  geom_histogram(fill = "skyblue") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

#####
EXTOT2 = data %>%
  group_by(EXTOT2) %>%
  summarise(count = n())

data %>%
  filter(EXTOT2 <= 300000) %>%
  ggplot(aes(x = EXTOT2)) +
  geom_histogram(fill = "skyblue") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
#####
EXCD2 = data %>%
  group_by(EXCD2) %>%
  summarise(count = n())  %>%
  arrange(-count)

data %>%
  group_by(EXCD2)%>%
  summarise(count = n()) %>%
  filter(!is.na(EXCD2)) %>%
  arrange(-count) %>%
  head(15) %>%
  ggplot(aes(x = reorder(factor(EXCD2),-count), y = count)) +
  geom_col(fill = "skyblue", color = "black") +
  theme_classic() +
  xlab("EXCD2")+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))




####

PERIOD = data %>%
  group_by(PERIOD) %>%
  summarise(count = n())%>%
  arrange(-count)

data %>%
  group_by(PERIOD)%>%
  summarise(count = n()) %>%
  ggplot(aes(x = reorder(factor(PERIOD),count), y = count)) +
  geom_col(fill = "skyblue", color = "black", width = 0.3) +
  theme_classic() +
  xlab("PERIOD") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

#####
YEAR = data %>%
  group_by(YEAR) %>%
  summarise(count = n())

data %>%
  group_by(YEAR)%>%
  summarise(count = n()) %>%
  ggplot(aes(x = reorder(factor(YEAR),count), y = count)) +
  geom_col(fill = "skyblue", color = "black", width = 0.3) +
  theme_classic() +
  xlab("YEAR") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
####
VALTYPE = data %>%
  group_by(VALTYPE) %>%
  summarise(count = n())

data %>%
  group_by(VALTYPE)%>%
  summarise(count = n()) %>%
  ggplot(aes(x = reorder(factor(VALTYPE),count), y = count)) +
  geom_col(fill = "skyblue", color = "black", width = 0.3) +
  theme_classic() +
  xlab("VALTYPE") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
#判断0的个数
data %>%
  filter(OWNER == 0) %>%
  summarise(count = n())


#计算numerical的各个数据

summary(data$LTFRONT)
sd(data$LTFRONT)
mean(data$LTFRONT)

summary(data$LTDEPTH)
sd(data$LTDEPTH)

summary(data$STORIES)
sd(data$STORIES, na.rm = TRUE)

summary(data$FULLVAL)
sd(data$FULLVAL)

summary(data$AVLAND)
sd(data$AVLAND)

summary(data$AVTOT)
sd(data$AVTOT)

summary(data$EXLAND)
sd(data$EXLAND)

summary(data$EXTOT)
sd(data$EXTOT)

summary(data$EXCD1)
sd(data$EXCD1)

summary(data$BLDFRONT)
sd(data$BLDFRONT)

summary(data$BLDDEPTH)
sd(data$BLDDEPTH)

summary(data$AVLAND2)
sd(data$AVLAND2,na.rm = TRUE)

summary(data$AVTOT2)
sd(data$AVTOT2,na.rm = TRUE)

summary(data$EXLAND2)
sd(data$EXLAND2,na.rm = TRUE)

summary(data$EXTOT2)
sd(data$EXTOT2)

summary(data$EXCD2)
sd(data$EXCD2,na.rm = TRUE)
