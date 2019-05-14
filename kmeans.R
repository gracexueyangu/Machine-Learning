library(data.table)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(dplyr)
library(ggplot2)

train = fread('FETrain.csv')
test = fread('FETest.csv')

# Combine train and test data together
df = merge(train,test,all = TRUE)[,-'is_active']
data %>%
  filter(TENURE_GRP == 1) %>%
  group_by(target_label)%>%
  summarise(count = n())

data = df %>%
  filter(df$target_label == 1)

data2 = df %>%
  filter(df$target_label == 0)

data = data[,-31]
data2 = data2[,-31]
str(data)

## remove NA
data = data.frame(lapply(data, function(x) as.numeric(as.character(x))))
data = na.exclude(data)
data <- data[complete.cases(data),]
data <- data[!is.infinite(rowSums(data)),]



data2 = data.frame(lapply(data2, function(x) as.numeric(as.character(x))))
data2 = na.exclude(data2)
data2 <- data2[complete.cases(data2),]
data2 <- data2[!is.infinite(rowSums(data2)),]




# pca
pca_data = prcomp(data, center = T, scale = T)

pca_data2 = prcomp(data2, center = T, scale = T)
## if we do not do z-scale manually, we use "center = T, scale = T" and get the same output

plot(pca_data, xlab = "Principal Component")
fviz_eig(pca_data)
names(pca_data)
dim(pca_data$x)
summary(pca_data)


plot(pca_data2, xlab = "Principal Component")
fviz_eig(pca_data2)
names(pca_data2)
dim(pca_data2$x)
summary(pca_data2)



# 80%
PCA <- data.frame(pca_data$x[,1:9])

PCA2 <- data.frame(pca_data2$x[,1:10])

# PCA to 80%.
#PCA = data.frame(pca_data$rotation)
#PCA = PCA[,1:9]

# method 1:
#wss <- (nrow(data)-1)*sum(apply(data,2,var))
#for (i in 2:15) wss[i] <- sum(kmeans(data,
                                     #centers=i)$withinss)
#plot(1:15, wss, type="b", xlab="Number of Clusters",
     #ylab="Within groups sum of squares")

# method 2:
fviz_nbclust(data, kmeans, method = "wss")

fviz_nbclust(data2, kmeans, method = "wss")

# cluster: renew
km_renew = kmeans(PCA, 3, iter.max=100)


fviz_cluster(km_renew, data = data)


renew = data %>%
  mutate(cluster = km_renew$cluster) 

renew1 = renew %>%
  filter(cluster == 2) %>%
  arrange(total_items)


# count of cluster 1 & 2
renew  %>%
  group_by(cluster) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = as.factor(cluster),y = count)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  xlab("Cluster") +
  ylab("Count")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))


renew %>%
  group_by(target_label) %>%
  filter(TENURE_GRP == 1, cluster == 1) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

renew %>%
  group_by(target_label) %>%
  filter(TENURE_GRP == 1, cluster == 2) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))


  
# total # of items in 2017
renew %>%
  group_by(cluster) %>%
  filter(TENURE_GRP == 1) %>%
  summarise(num_item = mean(total_items)) %>%
  ggplot(aes(x = as.factor(cluster),y = num_item)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  xlab("Cluster") +
  ylab("Total items")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
  
# total visits in 2017
renew %>%
  group_by(cluster) %>%
  filter(TENURE_GRP == 1) %>%
  summarise(num_item = mean(total_visit)) %>%
  ggplot(aes(x = as.factor(cluster),y = num_item)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  xlab("Cluster") +
  ylab("Total visit")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

# total # of items last month
renew %>%
  group_by(cluster) %>%
  filter(TENURE_GRP == 1) %>%
  summarise(num_item = mean(total_itemslastm)) %>%
  ggplot(aes(x = as.factor(cluster),y = num_item)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  xlab("Cluster") +
  ylab("Total items bought last month")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

#total visit last month
renew %>%
  group_by(cluster) %>%
  filter(TENURE_GRP == 1) %>%
  summarise(num_item = mean(total_visitlastm)) %>%
  ggplot(aes(x = as.factor(cluster),y = num_item)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  xlab("Cluster") +
  ylab("Total visit last month")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

# market area (no use!!!)
renew %>%
  filter(cluster == 1) %>%
  filter(TENURE_GRP == 1) %>%
  group_by(MARKET_AREA_NAME) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  mutate(percent = count/sum(count)) %>%
  head(5) %>%
  ggplot(aes(x = reorder(MARKET_AREA_NAME, count),y = percent)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  coord_flip() +
  xlab("Favourite market area") +
  ylab("Percentage")+
  ggtitle("Cluster = 1") +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

renew %>%
  filter(cluster == 2) %>%
  filter(TENURE_GRP == 1) %>%
  group_by(MARKET_AREA_NAME) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  mutate(percent = count/sum(count)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(MARKET_AREA_NAME, count),y = percent)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  coord_flip() +
  xlab("Favourite market area") +
  ylab("Percentage")+
  ggtitle("Cluster = 2") +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))


# the ratio of (the visits he or she shops in the favorite category)/(the total visits)
renew %>%
  group_by(cluster) %>%
  filter(TENURE_GRP == 1) %>%
  summarise(num_item = mean(fav_ctg_ratio)) %>%
  ggplot(aes(x = as.factor(cluster),y = num_item)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  xlab("Cluster") +
  ylab("Ratio")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

# most spent category
renew %>%
  filter(cluster == 1) %>%
  filter(TENURE_GRP == 1) %>%
  group_by(CATEGORY_DESC) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  mutate(percent = count/sum(count)) %>%
  head(10) %>%
  mutate(percent = count/sum(count)) %>%
  ggplot(aes(x = reorder(CATEGORY_DESC, count), percent)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  coord_flip() +
  xlab("Most spent category") +
  ylab("Percentage")+
  ggtitle("Cluster = 1") +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

renew %>%
  filter(cluster == 2) %>%
  filter(TENURE_GRP == 1) %>%
  group_by(CATEGORY_DESC) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  mutate(percent = count/sum(count)) %>%
  head(10) %>%
  mutate(percent = count/sum(count)) %>%
  ggplot(aes(x = reorder(CATEGORY_DESC, count), percent)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  coord_flip() +
  xlab("Most spent category") +
  ylab("Percentage")+
  ggtitle("Cluster = 2") +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

# most spent category amount

renew %>%
  filter(TENURE_GRP == 1) %>%
  group_by(cluster) %>% 
  summarise(most_amt = mean(most_ctg_amt))

# gas spent amount

renew %>%
  filter(TENURE_GRP == 1) %>%
  group_by(cluster) %>% 
  summarise(most_amt = mean(gas_spent))


# region

renew_region %>%
  filter(cluster == 1) %>%
  filter(TENURE_GRP == 1) %>%
  group_by(REGION_NAME) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  mutate(percent = count/sum(count)) %>%
  head(10) %>%
  mutate(percent = count/sum(count)) %>%
  ggplot(aes(x = reorder(REGION_NAME, count), percent)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  coord_flip() +
  xlab("Region") +
  ylab("Percentage")+
  ggtitle("Cluster = 1") +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

renew_region %>%
  filter(cluster == 2) %>%
  filter(TENURE_GRP == 1) %>%
  group_by(REGION_NAME) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  mutate(percent = count/sum(count)) %>%
  head(10) %>%
  mutate(percent = count/sum(count)) %>%
  ggplot(aes(x = reorder(REGION_NAME, count), percent)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  coord_flip() +
  xlab("Region") +
  ylab("Percentage")+
  ggtitle("Cluster = 2") +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

# auto renew (no difference!)

data_mem %>% 
  filter(TENURE_GRP.x=='1') %>%
  group_by(cluster, autorenew_ind) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))



# tender type

renew_tender %>%
  filter(cluster == 1) %>%
  filter(TENURE_GRP == 1) %>%
  group_by(TENDER_TYPE_DESC) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  mutate(percent = count/sum(count)) %>%
  head(5) %>%
  mutate(percent = count/sum(count)) %>%
  ggplot(aes(x = reorder(TENDER_TYPE_DESC, count), percent)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  coord_flip() +
  xlab("Tender Type") +
  ylab("Percentage")+
  ggtitle("Cluster = 1") +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))


renew_tender %>%
  filter(cluster == 2) %>%
  filter(TENURE_GRP == 1) %>%
  group_by(TENDER_TYPE_DESC) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  mutate(percent = count/sum(count)) %>%
  head(5) %>%
  mutate(percent = count/sum(count)) %>%
  ggplot(aes(x = reorder(TENDER_TYPE_DESC, count), percent)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  coord_flip() +
  xlab("Tender Type") +
  ylab("Percentage")+
  ggtitle("Cluster = 2") +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

## club

renew_club %>%
  filter(cluster == 1) %>%
  filter(TENURE_GRP == 1) %>%
  group_by(CLUB_NAME) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  mutate(percent = count/sum(count)) %>%
  head(5) %>%
  mutate(percent = count/sum(count)) %>%
  ggplot(aes(x = reorder(CLUB_NAME, count), percent)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  coord_flip() +
  xlab("Club") +
  ylab("Percentage")+
  ggtitle("Cluster = 1") +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

renew_club %>%
  filter(cluster == 2) %>%
  filter(TENURE_GRP == 1) %>%
  group_by(CLUB_NAME) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  mutate(percent = count/sum(count)) %>%
  head(5) %>%
  mutate(percent = count/sum(count)) %>%
  ggplot(aes(x = reorder(CLUB_NAME, count), percent)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  coord_flip() +
  xlab("Club") +
  ylab("Percentage")+
  ggtitle("Cluster = 2") +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))


## total sales

renew_price %>%
  filter(TENURE_GRP == 1) %>%
  group_by(cluster) %>%
  summarise(mean = mean(price)) %>%
  ggplot(aes(x = as.factor(cluster), mean)) +
  geom_col(fill = 'skyblue',width = 0.5) +
  theme_classic() +
  xlab("Total sales") +
  ylab("$")+
  ggtitle("Cluster = 1") +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

# unit price
renew_price %>%
  filter(TENURE_GRP == 1) %>%
  mutate(unit_price = price/total_items) %>%
  group_by(cluster) %>%
  summarise(mean = mean(unit_price)) %>%
  ggplot(aes(x = as.factor(cluster), mean)) +
  geom_col(fill = 'skyblue',width = 0.5) +
  theme_classic() +
  xlab("Unit price") +
  ylab("$")+
  ggtitle("Cluster = 1") +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

# price per visit
renew_price %>%
  filter(TENURE_GRP == 1) %>%
  mutate(unit_price = price/total_visit) %>%
  group_by(cluster) %>%
  summarise(mean = mean(unit_price)) %>%
  ggplot(aes(x = as.factor(cluster), mean)) +
  geom_col(fill = 'skyblue',width = 0.5) +
  theme_classic() +
  xlab("Unit price") +
  ylab("$")+
  ggtitle("Cluster = 1") +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

#######
pos_train = fread('pos_train.csv') 
pos_test = fread('pos_test.csv') 
pos = merge(pos_train,pos_test,all = TRUE)
data_pos = inner_join(renew, pos, by = "MEMBERSHIP_ID")


mem_train = fread('member_train.csv')
mem_test = fread('member_test.csv') 
mem = merge(mem_train,mem_test,all = TRUE)


ten_train = fread('tender_train.csv') 
ten_test = fread('tender_test.csv') 
ten = merge(ten_train,ten_test,all = TRUE)

data_pos = inner_join(renew, pos, by = "MEMBERSHIP_ID")

data_mem = inner_join(renew, mem, by = "MEMBERSHIP_ID")

data_ten = left_join(renew, ten, by = "MEMBERSHIP_ID")


price = data_pos %>%
  group_by(MEMBERSHIP_ID) %>%
  summarise(price = sum(RETAIL_PRICE))


renew_price = inner_join(renew, price, by = c("MEMBERSHIP_ID"))




club = data_pos %>%
  group_by(MEMBERSHIP_ID, CLUB_NBR, CLUB_NAME) %>%
  summarise(count = n()) 

# use aggregate to create new data frame with the maxima
df.agg <- aggregate(count ~ MEMBERSHIP_ID, club, max)
# then simply merge with the original
club <- merge(df.agg, club)

renew_club = inner_join(renew, club, by = c("MEMBERSHIP_ID"))




market = data_pos %>%
  group_by(MARKET_AREA_NAME, MARKET_AREA_NBR) %>%
  summarise(count = n()) 

category = data_pos %>%
  group_by(CATEGORY_NBR, CATEGORY_DESC) %>%
  summarise(count = n()) %>%
  arrange(-count)

region = data_pos %>%
  group_by(MEMBERSHIP_ID, REGION_NBR, REGION_NAME) %>%
  summarise(count = n())

tender = data_ten %>%
  group_by(MEMBERSHIP_ID,TENDER_TYPE_DESC) %>%
  summarise(count = n()) 



renew = left_join(renew, market, by = c("fav_area"= "MARKET_AREA_NBR"))

renew = left_join(renew, category, by = c("spendmost_ctg"= "CATEGORY_NBR"))

renew_region = inner_join(renew, region, by = c("MEMBERSHIP_ID"))

renew_tender = left_join(renew, tender, by = c("MEMBERSHIP_ID"))

####################### no use !!! ########################

# cluster: non_renew

km_non_renew = kmeans(PCA2, 2, iter.max=100)


fviz_cluster(km_non_renew, data = data2)


non_renew = data2 %>%
  mutate(cluster = km_non_renew$cluster) 


# count of cluster 1 & 2
non_renew  %>%
  group_by(cluster) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = as.factor(cluster),y = count)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  xlab("Cluster") +
  ylab("Count")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

# total # of items in 2017
non_renew %>%
  group_by(cluster) %>%
  filter(TENURE_GRP == 1) %>%
  summarise(num_item = mean(total_items)) %>%
  ggplot(aes(x = as.factor(cluster),y = num_item)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  xlab("Cluster") +
  ylab("Total items")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

# total visits in 2017
non_renew %>%
  group_by(cluster) %>%
  filter(TENURE_GRP == 1) %>%
  summarise(num_item = mean(total_visit)) %>%
  ggplot(aes(x = as.factor(cluster),y = num_item)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  xlab("Cluster") +
  ylab("Total visit")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

# total # of items last month
non_renew %>%
  group_by(cluster) %>%
  filter(TENURE_GRP == 1) %>%
  summarise(num_item = mean(total_itemslastm)) %>%
  ggplot(aes(x = as.factor(cluster),y = num_item)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  xlab("Cluster") +
  ylab("Total items bought last month")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

#total visit last month
non_renew %>%
  group_by(cluster) %>%
  filter(TENURE_GRP == 1) %>%
  summarise(num_item = mean(total_visitlastm)) %>%
  ggplot(aes(x = as.factor(cluster),y = num_item)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  xlab("Cluster") +
  ylab("Total visit last month")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

# market area (no use!!!)
non_renew %>%
  filter(cluster == 1) %>%
  filter(TENURE_GRP == 1) %>%
  group_by(MARKET_AREA_NAME) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  mutate(percent = count/sum(count)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(MARKET_AREA_NAME, count),y = percent)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  coord_flip() +
  xlab("Favourite market area") +
  ylab("Percentage")+
  ggtitle("Cluster = 1") +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

non_renew %>%
  filter(cluster == 2) %>%
  filter(TENURE_GRP == 1) %>%
  group_by(MARKET_AREA_NAME) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  mutate(percent = count/sum(count)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(MARKET_AREA_NAME, count),y = percent)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  coord_flip() +
  xlab("Favourite market area") +
  ylab("Percentage")+
  ggtitle("Cluster = 2") +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))


# the ratio of (the visits he or she shops in the favorite category)/(the total visits)
non_renew %>%
  group_by(cluster) %>%
  filter(TENURE_GRP == 1) %>%
  summarise(num_item = mean(fav_ctg_ratio)) %>%
  ggplot(aes(x = as.factor(cluster),y = num_item)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  xlab("Cluster") +
  ylab("Ratio")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

# most spent category
non_renew %>%
  filter(cluster == 1) %>%
  filter(TENURE_GRP == 1) %>%
  group_by(CATEGORY_DESC) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  mutate(percent = count/sum(count)) %>%
  head(10) %>%
  mutate(percent = count/sum(count)) %>%
  ggplot(aes(x = reorder(CATEGORY_DESC, count), percent)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  coord_flip() +
  xlab("Most spent category") +
  ylab("Percentage")+
  ggtitle("Cluster = 1") +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

non_renew %>%
  filter(cluster == 2) %>%
  filter(TENURE_GRP == 1) %>%
  group_by(CATEGORY_DESC) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  mutate(percent = count/sum(count)) %>%
  head(10) %>%
  mutate(percent = count/sum(count)) %>%
  ggplot(aes(x = reorder(CATEGORY_DESC, count), percent)) +
  geom_col(fill = 'skyblue',width = 0.6) +
  theme_classic() +
  coord_flip() +
  xlab("Most spent category") +
  ylab("Percentage")+
  ggtitle("Cluster = 2") +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))


# region (not 1-by-1)




#######
pos_train = fread('pos_train.csv') 
pos_test = fread('pos_test.csv') 
pos = merge(pos_train,pos_test,all = TRUE)

data_pos = inner_join(renew, pos, by = "MEMBERSHIP_ID")


market = data_pos %>%
  group_by(MARKET_AREA_NAME, MARKET_AREA_NBR) %>%
  summarise(count = n())

category = data_pos %>%
  group_by(CATEGORY_NBR, CATEGORY_DESC) %>%
  summarise(count = n())

region = data_pos %>%
  group_by(MEMBERSHIP_ID, REGION_NBR, REGION_NAME) %>%
  summarise(count = n())

renew = left_join(renew, market, by = c("fav_area"= "MARKET_AREA_NBR"))

renew = left_join(renew, category, by = c("spendmost_ctg"= "CATEGORY_NBR"))

try = inner_join(renew, region, by = c("MEMBERSHIP_ID"))

non_renew = left_join(non_renew, market, by = c("fav_area"= "MARKET_AREA_NBR"))

non_renew = left_join(non_renew, category, by = c("spendmost_ctg"= "CATEGORY_NBR"))


