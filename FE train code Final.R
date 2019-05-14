library(data.table)
library(caret)
library(dplyr)
library(stringr)
library(RPostgreSQL)
library(sqldf)

pos = fread('pos_train.csv') #fread used fro big data and it returns a data table 
tender = fread('tender_train.csv')
member = fread('member_train.csv')
dmm = fread('dmm_gmm.txt')

class(pos)
dim(pos)
summary(pos) #this will give any missing values 

str(pos)
str(member)
str(tender)
str(dmm)


pg = dbDriver("PostgreSQL")
con = dbConnect(pg, user = "postgres", password = "chetan22", host = "localhost", 
                port = 5432, dbname = "postgres")



pos1=pos
names(pos1) = str_replace_all(names(pos1),"_","")
names(pos1)=tolower(names(pos1))
#We here need to convert column names to lowercase that SQL would read
dbWriteTable(con, 'pos', pos1, row.names=FALSE, overwrite=TRUE)

names(pos)


lookup<-dbGetQuery(con, "
                   with uservisitctgr as (
                   select membershipid, categorynbr, visitnbr,
                   count(*) over (partition by membershipid) cnt,
                   dense_rank() over (partition by membershipid order by visitnbr) + dense_rank()
                   over (partition by membershipid order by visitnbr desc) - 1 as visitcnt,
                   dense_rank() over (partition by membershipid,categorynbr order by visitnbr) + dense_rank()
                   over (partition by membershipid,categorynbr order by visitnbr desc) - 1 as ctgrvisitcnt
                   from pos
                   )
                   select membershipid,
                   avg(cnt) total_items,
                   avg(visitcnt) total_visit,
                   avg(case when ctgrvisitrank
                   = 1 then ctgrvisitcnt else null end)*1.0/avg(visitcnt) fav_ctg_ratio
                   from
                   (
                   select membershipid, categorynbr, visitnbr,
                   cnt, visitcnt, ctgrvisitcnt,
                   rank() over (partition by membershipid order by ctgrvisitcnt desc) ctgrvisitrank
                   from uservisitctgr
                   ) tmp
                   group by membershipid
                   ")

head(lookup)

lookup9 <-dbGetQuery(con, "
with tbl as (
                    select membershipid, visitnbr,
                    count(*) over (partition by membershipid) cnt,
                    dense_rank() over (partition by membershipid order by visitnbr) + dense_rank()
                    over (partition by membershipid order by visitnbr desc) - 1 as visitcnt
                    from pos where visitdate > '2017-11-31'
)
select membershipid, avg(cnt) total_itemsLastM, avg(visitcnt) total_visitLastM
from tbl
group by membershipid 
        ")

head(lookup9)

names(lookup)[1]="MEMBERSHIP_ID"
names(lookup9)[1] = "MEMBERSHIP_ID"
lookup10=left_join(lookup, lookup9)

lookup10[is.na(lookup10)] <- 0


lookup11 <-dbGetQuery(con, "
with tbl as (
                     select membershipid, visitnbr,
                     count(*) over (partition by membershipid) cnt,
                     dense_rank() over (partition by membershipid order by visitnbr) + dense_rank()
                     over (partition by membershipid order by visitnbr desc) - 1 as visitcnt
                     from pos where visitdate > '2017-10-31'
)
select membershipid, avg(cnt) total_itemsLast2M, avg(visitcnt) total_visitLast2M
from tbl
group by membershipid 
")


names(lookup11)[1] = "MEMBERSHIP_ID"
lookup12=left_join(lookup10, lookup11)

lookup12[is.na(lookup12)] <- 0

lookup13 <-dbGetQuery(con, "
with tbl as (
                      select membershipid, visitnbr,
                      count(*) over (partition by membershipid) cnt,
                      dense_rank() over (partition by membershipid order by visitnbr) + dense_rank()
                      over (partition by membershipid order by visitnbr desc) - 1 as visitcnt
                      from pos where visitdate > '2017-09-31'
)
select membershipid, avg(cnt) total_itemsLast3M, avg(visitcnt) total_visitLast3M
from tbl
group by membershipid 
")


names(lookup13)[1] = "MEMBERSHIP_ID"
lookup14=left_join(lookup12, lookup13)

lookup14[is.na(lookup14)] <- 0

lookup15 <-dbGetQuery(con, "
with tbl as (
                      select membershipid, visitnbr,
                      count(*) over (partition by membershipid) cnt,
                      dense_rank() over (partition by membershipid order by visitnbr) + dense_rank()
                      over (partition by membershipid order by visitnbr desc) - 1 as visitcnt
                      from pos where visitdate > '2017-08-31'
)
select membershipid, avg(cnt) total_itemsLast4M, avg(visitcnt) total_visitLast4M
from tbl
group by membershipid 
")


names(lookup15)[1] = "MEMBERSHIP_ID"
lookup16=left_join(lookup14, lookup15)

lookup16[is.na(lookup16)] <- 0

lookup17 <-dbGetQuery(con, "
with tbl as (
                      select membershipid, visitnbr,
                      count(*) over (partition by membershipid) cnt,
                      dense_rank() over (partition by membershipid order by visitnbr) + dense_rank()
                      over (partition by membershipid order by visitnbr desc) - 1 as visitcnt
                      from pos where visitdate > '2017-06-31'
)
select membershipid, avg(cnt) total_itemsLast6M, avg(visitcnt) total_visitLast6M
from tbl
group by membershipid 
")


names(lookup17)[1] = "MEMBERSHIP_ID"
lookup18=left_join(lookup16, lookup17)

lookup18[is.na(lookup18)] <- 0

member1=left_join(member, lookup18)
str(member1)

#identifying Market Area
col = pos1[, c('membershipid',"visitnbr","marketareanbr","regionnbr")]
data = unique(col)

lookupa = data %>%
  group_by(membershipid) %>%
  summarise(fav_area = names(table(marketareanbr))[which.max(table(marketareanbr))])
head(lookupa)

names(lookupa)[1]="MEMBERSHIP_ID"
member1=left_join(member1, lookupa)


#for each member, identify highest spending categories

col=pos1[,c("membershipid","visitnbr","categorynbr","retailprice")]
data = col %>%
  group_by(membershipid, categorynbr) %>%
  summarise(categoryprice = sum(retailprice))
lookup_a=data %>%
  group_by(membershipid) %>%
  summarise(spendmost_ctg = categorynbr[which.max(categoryprice)])
lookup_b=data %>%
  group_by(membershipid) %>%
  summarise(most_ctg_amt = max(categoryprice))
lookup_c=data %>%
  group_by(membershipid) %>%
  summarise(most_ctg_ratio = max(categoryprice)/sum(categoryprice))
lookupN = left_join(lookup_a, lookup_b)
lookupN = left_join(lookupN, lookup_c)
names(lookupN)[1]="MEMBERSHIP_ID"
head(lookupN)

# Gas Stations 

col=pos1[,c("membershipid","visitnbr","categorynbr","retailprice")]
data = col %>%
  group_by(membershipid, categorynbr) %>%
  summarise(categoryprice = sum(retailprice))
Gas=data[which(data$categorynbr==35),]
Gas=Gas[,-2]
names(Gas)[2]="gas_spent"
head(Gas)

names(Gas)[1]="MEMBERSHIP_ID"
lookup30=left_join(lookupN,Gas)
lookup30[is.na(lookup30)] <- 0
lookup30$regular_gas <-as.numeric(lookup30$gas_spent>50)
#member1 = left_join(member1,lookup30, by = "MEMBERSHIP_ID")

# pharmacy

col=pos1[,c("membershipid","visitnbr","categorynbr","retailprice")]
data = col %>%
  group_by(membershipid, categorynbr) %>%
  summarise(categoryprice = sum(retailprice))
Pharmacy=data[which(data$categorynbr==27),]
Pharmacy=Pharmacy[,-2]
names(Pharmacy)[2]="Pharmacy_spent"
head(Pharmacy)

names(Pharmacy)[1]="MEMBERSHIP_ID"
lookup30=left_join(lookup30,Pharmacy)
lookup30[is.na(lookup30)] <- 0
lookup30$regular_pharmacy <-as.numeric(lookup30$Pharmacy_spent>16)

#Furniture

col=pos1[,c("membershipid","visitnbr","categorynbr","retailprice")]
data = col %>%
  group_by(membershipid, categorynbr) %>%
  summarise(categoryprice = sum(retailprice))
fur=data[which(data$categorynbr==17),]
fur = fur[,-2]
names(fur)[2]="fur_spent"
head(fur)

names(fur)[1]="MEMBERSHIP_ID"
lookup30=left_join(lookup30,fur)
lookup30[is.na(lookup30)] <- 0
lookup30$regular_fur <-as.numeric(lookup30$fur_spent>250)

#Optical 

col=pos1[,c("membershipid","visitnbr","categorynbr","retailprice")]
data = col %>%
  group_by(membershipid, categorynbr) %>%
  summarise(categoryprice = sum(retailprice))
op=data[which(data$categorynbr==88),]
op= op[,-2]
names(op)[2]="opt_spent"
head(op)

names(op)[1]="MEMBERSHIP_ID"
lookup30=left_join(lookup30,op)
lookup30[is.na(lookup30)] <- 0
lookup30$regular_opt <-as.numeric(lookup30$opt_spent>17)

#health and beauty


col=pos1[,c("membershipid","visitnbr","categorynbr","retailprice")]
data = col %>%
  group_by(membershipid, categorynbr) %>%
  summarise(categoryprice = sum(retailprice))
heal=data[which(data$categorynbr==2),]
heal= heal[,-2]
names(heal)[2]="heal_spent"
head(heal)

names(heal)[1]="MEMBERSHIP_ID"
lookup30=left_join(lookup30,heal)
lookup30[is.na(lookup30)] <- 0
lookup30$regular_heal <-as.numeric(lookup30$heal_spent>15)


member1 = left_join(member1,lookup30, by = "MEMBERSHIP_ID")


member2 = member1
inactive=as.numeric(is.na(member2$total_visit))
member2$is_active=1-inactive


unrenewed=as.numeric(member2$RENEW_IND=="UNRENEWED")
member2$target_label=1-unrenewed
str(member2)

member2 = member2[,-1]
member2 = member2[,-(3:4)]
member2 = member2[,-(4:13)]
member3 = na.omit(member2)

write.csv(member3, 'FETrain.csv', row.names = FALSE)
