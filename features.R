setwd("~/Downloads/2Fraud/proj 2_CreditCardTran ")
ptm = proc.time()


library(data.table)
library(lubridate)

train = fread('fdata.csv', stringsAsFactors = F)
#d = fread('train100k.csv', stringsAsFactors = F)
train$Date=as.Date(train$Date)
# See it has a data.table class
#str(train)
# Create another new copy
train2 = train
setDT(train2)
# Define a function to implement time-interval join in data.table
timeWinJoin = function(dt, n, byVar){
  dt1 = dt
  # Generate duplicated copy for the columns we'll join on
  # as they'll disappear after running the data.table join method,
  # n is the length of the time window
  dt1$join_ts1 = dt1$Date
  dt1$join_ts2 = dt1$Date + n
  dt1$join_rec = dt1$Recnum
  # The join conditions below are equivalent to what in the sqldf code
  keys = c(byVar, 'join_ts1<=Date', 'join_ts2>=Date', 'Recnum<=Recnum')
  dt2 = dt1[dt, on=keys, allow.cartesian=T]
  return(dt2)
}

#check = data.frame(ipJoin60s_dt$Date,ipJoin60s_dt$join_ts1,ipJoin60s_dt$Recnum,ipJoin60s_dt$Amount,ipJoin60s_dt$i.Amount)

#system.time(dtCardnum0c <- timeWinJoin(train2, 0, 'Cardnum'))


for (i in c(0,1,3,7,14,30)){
  for (j in c('Cardnum','Merch description')){
    assign(paste0("dt",j,i), timeWinJoin(train2, i, j))
  }
}

  for (i in c(0,1,3,7,14,30)){
    for (j in list(c('Cardnum','Merch description'),c('Cardnum','Merch state'),c('Cardnum','Merch zip'))){
      assign(paste0("dt",j[1],j[2],i), timeWinJoin(train2, i, j))
    }
  }

#######

  dtCardnum0agg <- dtCardnum0[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  dtMerchdescription0agg <- `dtMerch description0`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  ## Using the self-joined table to do aggregation
  dtCardnum1agg <- dtCardnum1[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]


  dtMerchdescription1agg <- `dtMerch description1`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]


  dtCardnum3agg <- dtCardnum3[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  dtMerchdescription3agg <- `dtMerch description3`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  dtCardnum7agg <- dtCardnum7[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  dtMerchdescription7agg <- `dtMerch description7`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  dtCardnum14agg <- dtCardnum14[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  dtMerchdescription14agg <- `dtMerch description14`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  dtCardnum30agg <- dtCardnum30[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  dtMerchdescription30agg <- `dtMerch description30`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  dtCardnumMerchdescription0agg <- `dtCardnumMerch description0`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]


  dtCardnumMerchstate0agg <- `dtCardnumMerch state0`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  dtCardnumMerchzip0agg <- `dtCardnumMerch zip0`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  dtCardnumMerchdescription1agg <- `dtCardnumMerch description1`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  dtCardnumMerchstate1agg <- `dtCardnumMerch state1`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  dtCardnumMerchzip1agg <- `dtCardnumMerch zip1`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  dtCardnumMerchdescription3agg <- `dtCardnumMerch description3`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  dtCardnumMerchstate3agg <- `dtCardnumMerch state3`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  dtCardnumMerchzip3agg <- `dtCardnumMerch zip3`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  dtCardnumMerchdescription7agg <- `dtCardnumMerch description7`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  dtCardnumMerchstate7agg <- `dtCardnumMerch state7`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  dtCardnumMerchzip7agg <- `dtCardnumMerch zip7`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  dtCardnumMerchdescription14agg <- `dtCardnumMerch description14`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  dtCardnumMerchstate14agg <- `dtCardnumMerch state14`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]

  dtCardnumMerchzip14agg <- `dtCardnumMerch zip14`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]
  
  dtCardnumMerchdescription30agg <- `dtCardnumMerch description30`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]
  
  dtCardnumMerchstate30agg <- `dtCardnumMerch state30`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]
  
  dtCardnumMerchzip30agg <- `dtCardnumMerch zip30`[, .(
    mean = mean(Amount),
    max = max(Amount),
    median = median(Amount),
    min = min(Amount),
    total = sum(Amount),
    count = .N),
    by=Recnum]


card_0=dtCardnum0agg[train2,on=.(Recnum)]
train$Actual_avg_card_0 = card_0$Amount/card_0$mean
train$Actual_max_card_0 = card_0$Amount/card_0$max
train$Actual_med_card_0= card_0$Amount/card_0$median
train$Actual_tot_card_0 = card_0$Amount/card_0$total

card_1=dtCardnum1agg[train2,on=.(Recnum)]
train$Actual_avg_card_1 = card_1$Amount/card_1$mean
train$Actual_max_card_1 = card_1$Amount/card_1$max
train$Actual_med_card_1= card_1$Amount/card_1$median
train$Actual_tot_card_1 = card_1$Amount/card_1$total


card_3=dtCardnum3agg[train2,on=.(Recnum)]
train$Actual_avg_card_3 = card_3$Amount/card_3$mean
train$Actual_max_card_3 = card_3$Amount/card_3$max
train$Actual_med_card_3= card_3$Amount/card_3$median
train$Actual_tot_card_3 = card_3$Amount/card_3$total


card_7=dtCardnum7agg[train2,on=.(Recnum)]
train$Actual_avg_card_7 = card_7$Amount/card_7$mean
train$Actual_max_card_7 = card_7$Amount/card_7$max
train$Actual_med_card_7= card_7$Amount/card_7$median
train$Actual_tot_card_7 = card_7$Amount/card_7$total


card_14=dtCardnum14agg[train2,on=.(Recnum)]
train$Actual_avg_card_14 = card_14$Amount/card_14$mean
train$Actual_max_card_14 = card_14$Amount/card_14$max
train$Actual_med_card_14= card_14$Amount/card_14$median
train$Actual_tot_card_14 = card_14$Amount/card_14$total


card_30=dtCardnum30agg[train2,on=.(Recnum)]
train$Actual_avg_card_30 = card_30$Amount/card_30$mean
train$Actual_max_card_30 = card_30$Amount/card_30$max
train$Actual_med_card_30= card_30$Amount/card_30$median
train$Actual_tot_card_30 = card_30$Amount/card_30$total


md_0= dtMerchdescription0agg[train2,on=.(Recnum)]
train$Actual_avg_md_0 = md_0$Amount/md_0$mean
train$Actual_max_md_0 = md_0$Amount/md_0$max
train$Actual_med_md_0 = md_0$Amount/md_0$median
train$Actual_tot_md_0 = md_0$Amount/md_0$total

md_1= dtMerchdescription1agg[train2,on=.(Recnum)]
train$Actual_avg_md_1 = md_1$Amount/md_1$mean
train$Actual_max_md_1 = md_1$Amount/md_1$max
train$Actual_med_md_1 = md_1$Amount/md_1$median
train$Actual_tot_md_1 = md_1$Amount/md_1$total


md_3= dtMerchdescription3agg[train2,on=.(Recnum)]
train$Actual_avg_md_3 = md_3$Amount/md_3$mean
train$Actual_max_md_3 = md_3$Amount/md_3$max
train$Actual_med_md_3 = md_3$Amount/md_3$median
train$Actual_tot_md_3 = md_3$Amount/md_3$total


md_7= dtMerchdescription7agg[train2,on=.(Recnum)]
train$Actual_avg_md_7 = md_7$Amount/md_7$mean
train$Actual_max_md_7 = md_7$Amount/md_7$max
train$Actual_med_md_7 = md_7$Amount/md_7$median
train$Actual_tot_md_7 = md_7$Amount/md_7$total


md_14= dtMerchdescription14agg[train2,on=.(Recnum)]
train$Actual_avg_md_14 = md_14$Amount/md_14$mean
train$Actual_max_md_14 = md_14$Amount/md_14$max
train$Actual_med_md_14 = md_14$Amount/md_14$median
train$Actual_tot_md_14 = md_14$Amount/md_14$total


md_30= dtMerchdescription30agg[train2,on=.(Recnum)]
train$Actual_avg_md_30 = md_30$Amount/md_30$mean
train$Actual_max_md_30 = md_30$Amount/md_30$max
train$Actual_med_md_30 = md_30$Amount/md_30$median
train$Actual_tot_md_30 = md_30$Amount/md_30$total


card_md0= dtCardnumMerchdescription0agg[train2,on=.(Recnum)]
train$Actual_avg_card_md0 = card_md0$Amount/card_md0$mean
train$Actual_max_card_md0 = card_md0$Amount/card_md0$max
train$Actual_med_card_md0= card_md0$Amount/card_md0$median
train$Actual_tot_card_md0 = card_md0$Amount/card_md0$total

card_ms0= dtCardnumMerchstate0agg[train2,on=.(Recnum)]
train$Actual_avg_card_ms0 = card_ms0$Amount/card_ms0$mean
train$Actual_max_card_ms0 = card_ms0$Amount/card_ms0$max
train$Actual_med_card_ms0 = card_ms0$Amount/card_ms0$median
train$Actual_tot_card_ms0 = card_ms0$Amount/card_ms0$total

card_mz0= dtCardnumMerchzip0agg[train2,on=.(Recnum)]
train$Actual_avg_card_mz0= card_mz0$Amount/card_mz0$mean
train$Actual_max_card_mz0 = card_mz0$Amount/card_mz0$max
train$Actual_med_card_mz0 = card_mz0$Amount/card_mz0$median
train$Actual_tot_card_mz0 = card_mz0$Amount/card_mz0$total

card_md1= dtCardnumMerchdescription1agg[train2,on=.(Recnum)]
train$Actual_avg_card_md1 = card_md1$Amount/card_md1$mean
train$Actual_max_card_md1 = card_md1$Amount/card_md1$max
train$Actual_med_card_md1= card_md1$Amount/card_md1$median
train$Actual_tot_card_md1 = card_md1$Amount/card_md1$total

card_ms1= dtCardnumMerchstate1agg[train2,on=.(Recnum)]
train$Actual_avg_card_ms1 = card_ms1$Amount/card_ms1$mean
train$Actual_max_card_ms1 = card_ms1$Amount/card_ms1$max
train$Actual_med_card_ms1 = card_ms1$Amount/card_ms1$median
train$Actual_tot_card_ms1 = card_ms1$Amount/card_ms1$total

card_mz1= dtCardnumMerchzip1agg[train2,on=.(Recnum)]
train$Actual_avg_card_mz1= card_mz1$Amount/card_mz1$mean
train$Actual_max_card_mz1 = card_mz1$Amount/card_mz1$max
train$Actual_med_card_mz1 = card_mz1$Amount/card_mz1$median
train$Actual_tot_card_mz1 = card_mz1$Amount/card_mz1$total


card_md3= dtCardnumMerchdescription3agg[train2,on=.(Recnum)]
train$Actual_avg_card_md3 = card_md3$Amount/card_md3$mean
train$Actual_max_card_md3 = card_md3$Amount/card_md3$max
train$Actual_med_card_md3= card_md3$Amount/card_md3$median
train$Actual_tot_card_md3 = card_md3$Amount/card_md3$total

card_ms3= dtCardnumMerchstate3agg[train2,on=.(Recnum)]
train$Actual_avg_card_ms3 = card_ms3$Amount/card_ms3$mean
train$Actual_max_card_ms3 = card_ms3$Amount/card_ms3$max
train$Actual_med_card_ms3 = card_ms3$Amount/card_ms3$median
train$Actual_tot_card_ms3 = card_ms3$Amount/card_ms3$total

card_mz3= dtCardnumMerchzip3agg[train2,on=.(Recnum)]
train$Actual_avg_card_mz3= card_mz3$Amount/card_mz3$mean
train$Actual_max_card_mz3 = card_mz3$Amount/card_mz3$max
train$Actual_med_card_mz3 = card_mz3$Amount/card_mz3$median
train$Actual_tot_card_mz3 = card_mz3$Amount/card_mz3$total


card_md7= dtCardnumMerchdescription7agg[train2,on=.(Recnum)]
train$Actual_avg_card_md7 = card_md7$Amount/card_md7$mean
train$Actual_max_card_md7 = card_md7$Amount/card_md7$max
train$Actual_med_card_md7= card_md7$Amount/card_md7$median
train$Actual_tot_card_md7 = card_md7$Amount/card_md7$total

card_ms7= dtCardnumMerchstate7agg[train2,on=.(Recnum)]
train$Actual_avg_card_ms7 = card_ms7$Amount/card_ms7$mean
train$Actual_max_card_ms7 = card_ms7$Amount/card_ms7$max
train$Actual_med_card_ms7 = card_ms7$Amount/card_ms7$median
train$Actual_tot_card_ms7 = card_ms7$Amount/card_ms7$total

card_mz7= dtCardnumMerchzip7agg[train2,on=.(Recnum)]
train$Actual_avg_card_mz7= card_mz7$Amount/card_mz7$mean
train$Actual_max_card_mz7 = card_mz7$Amount/card_mz7$max
train$Actual_med_card_mz7 = card_mz7$Amount/card_mz7$median
train$Actual_tot_card_mz7 = card_mz7$Amount/card_mz7$total


card_md14= dtCardnumMerchdescription14agg[train2,on=.(Recnum)]
train$Actual_avg_card_md14 = card_md14$Amount/card_md14$mean
train$Actual_max_card_md14 = card_md14$Amount/card_md14$max
train$Actual_med_card_md14= card_md14$Amount/card_md14$median
train$Actual_tot_card_md14 = card_md14$Amount/card_md14$total

card_ms14= dtCardnumMerchstate14agg[train2,on=.(Recnum)]
train$Actual_avg_card_ms14 = card_ms14$Amount/card_ms14$mean
train$Actual_max_card_ms14 = card_ms14$Amount/card_ms14$max
train$Actual_med_card_ms14 = card_ms14$Amount/card_ms14$median
train$Actual_tot_card_ms14 = card_ms14$Amount/card_ms14$total

card_mz14= dtCardnumMerchzip14agg[train2,on=.(Recnum)]
train$Actual_avg_card_mz14= card_mz14$Amount/card_mz14$mean
train$Actual_max_card_mz14 = card_mz14$Amount/card_mz14$max
train$Actual_med_card_mz14 = card_mz14$Amount/card_mz14$median
train$Actual_tot_card_mz14 = card_mz14$Amount/card_mz14$total


card_md30= dtCardnumMerchdescription30agg[train2,on=.(Recnum)]
train$Actual_avg_card_md30 = card_md30$Amount/card_md30$mean
train$Actual_max_card_md30 = card_md30$Amount/card_md30$max
train$Actual_med_card_md30= card_md30$Amount/card_md30$median
train$Actual_tot_card_md30 = card_md30$Amount/card_md30$total

card_ms30= dtCardnumMerchstate30agg[train2,on=.(Recnum)]
train$Actual_avg_card_ms30 = card_ms30$Amount/card_ms30$mean
train$Actual_max_card_ms30 = card_ms30$Amount/card_ms30$max
train$Actual_med_card_ms30 = card_ms30$Amount/card_ms30$median
train$Actual_tot_card_ms30 = card_ms30$Amount/card_ms30$total

card_mz30= dtCardnumMerchzip30agg[train2,on=.(Recnum)]
train$Actual_avg_card_mz30= card_mz30$Amount/card_mz30$mean
train$Actual_max_card_mz30 = card_mz30$Amount/card_mz30$max
train$Actual_med_card_mz30 = card_mz30$Amount/card_mz30$median
train$Actual_tot_card_mz30 = card_mz30$Amount/card_mz30$total


alldata=data.table(train,card_0[,2:7],card_1[,2:7],card_3[,2:7],card_7[,2:7],card_14[,2:7],card_30[,2:7],md_0[,2:7],md_1[,2:7],md_3[,2:7],md_7[,2:7],md_14[,2:7],
                   md_30[,2:7],card_md0[,2:7],card_md1[,2:7],card_md3[,2:7],card_md7[,2:7],card_md14[,2:7],card_md30[,2:7],card_ms0[,2:7],card_ms1[,2:7],
                   card_ms3[,2:7],card_ms7[,2:7],card_ms14[,2:7],card_ms30[,2:7],card_mz0[,2:7],card_mz1[,2:7],card_mz3[,2:7],card_mz7[,2:7],card_mz14[,2:7]
                   ,card_mz30[,2:7])

colnames(alldata)=c(colnames(alldata)[1:130],'card_0_mean','card_0_max','card_0_median','card_0_min','card_0_total','card_0_count','card_1_mean','card_1_max','card_1_median','card_1_min','card_1_total','card_1_count','card_3_mean','card_3_max','card_3_median','card_3_min','card_3_total','card_3_count','card_7_mean','card_7_max','card_7_median','card_7_min','card_7_total','card_7_count','card_14_mean','card_14_max','card_14_median','card_14_min','card_14_total','card_14_count','card_30_mean','card_30_max','card_30_median','card_30_min','card_30_total','card_30_count','md_0_mean','md_0_max','md_0_median','md_0_min','md_0_total','md_0_count','md_1_mean','md_1_max','md_1_median','md_1_min','md_1_total','md_1_count','md_3_mean','md_3_max','md_3_median','md_3_min','md_3_total','md_3_count','md_7_mean','md_7_max','md_7_median','md_7_min','md_7_total','md_7_count','md_14_mean','md_14_max','md_14_median','md_14_min','md_14_total','md_14_count','md_30_mean','md_30_max','md_30_median','md_30_min','md_30_total','md_30_count','card_md0_mean','card_md0_max','card_md0_median','card_md0_min','card_md0_total','card_md0_count','card_md1_mean','card_md1_max','card_md1_median','card_md1_min','card_md1_total','card_md1_count','card_md3_mean','card_md3_max','card_md3_median','card_md3_min','card_md3_total','card_md3_count','card_md7_mean','card_md7_max','card_md7_median','card_md7_min','card_md7_total','card_md7_count','card_md14_mean','card_md14_max','card_md14_median','card_md14_min','card_md14_total','card_md14_count','card_md30_mean','card_md30_max','card_md30_median','card_md30_min','card_md30_total','card_md30_count','cars_ms0_mean','cars_ms0_max','cars_ms0_median','cars_ms0_min','cars_ms0_total','cars_ms0_count','cars_ms1_mean','cars_ms1_max','cars_ms1_median','cars_ms1_min','cars_ms1_total','cars_ms1_count','cars_ms3_mean','cars_ms3_max','cars_ms3_median','cars_ms3_min','cars_ms3_total','cars_ms3_count','cars_ms7_mean','cars_ms7_max','cars_ms7_median','cars_ms7_min','cars_ms7_total','cars_ms7_count','cars_ms14_mean','cars_ms14_max','cars_ms14_median','cars_ms14_min','cars_ms14_total','cars_ms14_count','cars_ms30_mean','cars_ms30_max','cars_ms30_median','cars_ms30_min','cars_ms30_total','cars_ms30_count','cars_mz0_mean','cars_mz0_max','cars_mz0_median','cars_mz0_min','cars_mz0_total','cars_mz0_count','cars_mz1_mean','cars_mz1_max','cars_mz1_median','cars_mz1_min','cars_mz1_total','cars_mz1_count','cars_mz3_mean','cars_mz3_max','cars_mz3_median','cars_mz3_min','cars_mz3_total','cars_mz3_count','cars_mz7_mean','cars_mz7_max','cars_mz7_median','cars_mz7_min','cars_mz7_total','cars_mz7_count','cars_mz14_mean','cars_mz14_max','cars_mz14_median','cars_mz14_min','cars_mz14_total','cars_mz14_count','cars_mz30_mean','cars_mz30_max','cars_mz30_median','cars_mz30_min','cars_mz30_total','cars_mz30_count')
proc.time()-ptm
write.csv(alldata,file = "var310.csv")

#table(alldata$Fraud)






