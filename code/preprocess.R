source("code/util.R")

purchase <- fread("rawdata/approved_data_purchase-v5.csv", stringsAsFactors = FALSE)

# sample 500000 rows
index <- sample(1:nrow(purchase), 500000)
purchase.sample <- purchase[index, ]
purchase.sample <- select(purchase.sample, -(gndr_cd:occpn_2nd_val))

# get logitude and latitude for each row
purchase.sample <- getLongLatDist(purchase.sample) 

########## bought_in_presale
purchase.sample$bought_in_presale <- as.factor(buyTicketDuringPresale(purchase.sample$presale_dt, purchase.sample$onsale_dt, purchase.sample$sales_ord_tran_dt))

########## factorization
purchase.sample$event_id <- as.factor(purchase.sample$event_id)
purchase.sample$primary_act_id <- as.factor(purchase.sample$primary_act_id)
purchase.sample$secondary_act_id <- as.factor(purchase.sample$secondary_act_id)
purchase.sample$purch_party_lkup_id <- as.factor(purchase.sample$purch_party_lkup_id)
purchase.sample$major_cat_name <- as.factor(purchase.sample$major_cat_name)
purchase.sample$la_event_type_cat <- as.factor(purchase.sample$la_event_type_cat)
purchase.sample$delivery_type_cd <- as.factor(purchase.sample$delivery_type_cd)
purchase.sample$venue_city <- as.factor(purchase.sample$venue_city)
purchase.sample$venue_state <- as.factor(purchase.sample$venue_state)
purchase.sample$print_flg <- as.factor(purchase.sample$print_flg)
purchase.sample$la_valid_tkt_event_flg <- as.factor(purchase.sample$la_valid_tkt_event_flg)
purchase.sample$fin_mkt_nm <- as.factor(purchase.sample$fin_mkt_nm)
purchase.sample$event_disp_name <- as.factor(purchase.sample$event_disp_name)
purchase.sample$timezn_nm <- as.factor(purchase.sample$timezn_nm)

########## convert date
purchase.sample$event_dt <- ymd(purchase.sample$event_dt)
purchase.sample$presale_dt <- ymd(purchase.sample$presale_dt)
purchase.sample$onsale_dt <- ymd(purchase.sample$onsale_dt)
purchase.sample$sales_ord_tran_dt <- ymd(purchase.sample$sales_ord_tran_dt)
purchase.sample$sales_ord_create_dttm <- ymd_hms(purchase.sample$sales_ord_create_dttm)
purchase.sample$print_dt <- ymd(purchase.sample$print_dt)
purchase.sample$event_date_time <- ymd_hms(purchase.sample$event_date_time)

########## event_purchase_lag
View(purchase.sample)
purchase.sample$event_purchase_lag <- as.numeric(purchase.sample$event_dt - purchase.sample$sales_ord_tran_dt) / (3600 * 24)
purchase.sample$event_print_lag<- as.numeric(purchase.sample$event_dt - purchase.sample$print_dt) / (3600 * 24)

# lag between purchase and sale
purchase.sample$purchase_sale_lag[purchase.sample$bought_in_presale == FALSE] <- as.numeric(purchase.sample[purchase.sample$bought_in_presale == FALSE, ]$sales_ord_tran_dt - purchase.sample[purchase.sample$bought_in_presale == FALSE, ]$onsale_dt) / (3600 * 24)
purchase.sample$purchase_sale_lag[purchase.sample$bought_in_presale == TRUE] <- as.numeric(purchase.sample[purchase.sample$bought_in_presale == TRUE, ]$sales_ord_tran_dt - purchase.sample[purchase.sample$bought_in_presale == TRUE, ]$presale_dt) / (3600 * 24)

# lag between event and print
purchase.sample$event_print_lag[is.na(purchase.sample$event_print_lag)] <- -1

########## remove outliers
purchase.sample$purchase_sale_lag[purchase.sample$purchase_sale_lag < 0] <- 0
purchase.sample <- filter(purchase.sample, purchase_sale_lag < 400)
purchase.sample <- filter(purchase.sample, event_purchase_lag < 900)

########## group by user
user_g <- group_by(purchase.sample, purch_party_lkup_id)
names(purchase.sample)
user_specific <- user_g %>% summarise(n = n(), 
                     n_event = n_distinct(as.numeric(event_id)), 
                     avg_price_of_ticket = mean(trans_face_val_amt, na.rm =T), 
                     n_primary = n_distinct(as.numeric(primary_act_id)), 
                     total_q = sum(tickets_purchased_qty, na.rm = T), 
                     
                     avg_q = mean(tickets_purchased_qty, na.rm = T), 
                
                     avg_purchase_sale_lag = mean(purchase_sale_lag, na.rm =T), 
                     
                     
                     avg_event_print_lag = mean(event_print_lag, na.rm = T), 
                     avg_event_purchase_lag = mean(event_purchase_lag, na.rm = T), 
                     avg_bought_in_presale = mean(as.numeric(bought_in_presale) - 1, na.rm = T),
                     total_bought_in_presale = sum(as.numeric(bought_in_presale) - 1),
                     avg_dist = mean(distance, na.rm = T)
                     
)

user_specific.id <- user_specific$purch_party_lkup_id
user_specific$purch_party_lkup_id <- NULL
user_specific <- as.data.frame(user_specific)





index <- createDataPartition(y = user_specific2$cluster, p = 0.8, list = F)
train <- user_specific2[index, ]
test <- user_specific2[-index, ]

model.rf <- train(cluster ~. ,data = train, method = "gbm")
model.rf
model.pred <- predict(model.rf, train)

View(purchase.sample)

purchase_570 <- num_purchase %>% filter(n == 570)
filter(purchase.sample, purch_party_lkup_id == purchase_570$purch_party_lkup_id) %>% View()

filter(purchase.sample, purch_party_lkup_id == "6cb4df50c5ebf62a4040")

filter(purchase.sample, secondary_act_id == "b85143bf51323b72e53c") %>% View()
filter(purchase.sample, primary_act_id == "43f0436b905bfa7c2eec") %>% View()
filter(purchase.sample, secondary_act_id == "43f0436b905bfa7c2eec") %>% nrow()

sort(table(purchase.sample$primary_act_id), decreasing = T)

filter(purchase.sample, primary_act_id == "4b677c3f5bec71eec8d1") %>% View()
filter(purchase.sample, primary_act_id == "1a3e9aecd0617706a794") %>% View()
filter(purchase.sample, primary_act_id == "6cdc2e270775b7e2f709") %>% View()
filter(purchase.sample, primary_act_id == "ac4b847b3fde66f2117e") %>% View()



temp <- user_g %>% summarise(n = n()) %>% filter(n > 2) 

filter(user_g, purch_party_lkup_id == temp$purch_party_lkup_id) %>% nrow()

dum <- dummyVars( ~ major_cat_name, data = purchase.sample)
dum <- as.data.frame(predict(dum, purchase.sample))
purch_party_lkup_id <- purchase.sample$purch_party_lkup_id

data.pca <- cbind(purch_party_lkup_id, dum)
data.pca_g <- group_by(data.pca, purch_party_lkup_id)

d <- data.pca_g %>% summarise_each(funs(sum))
data.pca.id <- d$purch_party_lkup_id
d$purch_party_lkup_id <- NULL
data.model.pca <- prcomp(d, center = TRUE, scale. = TRUE)
summary(data.model.pca)
plot(data.model.pca)

head(d) %>% View()

View(data.pca)

View(dum)

purchase.sample$minor_cat_name %>% table()

############## exploring 

# print flag
purchase.sample$print_flg %>% table()

View(purchase.sample)

summary(purchase.sample$event_purchase_lag)
summary(purchase.sample$event_print_lag)

purchase.sample[purchase.sample$bought_in_presale == TRUE, ] %>% nrow()

filter(purchase.sample, bought_in_presale == TRUE) %>% nrow()
filter(purchase.sample, bought_in_presale == FALSE) %>% nrow()
nrow(purchase.sample)
num_purchase <- purchase_party_id %>% summarise(n = n()) %>% filter(n > 2)
summary(num_purchase$n)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.000   3.000   4.000   4.875   5.000 570.000 



