ggplot(user_specific2, aes(x = n, fill = cluster)) + geom_histogram(binwidth = 1) + xlim(0, 20) + xlab("# of purchases")
ggplot(user_specific2, aes(x = total_q, fill = cluster)) + geom_histogram()  + xlim(0, 100) + xlab("Total Qunatity")
ggplot(user_specific2, aes(x = avg_dist, fill = cluster)) + geom_histogram(binwidth = 100) + xlab("Average Distance")

ggplot(user_specific2, aes(x = avg_price_of_ticket, fill = cluster)) + geom_histogram(binwidth = 100) + xlab("Average Price of Ticket")
ggplot(user_specific2, aes(x = avg_event_purchase_lag, fill = cluster)) + geom_histogram(binwidth = 100) + xlab("Even - Purchase Lag")
ggplot(user_specific2, aes(x = avg_purchase_sale_lag, fill = cluster)) + geom_histogram(binwidth = 20) + xlab("Purchase - Sale Lag")
ggplot(user_specific2, aes(x = avg_event_print_lag, fill = cluster)) + geom_histogram(binwidth = 200) + xlab("Event - Print Lag")

ggplot(user_specific2, aes(x = avg_bought_in_presale, fill = cluster)) + geom_histogram() + xlab("Bought during presale Period")
ggplot(user_specific2, aes(x = total_bought_in_presale, fill = cluster)) + geom_histogram() + xlab("Total Quantity bought in presale Period")
ggplot(user_specific2, aes(x = avg_q, fill = cluster)) + geom_histogram(binwidth = 1) 
ggplot(user_specific2, aes(x = n_primary, fill = cluster)) + geom_histogram() + xlab("# of different events")

ggplot(num_purchase, aes(x = n)) + geom_histogram(binwidth = 1)

summary(purchase.sample$purchase_sale_lag)
ggplot(purchase.sample, aes(x = purchase_sale_lag)) + geom_histogram() + xlab("Purchase - Sale Lag")

summary(purchase.sample$event_purchase_lag)
ggplot(purchase.sample, aes(x = event_purchase_lag)) + geom_histogram() + xlab("Event - Purchase Lag")


summary(purchase.sample$event_print_lag)
ggplot(purchase.sample, aes(x = event_print_lag)) + geom_histogram(binwidth = 1)
