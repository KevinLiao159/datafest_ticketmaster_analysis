####### PCA

model.pr <- prcomp(user_specific, center = T, scale. = T)
summary(model.pr)
plot(model.pr)
plot(model.pr$x[, 1], model.pr$x[, 2])

####### kmeans

user_specific2 <- filter(user_specific)
model.kmeans <- kmeans(user_specific2, centers = 10)
model.kmeans$cluster %>% length()
user_specific2$cluster <- as.factor(model.kmeans$cluster)

####### rpart

model.rpart <- rpart(cluster ~ ., data = user_specific2, method = 'class')
fancyRpartPlot(model.rpart)

####### cpart
fit <- ctree(cluster ~ ., data=user_specific2)
plot(fit, main="Conditional Inference Tree for Cluster")
