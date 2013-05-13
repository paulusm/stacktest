
# see if users of particular types based on their ratings and prior knowledge

#summarise by their mean rating and prior K
userdata <- ddply(data, "subject_id", summarise, mean_rating = mean(user_rating, na.rm = TRUE), 
                  mean_prior = mean(priorknowledge, na.rm=TRUE)) #, var_rating = var(user_rating, na.rm=TRUE), 
                  #var_prior = var(priorknowledge, na.rm=TRUE))

#data.scaled <- scale(data[,c(2:17)])

# Determine number of clusters
# wss <- (nrow(questions)-1)*sum(apply(questions,2,var))
# for (i in 2:15) wss[i] <- sum(kmeans(questions,
#    centers=i)$withinss)
# plot(1:15, wss, type="b", xlab="Number of Clusters",
#   ylab="Within groups sum of squares") 
# K-Means Cluster Analysis
fit <- kmeans(userdata[,c(2:3)], 3) 
# get cluster means
user.means<-aggregate(userdata,by=list(fit$cluster),FUN=mean)

# append cluster assignment
userclust <- data.frame(userdata , fit$cluster) 
userclust$fit.cluster <- as.factor(userclust$fit.cluster)


# count per cluster
table(userclust$fit.cluster)

#plot those clusters
p <- ggplot(userclust, aes(x=mean_prior, y=mean_rating, colour=fit.cluster)) + geom_point()
p

#test significance of clusters
foo <- lm(mean_rating~fit.cluster, data=userclust)
summary(foo)
foo <- lm(mean_prior~fit.cluster, data=userclust)
summary(foo)

#plotmeans
plotmeans(mean_prior~fit.cluster, data=userclust)
plotmeans(mean_rating~fit.cluster, data=userclust)

# test for normality - only just!
shapiro.test(userclust$mean_rating)
shapiro.test(userclust$mean_prior)

#principle components analysis
#questions.srpc <- princomp(questions[,c(2:17)])
#summary(questions.pc)