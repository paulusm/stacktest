#repdata <- ddply(data, "rep_shown", summarise, mean_rating = mean(user_rating, na.rm = TRUE), 
             #     mean_prior = mean(priorknowledge, na.rm=TRUE)) 


data$rep_shown<-as.factor(data$rep_shown)

wilcox.test(user_rating~rep_shown, data=data)

wilcox.test(priorknowledge~rep_shown, data=data)

#plotmeans(user_rating~rep_shown, data=data)

#plotmeans(priorknowledge~rep_shown, data=data)

#qqnorm(data$user_rating)