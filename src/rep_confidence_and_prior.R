#repdata <- ddply(data, "rep_shown", summarise, mean_rating = mean(user_rating, na.rm = TRUE), 
             #     mean_prior = mean(priorknowledge, na.rm=TRUE)) 

str(t.test(user_rating~rep_shown, data=data))

t.test(priorknowledge~rep_shown, data=data)

plotmeans(user_rating~rep_shown, data=data)

plotmeans(priorknowledge~rep_shown, data=data)