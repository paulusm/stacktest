cor.test(df.norep$score, df.norep$user_rating)
cor.test(df.rep$score, df.rep$user_rating)

library(ggplot2)
p <-ggplot(data, aes(x=user_rating, y=log(score), colour=rep_shown)) + geom_point()
p