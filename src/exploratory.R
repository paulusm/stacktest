
# Correlation between visibility of cues and user's credibility rating
cor.norep<-cor.test(df.norep$score, df.norep$user_rating)
str(cor.norep)
cor.rep<-cor.test(df.rep$score, df.rep$user_rating)
tbl <- matrix(c(cor.norep$statistic, cor.rep$statistic,
                cor.norep$parameter, cor.rep$parameter,
                cor.norep$p.value, cor.rep$p.value
                ),ncol=2,byrow=TRUE)
colnames(tbl) <- c("Cues Not Shown","Cues Shown")
rownames(tbl) <- c("Correlation","df","p")
cred <- as.table(tbl)
cred


library(ggplot2)
p <-ggplot(data, aes(x=user_rating, y=log(score), colour=rep_shown)) + geom_point()
p

cor.test(df.norep$rank_presented, df.norep$user_rating)
cor.test(df.rep$rank_presented, df.rep$user_rating)

cor.test(df.norep$userrep, df.norep$user_rating)
cor.test(df.rep$userrep, df.rep$user_rating)

library(plyr)
#data$user_rating = as.numeric(data$user_rating)
means<- ddply(data, c("rank_presented","rep_shown"), summarise, mean_rating = mean(user_rating, na.rm = TRUE))
p <-ggplot(means, aes(x=rank_presented, y=mean_rating, fill=rep_shown)) + geom_bar(stat="identity",position=position_dodge())
p

means<- ddply(data, c("rank_presented","rep_shown"), summarise, mean_score = mean(score, na.rm = TRUE))
p <-ggplot(means, aes(x=rank_presented, y=mean_score, fill=rep_shown)) + geom_bar(stat="identity",position=position_dodge())
p

means<- ddply(data, c("rank_presented","rep_shown"), summarise, mean_rep = mean(userrep, na.rm = TRUE))
p <-ggplot(means, aes(x=rank_presented, y=mean_rep, fill=rep_shown)) + geom_bar(stat="identity",position=position_dodge())
p

means<- ddply(data, "rep_shown", summarise, mean_know = mean(priorknowledge, na.rm = TRUE))
p <-ggplot(means, aes(x=rep_shown, y=mean_know, fill=rep_shown)) + geom_bar(stat="identity",position=position_dodge())
p

means<- ddply(data, "rep_shown", summarise, mean_rating = mean(user_rating, na.rm = TRUE))
p <-ggplot(means, aes(x=rep_shown, y=mean_rating, fill=rep_shown)) + geom_bar(stat="identity",position=position_dodge())
p

# any link between prior k and confidence?
means<- ddply(data, c("priorknowledge", "rep_shown"), summarise, mean_rating = mean(user_rating, na.rm = TRUE))
p <-ggplot(means, aes(x=priorknowledge, y=mean_rating, fill=rep_shown)) + geom_bar(stat="identity",position=position_dodge())
p

# scatter
prior.lm <-lm(user_rating ~ priorknowledge, data = data)
coef(prior.lm)
summary(prior.lm)
p<-ggplot(data, aes(x=priorknowledge, y=user_rating, colour=quality,size=2))+geom_jitter()+geom_abline(intercept=2.51, slope=0.17)
p

# derive an answer quality score
data$quality_score = log(data$score+2) + log(data$length) + data$containscode
data$quality = cut(data$quality_score, breaks = c(5, 7, 10 ,20), labels=c("low", "medium", "high"))
