
#Regression models
lm.norep.rating<-lm(df.norep$user_rating~log(df.norep$score+10)) #*log(df.norep$userrep+1)
summary(lm.norep.rating)
lm.norep.rep<-lm(df.norep$user_rating~log(df.norep$userrep+1)) #*
summary(lm.norep.rep)
lm.rep.rating<-lm(df.rep$user_rating~log(df.rep$score+10)) #*log(df.rep$userrep+1)
summary(lm.rep.rating)
lm.rep.rep<-lm(df.rep$user_rating~log(df.rep$userrep+1)) #*log(df.rep$userrep+1)
summary(lm.rep.rep)
#str(lm.rep.rep)
# Diagnostics
#layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
#plot(lm.rep)
#plot(log(df.rep$score),df.rep$user_rating,)
#abline(lm.rep)

#Non para anova anyone, both significant!
kruskal.test(user_rating~score+userrep, df.norep)
kruskal.test(user_rating~score+userrep, df.rep)


# Nonparametric Correlation between community score and user's credibility rating
cor.norep<-cor.test(df.norep$score, df.norep$user_rating, method="kendall")
#str(cor.norep)
cor.rep<-cor.test(df.rep$score, df.rep$user_rating, method="kendall")
#str(cor.rep)
tbl <- matrix(c(  round(cor.norep$estimate,3), round(cor.rep$estimate,3),
               round(cor.norep$p.value,5), round(cor.rep$p.value,5)
                ),ncol=2,byrow=TRUE)
colnames(tbl) <- c("Cues Not Shown","Cues Shown")
rownames(tbl) <- c("Correlation","p")
cred <- as.table(tbl)
cred

# Parametric Correlation between community score and user's credibility rating
cor.norep<-cor.test(log(df.norep$score+10), df.norep$user_rating, method="pearson")
#str(cor.norep)
cor.rep<-cor.test(log(df.rep$score+10), df.rep$user_rating, method="pearson")
#str(cor.rep)
tbl <- matrix(c(  round(cor.norep$estimate,3), round(cor.rep$estimate,3),
                  round(cor.norep$p.value,5), round(cor.rep$p.value,5)
),ncol=2,byrow=TRUE)
colnames(tbl) <- c("Cues Not Shown","Cues Shown")
rownames(tbl) <- c("Correlation","p")
cred <- as.table(tbl)
cred

# Non parametric correlation between answerer's reputation and user's credibility rating
cor.norep<-cor.test(df.norep$userrep, df.norep$user_rating, method="spearman")
#str(cor.norep)
cor.rep<-cor.test(df.rep$userrep, df.rep$user_rating, method="spearman")
#str(cor.rep)
tbl <- matrix(c(  round(cor.norep$estimate,3), round(cor.rep$estimate,3),
                  round(cor.norep$p.value,5), round(cor.rep$p.value,5)
),ncol=2,byrow=TRUE)
colnames(tbl) <- c("Cues Not Shown","Cues Shown")
rownames(tbl) <- c("Correlation","p")
cred <- as.table(tbl)
cred


#Graphs
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
#means<- ddply(data, c("priorknowledge", "rep_shown"), summarise, mean_rating = mean(user_rating, na.rm = TRUE))
#p <-ggplot(means, aes(x=priorknowledge, y=mean_rating, fill=rep_shown)) + geom_bar(stat="identity",position=position_dodge())
#p

# any link between aggregated prior k and confidence?
datak<-subset(data, priork!='NA')
means<- ddply(datak, c("priork", "rep_shown"), summarise, mean_rating = mean(user_rating, na.rm = TRUE))
ses<- ddply(datak, c("priork", "rep_shown"), summarise, se_rating = sd(user_rating, na.rm=TRUE)/sqrt(length(user_rating)))
kdata<-data.frame(means, ses)
limits <- aes(ymax = mean_rating + se_rating, ymin= mean_rating - se_rating)
dodge <- position_dodge(width=0.9)
p <-ggplot(kdata, aes(x=priork, y=mean_rating, fill=rep_shown)) + geom_bar(stat="identity",position=dodge)
p<-p + geom_errorbar(limits, position=dodge, width=0.25)
p<-p + scale_y_discrete("Mean Credibility Rating")
p<-p + scale_x_discrete("Prior Knowledge (self-rating)")
p<-p + labs(fill = "Cues Visible")
p

# scatter
prior.lm <-lm(user_rating ~ priorknowledge, data = data)
coef(prior.lm)
summary(prior.lm)
p<-ggplot(data, aes(x=priorknowledge, y=user_rating, colour=quality,size=2))+geom_jitter()+geom_abline(intercept=2.51, slope=0.17)
p

# derive an answer quality score - moved to munge
#data$quality_score = log(data$score+2) + log(data$length) + data$containscode
#data$quality = cut(data$quality_score, breaks = c(5, 7, 10 ,20), labels=c("low", "medium", "high"))

#Are the ratings with cues shown significantly higher?
wilcox.test(data$user_rating~data$rep_shown)
t.test(data$user_rating~data$rep_shown)
# No - but this is not important !
