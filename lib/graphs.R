priork.graph <- function()
{
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
  return(p)
}