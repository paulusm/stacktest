cor.score <- function()
{
  # Nonparametric Correlation between community score and user's credibility rating
  cor.tot.norep<-cor.test(df.norep$score, df.norep$user_rating, method="kendall")
  cor.tot.rep<-cor.test(df.rep$score, df.rep$user_rating, method="kendall")
  
  cor.ug.norep<-cor.test(df.ug.norep$score, df.ug.norep$user_rating, method="kendall")
  cor.ug.rep<-cor.test(df.ug.rep$score, df.ug.rep$user_rating, method="kendall")
  
  cor.msc.norep<-cor.test(df.msc.norep$score, df.msc.norep$user_rating, method="kendall")
  cor.msc.rep<-cor.test(df.msc.rep$score, df.msc.rep$user_rating, method="kendall")
  
  cor.phil.norep<-cor.test(df.phil.norep$score, df.phil.norep$user_rating, method="kendall")
  cor.phil.rep<-cor.test(df.phil.rep$score, df.phil.rep$user_rating, method="kendall")
  
  cor.score.summary<-data.frame(
    c(
      paste(round(cor.ug.norep$estimate,3)," (", format(round(cor.ug.norep$p.value,3),nsmall=3),")", sep=""),
      paste(round(cor.msc.norep$estimate,3)," (", format(round(cor.msc.norep$p.value,3),nsmall=3),")", sep=""),
      paste(round(cor.phil.norep$estimate,3)," (", format(round(cor.phil.norep$p.value,3),nsmall=3),")", sep=""),
      paste(round(cor.tot.norep$estimate,3)," (", format(round(cor.tot.norep$p.value,3),nsmall=3),")", sep="")
    ),
    c(
      paste(round(cor.ug.rep$estimate,3)," (", format(round(cor.ug.rep$p.value,3),nsmall=3),")", sep=""),
      paste(round(cor.msc.rep$estimate,3)," (", format(round(cor.msc.rep$p.value,3),nsmall=3),")", sep=""),
      paste(round(cor.phil.rep$estimate,3)," (", format(round(cor.phil.rep$p.value,3),nsmall=3),")", sep=""),
      paste(round(cor.tot.rep$estimate,3)," (", format(round(cor.tot.rep$p.value,3),nsmall=3),")", sep="")
    )
  )
  
  colnames(cor.score.summary) <- c("Cues Not Shown","Cues Shown")
  rownames(cor.score.summary) <- c("UG Comp/IS","PG IS","UG Phil","Overall")

  return(cor.score.summary)
}

cor.rep <- function()
{
  # Non parametric correlation between answerer's reputation and user's credibility rating
  cor.tot.norep<-cor.test(df.norep$userrep, df.norep$user_rating, method="kendall")
  cor.tot.rep<-cor.test(df.rep$userrep, df.rep$user_rating, method="kendall")
  
  cor.ug.norep<-cor.test(df.ug.norep$userrep, df.ug.norep$user_rating, method="kendall")
  cor.ug.rep<-cor.test(df.ug.rep$userrep, df.ug.rep$user_rating, method="kendall")
  
  cor.msc.norep<-cor.test(df.msc.norep$userrep, df.msc.norep$user_rating, method="kendall")
  cor.msc.rep<-cor.test(df.msc.rep$userrep, df.msc.rep$user_rating, method="kendall")
  
  cor.phil.norep<-cor.test(df.phil.norep$userrep, df.phil.norep$user_rating, method="kendall")
  cor.phil.rep<-cor.test(df.phil.rep$userrep, df.phil.rep$user_rating, method="kendall")
  
  cor.rep.summary<-data.frame(
    c(
      paste(round(cor.ug.norep$estimate,3)," (", format(round(cor.ug.norep$p.value,3),nsmall=3),")", sep=""),
      paste(round(cor.msc.norep$estimate,3)," (", format(round(cor.msc.norep$p.value,3),nsmall=3),")", sep=""),
      paste(round(cor.phil.norep$estimate,3)," (", format(round(cor.phil.norep$p.value,3),nsmall=3),")", sep=""),
      paste(round(cor.tot.norep$estimate,3)," (", format(round(cor.tot.norep$p.value,3),nsmall=3),")", sep="")
    ),
    c(
      paste(round(cor.ug.rep$estimate,3)," (", format(round(cor.ug.rep$p.value,3),nsmall=3),")", sep=""),
      paste(round(cor.msc.rep$estimate,3)," (", format(round(cor.msc.rep$p.value,3),nsmall=3),")", sep=""),
      paste(round(cor.phil.rep$estimate,3)," (", format(round(cor.phil.rep$p.value,3),nsmall=3),")", sep=""),
      paste(round(cor.tot.rep$estimate,3)," (", format(round(cor.tot.rep$p.value,3),nsmall=3),")", sep="")
    )
  )
  
  colnames(cor.rep.summary) <- c("Cues Not Shown","Cues Shown")
  rownames(cor.rep.summary) <- c("UG Comp/IS","PG IS","UG Phil","Overall")
  
  return(cor.rep.summary)
}