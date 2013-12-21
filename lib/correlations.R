cor.score <- function()
{
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
  return(cred)
}

cor.rep <- function()
{
  # Non parametric correlation between answerer's reputation and user's credibility rating
  cor.norep<-cor.test(df.norep$userrep, df.norep$user_rating, method="kendall")
  #str(cor.norep)
  cor.rep<-cor.test(df.rep$userrep, df.rep$user_rating, method="kendall")
  #str(cor.rep)
  tbl <- matrix(c(  round(cor.norep$estimate,3), round(cor.rep$estimate,3),
                    round(cor.norep$p.value,5), round(cor.rep$p.value,5)
  ),ncol=2,byrow=TRUE)
  colnames(tbl) <- c("Cues Not Shown","Cues Shown")
  rownames(tbl) <- c("Correlation","p")
  cred <- as.table(tbl)
  return(cred)
}