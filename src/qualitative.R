#comment classification by 

# 1) level of credibility of answers
df.bypersuasiveness<- ddply(data, c("persuasiveness","coding"), nrow)
df.bypersuasiveness<- subset(df.bypersuasiveness, coding != 'vague or uninformative comment!' )
df.bypersuasiveness<-df.bypersuasiveness[ order(df.bypersuasiveness[,1], -df.bypersuasiveness[,3]), ]

# 2) Prior knowledge of topic
df.bypriork<- ddply(data, c("priork","coding"), nrow)
df.bypriork<- subset(df.bypriork, coding != 'vague or uninformative comment!' )
df.bypriork<-df.bypriork[ order(df.bypriork[,1], -df.bypriork[,3]), ]

# 2) Cues shown or hidden
df.bycues<- ddply(data, c("rep_shown","coding"), nrow)
df.bycues<- subset(df.bycues, coding != 'vague or uninformative comment!' )
df.bycues<-df.bycues[ order(df.bycues[,1], -df.bycues[,3]), ]