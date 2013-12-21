# Preprocessing script.
data$rep_shown <- as.factor(data$rep_shown)

# derive an answer quality score
data$quality_score = log(data$score+10) + log(data$length) + data$containscode
data$quality = cut(data$quality_score, breaks = c(5, 7, 10 ,20), labels=c("low", "medium", "high"))

# derive a prior knowledge interval
data$priork = cut(data$priorknowledge, breaks = c(0,2.5,3.5,6), labels=c("low", "medium", "high"))

# derive a persuasiveness interval
data$persuasiveness = cut(data$user_rating, breaks = c(0,2.5,3.5,6), labels=c("low", "medium", "high"))

# Disaggregate by presence of cues
df.rep <- subset(data, rep_shown==1)
df.norep <- subset(data, rep_shown==0)

# Disaggregate philosophers
df.phil.rep <- subset(data, rep_shown==1 & grepl("UWEP",data$name))
df.phil.norep <- subset(data, rep_shown==0 & grepl("UWEP",data$name))
df.nonphil.rep <- subset(data, rep_shown==1 & !grepl("UWEP",data$name))
df.nonphil.norep <- subset(data, rep_shown==0 & !grepl("UWEP",data$name))

#Qualitative data frames for tables
# 1) By persuasiveness
df.bypersuasiveness<- ddply(data, c("persuasiveness","coding"), nrow)
df.bypersuasiveness<- subset(df.bypersuasiveness, coding != 'vague or uninformative comment!' & coding != 'repetition (bug)')
#df.bypersuasiveness<-dcast(df.bypersuasiveness,coding~persuasiveness, value_var="V1")
df.bypersuasiveness<-df.bypersuasiveness[ order(df.bypersuasiveness[,1],-df.bypersuasiveness[,3]), ]
df.bypersuasiveness$codecount<-paste(df.bypersuasiveness$coding," (", df.bypersuasiveness$V1, ")", sep="")
df.bp.cols<-data.frame(
    df.bypersuasiveness[df.bypersuasiveness$persuasiveness=="low",][1:20,]$codecount,
    df.bypersuasiveness[df.bypersuasiveness$persuasiveness=="medium",][1:20,]$codecount,
    df.bypersuasiveness[df.bypersuasiveness$persuasiveness=="high",][1:20,]$codecount
                       )
names(df.bp.cols)<-c("low", "medium", "high")

#2) By Prior Knowledge
df.bypriork<- ddply(data, c("priork","coding"), nrow)
df.bypriork<- subset(df.bypriork, coding != 'vague or uninformative comment!' & coding != 'repetition (bug)')
#df.bypriork<-dcast(df.bypriork,coding~persuasiveness, value_var="V1")
df.bypriork<-df.bypriork[ order(df.bypriork[,1],-df.bypriork[,3]), ]
df.bypriork$codecount<-paste(df.bypriork$coding," (", df.bypriork$V1, ")", sep="")
df.pk.cols<-data.frame(
  df.bypriork[df.bypriork$priork=="low",][1:20,]$codecount,
  df.bypriork[df.bypriork$priork=="medium",][1:20,]$codecount,
  df.bypriork[df.bypriork$priork=="high",][1:20,]$codecount
)
names(df.pk.cols)<-c("low", "medium", "high")
