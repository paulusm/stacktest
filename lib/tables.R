#Participants Summary table
table.participants<-function(){
  df.participants<- data.frame(
    c('Subjects','Questions presented', 'Rated answers (cues visible)','Rated answers (cues not visible)'),
    c(length(unique(df.ug.rep$subject_id))+length(unique(df.ug.norep$subject_id)),length(unique(df.ug.rep$question_id)), nrow(df.ug.rep),nrow(df.ug.norep)),
    c(length(unique(df.msc.rep$subject_id))+length(unique(df.msc.norep$subject_id)),length(unique(df.msc.rep$question_id)),nrow(df.msc.rep), nrow(df.msc.norep)),
    c(length(unique(df.phil.rep$subject_id))+length(unique(df.phil.norep$subject_id)),length(unique(df.phil.rep$question_id)),nrow(df.phil.rep),nrow(df.phil.norep)))
  
  names(df.participants) <- c(
    'Condition',
    'Undergraduate Computing and Information Science', 
    'Postgraduate Information Science',
    'Undergraduate Philosophy')
  return (df.participants)
}

table.coding.persuasiveness <- function(df){
  #Qualitative data frames for tables
  df.bypersuasiveness<- ddply(df, c("persuasiveness","coding"), nrow)
  df.bypersuasiveness<- subset(df.bypersuasiveness, coding != 'vague or uninformative comment!' & coding != 'repetition (bug)')
  #df.bypersuasiveness<-dcast(df.bypersuasiveness,coding~persuasiveness, value_var="V1")
  df.bypersuasiveness<-df.bypersuasiveness[ order(df.bypersuasiveness[,1],-df.bypersuasiveness[,3]), ]
  df.bypersuasiveness$codecount<-paste(df.bypersuasiveness$coding," (", df.bypersuasiveness$V1, ")", sep="")
  df.bp.cols<-data.frame(
    df.bypersuasiveness[df.bypersuasiveness$persuasiveness=="low",][1:10,]$codecount,
    df.bypersuasiveness[df.bypersuasiveness$persuasiveness=="medium",][1:10,]$codecount,
    df.bypersuasiveness[df.bypersuasiveness$persuasiveness=="high",][1:10,]$codecount
  )
  names(df.bp.cols)<-c("low", "medium", "high")
  return(df.bp.cols)
}

table.coding.priork <- function(df){
    #2) By Prior Knowledge
    df.bypriork<- ddply(df, c("priork","coding"), nrow)
    df.bypriork<- subset(df.bypriork, coding != 'vague or uninformative comment!' & coding != 'repetition (bug)')
    #df.bypriork<-dcast(df.bypriork,coding~persuasiveness, value_var="V1")
    df.bypriork<-df.bypriork[ order(df.bypriork[,1],-df.bypriork[,3]), ]
    df.bypriork$codecount<-paste(df.bypriork$coding," (", df.bypriork$V1, ")", sep="")
    df.pk.cols<-data.frame(
      df.bypriork[df.bypriork$priork=="low",][1:10,]$codecount,
      df.bypriork[df.bypriork$priork=="medium",][1:10,]$codecount,
      df.bypriork[df.bypriork$priork=="high",][1:10,]$codecount
    )
    names(df.pk.cols)<-c("low", "medium", "high")
    return(df.pk.cols)
}

