# Preprocessing script.
data$rep_shown <- as.factor(data$rep_shown)
#data$coding <- as.factor(data$coding)

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



