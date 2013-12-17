# Preprocessing script.
data$rep_shown <- as.factor(data$rep_shown)
data$logscore <- log(data$score+2)
data$loguserrep <- log(data$userrep)

df.rep <- subset(data, rep_shown==1)
df.norep <- subset(data, rep_shown==0)
df.phil.rep <- subset(data, rep_shown==1 & grepl("UWEP",data$name))
df.phil.norep <- subset(data, rep_shown==0 & grepl("UWEP",data$name))
df.nonphil.rep <- subset(data, rep_shown==1 & !grepl("UWEP",data$name))
df.nonphil.norep <- subset(data, rep_shown==0 & !grepl("UWEP",data$name))