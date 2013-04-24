# Preprocessing script.
data$rep_shown <- as.factor(data$rep_shown)
df.rep <- subset(data, rep_shown==1)
df.norep <- subset(data, rep_shown==0)