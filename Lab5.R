# load the data
res <- read.table("C:\\Users\\minhyekim\\Documents\\R\\6312\\Restaurant.txt", header = TRUE)

res$Pref <- as.factor(res$Pref)
res$Agegroup <- as.factor(res$Agegroup)

summary(res)

table(res$Agegroup)
table(res$Pref)


library(stats)
xtabs(~Pref + Agegroup, data=res)

# Question 1: 28-37 / B
prop.test(x=48, n=192, p=0.20, alternative="greater", conf.level=0.95, correct=FALSE)

# Question 2: 28-37, 38-47 / C
prop.test(x=c(15,17), n=c(192,260), alternative="greater", conf.level=0.95, correct=FALSE)

# Question 3: R & B
prop.test(x=576, n=(576+322), p=0.60, alternative="greater", conf.level=0.95, correct=FALSE)
