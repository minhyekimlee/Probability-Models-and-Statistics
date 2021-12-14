# load the data
odn <- read.table("C:\\Users\\minhyekim\\Documents\\R\\6312\\Odnetnin.Dat", header = TRUE)
summary(odn)

# slr w/Healthiness
heal <-lm(Costs~Healthiness, data = odn)
summary(heal)

# slr w/Engagement
eng <-lm(Costs~Engagement, data = odn)
summary(eng)

# mr
odn_mr <- lm(Costs~., data=odn)
summary(odn_mr)

# mr coefficient of partial determination
require(heplots)
etasq(odn_mr, anova=TRUE, partial=FALSE)

# corr
install.packages("agricolae")
library(agricolae)
correlation(odn$Engagement,odn$Healthiness, method="pearson")

# predict
predict(odn_mr, data.frame(Healthiness=50, Engagement=25))