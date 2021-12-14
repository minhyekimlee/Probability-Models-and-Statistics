# load the data
flix <- read.table("C:\\Users\\minhyekim\\Documents\\R\\6312\\FlixIt.Dat", header = FALSE,
                   col.names = c("cID", "hrs", "nChild", "income", "history"))

# basic information
summary(flix)

# standard deviation
sd(flix$nChild)

sd(flix$income)

sd(flix$history)

# slr w/number of children
flix_nChild<-flix[c("hrs", "nChild")]
nChild.slr<-lm(hrs~nChild, data = flix_nChild)
plot(hrs~nChild, data = flix_nChild)
abline(nChild.slr)
summary(nChild.slr)

predict(nChild.slr, data.frame(nChild=3), interval = "predict")

# slr w/income
flix_income<-flix[c("hrs", "income")]
income.slr<-lm(hrs~income, data = flix_income)
plot(hrs~income, data = flix_income)
abline(income.slr)
summary(income.slr)

predict(income.slr, data.frame(income=53))

# slr w/history
flix_history<-flix[c("hrs", "history")]
history.slr<-lm(hrs~history, data = flix_history)
plot(hrs~history, data = flix_history)
abline(history.slr)
summary(history.slr)

predict(history.slr, data.frame(history=2))
