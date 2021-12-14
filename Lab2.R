# load the data
flix <- read.table("C:\\Users\\minhyekim\\Documents\\R\\6312\\FlixIt.Dat", header = FALSE,
                   col.names = c("cID", "hrs", "nChild", "income", "history"))

# slr w/number of children
flix_nChild<-flix[c("hrs", "nChild")]
nChild.slr<-lm(hrs~nChild, data = flix_nChild)
summary(nChild.slr)

# slr w/income
flix_income<-flix[c("hrs", "income")]
income.slr<-lm(hrs~income, data = flix_income)
summary(income.slr)

# slr w/history
flix_history<-flix[c("hrs", "history")]
history.slr<-lm(hrs~history, data = flix_history)
summary(history.slr)

# mr
flix_mr <- lm(hrs~.-cID, data=flix)
summary(flix_mr)

# mr coefficient of partial determination
require(heplots)
etasq(flix_mr, anova=TRUE, partial=FALSE)