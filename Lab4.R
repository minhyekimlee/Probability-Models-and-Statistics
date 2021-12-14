# load the data
affi <- read.table("C:\\Users\\minhyekim\\Documents\\R\\6312\\Aristocrats.Dat", header = TRUE,
                   sep="", na.strings="NA", dec=".", strip.white=TRUE)
affi$Combo <- as.factor(affi$Combo)
summary(affi)

library(stats)
xtabs(~Combo+Temp, data=affi)

# at the bivariate level
library(psych)
describeBy(affi$NetProfit, affi$Combo)

# describe relationships
library(gplots)
plotmeans(NetProfit~Combo, data=affi, main="Plot of Means")
plotmeans(NetProfit~Temp, data=affi, main="Plot of Means")

ANOVA <- lm(NetProfit~Combo, data=affi)
summary(ANOVA)

# A-Posteriori Test
library(agricolae)
scheffe.test(ANOVA, "Combo", group=TRUE, console=TRUE, main="Scheffe Test")

# are "Combo" and "Temp" related?
plotmeans(Temp~Combo, data=affi, main="Plot of Means")
etasq(lm(Temp~Combo, data=affi), anova=TRUE, partial=FALSE)

# at the multivariate level
# ANACOVA analysis
ANACOVA <- lm(NetProfit~Combo + Temp, data=affi)
summary(ANACOVA)

library(lsmeans)
lsmeans(ANACOVA,"Combo")

library(heplots)
# assessing the magnitude of confounding: Temp
etasq(ANOVA, anova=TRUE, partial=FALSE)
etasq(ANACOVA, anova=TRUE, partial=FALSE)


# calculation of confounding
(0.15126-0.124637)/0.15126
