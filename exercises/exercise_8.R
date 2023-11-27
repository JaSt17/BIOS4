# load packages

# get the data
dat <- read.csv("~/BIOS4/data/Eulaema.csv")
names(dat)
# createa a model
m1 <- lm(Eulaema_nigrita ~ effort + MAP + Pseason + forest., data=dat)
m2 <- lm(Eulaema_nigrita ~ effort + MAP + Pseason, data=dat)
m3 <- lm(Eulaema_nigrita ~ effort + MAP + forest., data=dat)
m4 <- lm(Eulaema_nigrita ~ effort + forest. + Pseason, data=dat)
m5 <- lm(Eulaema_nigrita ~ effort + MAP, data=dat)
m6 <- lm(Eulaema_nigrita ~ effort + Pseason, data=dat)
m7 <- lm(Eulaema_nigrita ~ effort + forest., data=dat)

anova(m1, m2, m3, m4, m5, m6, m7)
mlist = list(m1, m2, m3, m4, m5, m6, m7)
AICTab = AIC(m1, m2, m3, m4, m5, m6, m7)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab
