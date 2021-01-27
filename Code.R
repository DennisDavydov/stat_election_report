library(ggplot2)
library(e1071)
library(rstatix)
library(ggpubr)
library(car)
library(dplyr)
library(psych)
library(readr)
KA <- read_csv("Downloads/Kuntien_avainluvut(1).csv", 
               locale = locale(encoding = "ISO-8859-1"))
tulot2017 <- read_csv("~/Desktop/University/DA_Project/tulot2017.csv", 
               locale = locale(encoding = "ISO-8859-1"))
ek2019 <- read_csv("~/Desktop/University/DA_Project/ek2019.csv", 
                      locale = locale(encoding = "ISO-8859-1"))
#rurality index
rurality <- merge(KA, ek2019, by = "Alue")
rurality <- rurality[c("Vaalipiiri", "Taajama-aste, %, 2018")]
describeBy(rurality, rurality$Vaalipiiri)
ruralityO <- rurality[rurality$Vaalipiiri == "Oulu",]
ruralityU <- rurality[rurality$Vaalipiiri == "Uusimaa",]
describe(ruralityO)
describe(ruralityU)
t.test(ruralityO$`Taajama-aste, %, 2018`, ruralityU$`Taajama-aste, %, 2018`, var.equal = FALSE)
#data preparation
data <- merge(tulot2017, ek2019, by="Alue",all=FALSE)
uusimaa <- data[data$Vaalipiiri == "Uusimaa", c("Mediaanitulot","SDP","PS","KOK","KESK", "VIHR", "VAS","RKP","KD")]
oulu <- data[data$Vaalipiiri == "Oulu", c("Mediaanitulot","SDP","PS","KOK","KESK", "VIHR", "VAS","RKP","KD")] 
uusioulu <- data[(data$Vaalipiiri == "Oulu" | data$Vaalipiiri== "Uusimaa"), c("Mediaanitulot","SDP","PS","KOK","KESK", "VIHR", "VAS","RKP","KD", "Vaalipiiri")]
describe(oulu)
describe(uusimaa)
#normality
shapirot <- c()
for (x in 1:(length(colnames(data))-2)) {
  shapirot <- c(shapirot, shapiro.test(as.numeric(unlist(data[x+1])))$p.value)
}
shapirot
qqPlot(uusioulu$Mediaanitulot)
qqPlot(uusioulu$SDP)
qqPlot(uusioulu$PS)
qqPlot(uusioulu$KOK)
qqPlot(uusioulu$KESK)
qqPlot(uusioulu$VIHR)
qqPlot(uusioulu$VAS)
qqPlot(uusioulu$RKP)
qqPlot(uusioulu$KD)
#average support
avSup <- aggregate(uusioulu[, 1:9], list(uusioulu$Vaalipiiri), mean)
#create chartable supports
vaalipiiri <- rep(avSup$Group.1, 8)
supports <- c(avSup$SDP,avSup$PS,avSup$KOK,avSup$KESK,avSup$VIHR, avSup$VAS, avSup$RKP, avSup$KD)
names <- c()
for (var in colnames(avSup)[3:10]) {
  names <- c(names, rep(var, 2))
}
chartable <- data.frame("Vaalipiiri"= vaalipiiri, "Support" = supports, "Party" = names)
#plot supports
figure1<-ggplot(chartable, aes(fill=Vaalipiiri, y=Support, x=Party)) + 
  geom_bar(position="dodge", stat="identity") +
  xlab("Party") + ylab("Support")
#create chartable median income
figure2<-ggplot(avSup, aes(y=Mediaanitulot, x=Group.1)) + 
  geom_bar(position="dodge", stat="identity") +
  xlab("Major Voting Area") + ylab("Median Income")
#t-tests
t.test(oulu$SDP, uusimaa$SDP, var.equal = FALSE)
t.test(oulu$PS, uusimaa$PS, var.equal = FALSE)
t.test(oulu$KOK, uusimaa$KOK, var.equal = FALSE)
t.test(oulu$KESK, uusimaa$KESK, var.equal = FALSE)
t.test(oulu$VIHR, uusimaa$VIHR, var.equal = FALSE)
t.test(oulu$VAS, uusimaa$VAS, var.equal = FALSE)
t.test(oulu$RKP, uusimaa$RKP, var.equal = FALSE)
t.test(oulu$KD, uusimaa$KD, var.equal = FALSE)
t.test(oulu$Mediaanitulot, uusimaa$Mediaanitulot, var.equal = FALSE)

#correlations
cors_oulu <-c()
for (y in 1:8) {
    cors_oulu <- c(cors_oulu, cor(oulu$Mediaanitulot, oulu[1+y], method = "pearson", use = "complete.obs"))
}
print(cors_oulu)

cors_uusimaa <-c()
for (y in 1:8) {
  cors_uusimaa <- c(cors_uusimaa, cor(uusimaa$Mediaanitulot, uusimaa[1+y], method = "pearson", use = "complete.obs"))
}
print(cors_uusimaa)

#ANCOVA models

modelSDP <- lm(SDP ~ Vaalipiiri + Mediaanitulot + Vaalipiiri:Mediaanitulot, data = uusioulu)
anova(modelSDP)
modelPS <- lm(PS ~ Vaalipiiri + Mediaanitulot + Vaalipiiri:Mediaanitulot, data = uusioulu)
anova(modelPS)
modelKOK <- lm(KOK ~ Vaalipiiri + Mediaanitulot + Vaalipiiri:Mediaanitulot, data = uusioulu)
anova(modelKOK)
modelKESK <- lm(KESK ~ Vaalipiiri + Mediaanitulot + Vaalipiiri:Mediaanitulot, data = uusioulu)
anova(modelKESK)
modelVIHR <- lm(VIHR ~ Vaalipiiri + Mediaanitulot + Vaalipiiri:Mediaanitulot, data = uusioulu)
anova(modelVIHR)
modelVAS <- lm(VAS ~ Vaalipiiri + Mediaanitulot + Vaalipiiri:Mediaanitulot, data = uusioulu)
anova(modelVAS)
modelRKP <- lm(RKP ~ Vaalipiiri + Mediaanitulot + Vaalipiiri:Mediaanitulot, data = uusioulu)
anova(modelRKP)
modelKD <- lm(KD ~ Vaalipiiri + Mediaanitulot + Vaalipiiri:Mediaanitulot, data = uusioulu)
anova(modelKD)
plot(modelSDP, add.smooth = FALSE, which = 1)
plot(modelSDP, which = 2)
plot(model, add.smooth = FALSE, which = 3)
#ANCOVA model graphs
pred.dataSDP <- expand.grid(Mediaanitulot = c(min(uusioulu$Mediaanitulot), max(uusioulu$Mediaanitulot)), Vaalipiiri = c("Oulu", "Uusimaa"))
pred.dataSDP <- mutate(pred.dataSDP, SDP = predict(modelSDP, pred.dataSDP))

pred.dataPS <- expand.grid(Mediaanitulot = c(min(uusioulu$Mediaanitulot), max(uusioulu$Mediaanitulot)), Vaalipiiri = c("Oulu", "Uusimaa"))
pred.dataPS <- mutate(pred.dataPS, PS = predict(modelPS, pred.dataPS))

pred.dataKOK <- expand.grid(Mediaanitulot = c(min(uusioulu$Mediaanitulot), max(uusioulu$Mediaanitulot)), Vaalipiiri = c("Oulu", "Uusimaa"))
pred.dataKOK <- mutate(pred.dataKOK, KOK = predict(modelKOK, pred.dataKOK))

pred.dataKESK <- expand.grid(Mediaanitulot = c(min(uusioulu$Mediaanitulot), max(uusioulu$Mediaanitulot)), Vaalipiiri = c("Oulu", "Uusimaa"))
pred.dataKESK <- mutate(pred.dataKESK, KESK = predict(modelKESK, pred.dataKESK))

pred.dataVIHR <- expand.grid(Mediaanitulot = c(min(uusioulu$Mediaanitulot), max(uusioulu$Mediaanitulot)), Vaalipiiri = c("Oulu", "Uusimaa"))
pred.dataVIHR <- mutate(pred.dataVIHR, VIHR = predict(modelVIHR, pred.dataVIHR))

pred.dataVAS <- expand.grid(Mediaanitulot = c(min(uusioulu$Mediaanitulot), max(uusioulu$Mediaanitulot)), Vaalipiiri = c("Oulu", "Uusimaa"))
pred.dataVAS <- mutate(pred.dataVAS, VAS = predict(modelVAS, pred.dataVAS))

pred.dataRKP <- expand.grid(Mediaanitulot = c(min(uusioulu$Mediaanitulot), max(uusioulu$Mediaanitulot)), Vaalipiiri = c("Oulu", "Uusimaa"))
pred.dataRKP <- mutate(pred.dataRKP, RKP = predict(modelRKP, pred.dataRKP))

pred.dataKD <- expand.grid(Mediaanitulot = c(min(uusioulu$Mediaanitulot), max(uusioulu$Mediaanitulot)), Vaalipiiri = c("Oulu", "Uusimaa"))
pred.dataKD <- mutate(pred.dataKD, KD = predict(modelKD, pred.dataKD))



SDPplot <- ggplot(pred.dataSDP, aes(x = Mediaanitulot, y = SDP, colour = Vaalipiiri)) + 
  geom_line() + geom_point(data = uusioulu) + 
  xlab("Median Income") + ylab("SDP Support")
PSplot<-ggplot(pred.dataPS, aes(x = Mediaanitulot, y = PS, colour = Vaalipiiri)) + 
  geom_line() + geom_point(data = uusioulu) + 
  xlab("Median Income") + ylab("Support")
KOKplot<-ggplot(pred.dataKOK, aes(x = Mediaanitulot, y = KOK, colour = Vaalipiiri)) + 
  geom_line() + geom_point(data = uusioulu) + 
  xlab("Median Income") + ylab("Support")
KESKplot<-ggplot(pred.dataKESK, aes(x = Mediaanitulot, y = KESK, colour = Vaalipiiri)) + 
  geom_line() + geom_point(data = uusioulu) + 
  xlab("Median Income") + ylab("Support")
VIHRplot<-ggplot(pred.dataVIHR, aes(x = Mediaanitulot, y = VIHR, colour = Vaalipiiri)) + 
  geom_line() + geom_point(data = uusioulu) + 
  xlab("Median Income") + ylab("Support")
VASplot<-ggplot(pred.dataVAS, aes(x = Mediaanitulot, y = VAS, colour = Vaalipiiri)) + 
  geom_line() + geom_point(data = uusioulu) + 
  xlab("Median Income") + ylab("Support")
RKPplot<-ggplot(pred.dataRKP, aes(x = Mediaanitulot, y = RKP, colour = Vaalipiiri)) + 
  geom_line() + geom_point(data = uusioulu) + 
  xlab("Median Income") + ylab("Support")
KDplot<-ggplot(pred.dataKD, aes(x = Mediaanitulot, y = KD, colour = Vaalipiiri)) + 
  geom_line() + geom_point(data = uusioulu) + 
  xlab("Median Income") + ylab("Support")

figure3 <- ggarrange(SDPplot, PSplot, KOKplot, KESKplot, VIHRplot, VASplot, RKPplot, KDplot,
                      labels = c("SDP","PS","KOK","KESK", "VIHR", "VAS","RKP","KD" ),
                      common.legend = TRUE, legend = "bottom",
                      ncol = 4, nrow = 2)
figure3
