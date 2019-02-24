data <- read.csv("/Users/konstantinosbousios/Desktop/RDATA/nba.csv",  sep = ";")
attach(data)
head(data)
str(data) # elegxw ti morfhs einai ta dedomena
t(t(sapply(data,class))) # elegxw ti morfhw einai ta dedomena
var.integer<-sapply(data,class) !="factor"  #epilegw tis posotikes metablites
data1<-data[var.integer]
str(data1) # elegxw thn morfh apo tis posotikes metablites pou phrame
data2<-data1[,2:9] # epilegw tis metablhtes pou exoun nohma
data2<-data.frame(data2)
attach(data2)
str(data2)
### ANALUSH POSOTIKWN METABLHTWN ###
summary(data2) #zhtaw perigrafika metra
sd(GP, na.rm = TRUE)
sd(MPG, na.rm = TRUE)
sd(PPG, na.rm = TRUE)
sd(RPG, na.rm = TRUE)
sd(APG, na.rm = TRUE)
sd(STPG, na.rm = TRUE)
sd(BLKPG, na.rm = TRUE)
sd(salary, na.rm = TRUE)
library(moments)
skewness(data2,na.rm = TRUE) #briskw summetria gia thn kathe metablhth
kurtosis(data2,na.rm = TRUE) #briskw kurtothta gia thn kathe metablhth

par(mfrow=c(2,4)) # elegxw ws pros tin kanonikotita
for(i in 1:8){

  qqnorm(data2[,i])
  qqline(data2[,i])
  legend("topleft",legend=(names(data2[i])),cex=0.8)
}
par(mfrow=c(1,1))
for(i in 1:8) { # meta tsekarw me eleghous ws pros tin kanonikotita
  print(names(data2[i]))
  print(test_i<-shapiro.test(data2[,i]))
}
par(mfrow=c(2,4)) 
for(i in 1:8){ #  istogrammata
  hist(data2[,i])
  
  legend("top",legend=(names(data2[i])),cex=0.5)
  
}

## KATHARIZOYME TA DEDOMENA MAS ####
library(dplyr)
data2 <- filter(data2, GP > 0, MPG > 0, PPG > 0, RPG > 0, APG > 0, STPG > 0, BLKPG > 0, salary > 0)
data2<-na.omit(data2) #diagrafoyme ta kena kelia
data2<-data.frame(data2)
attach(data2)

data3<-(log10(data2))# logarithmizoume gia na eleksimou an beltiwnetai h eikona
str(data3)
attach(data3)
par(mfrow=c(2,4)) # elegxw ws pros tin kanonikotita
for(i in 1:8){
  
  qqnorm(data3[,i])
  qqline(data3[,i])
  legend("topleft",legend=(names(data3[i])),cex=0.8)
}
qqnorm(APG)
qqline(APG)

legend("bottomright",legend=(names(data3[5])),cex=0.8)
hist(APG)
legend("top",legend=(names(data3[4])),cex=0.5)
qqnorm(RPG)
qqline(RPG)

legend("bottomright",legend=(names(data3[4])),cex=0.8)
hist(RPG)
legend("top",legend=(names(data3[4])),cex=0.5)
par(mfrow=c(1,1))
for(i in 1:8) { # meta tsekarw me eleghous ws pros tin kanonikotita
  print(names(data3[i]))
  print(test_i<-shapiro.test(data3[,i]))
}
par(mfrow=c(2,4)) 
for(i in 1:8){ #  istogrammata
  hist(data3[,i])
  legend("top",legend=(names(data3[i])),cex=0.5)
}
summary(data3)


data_new<-cbind(data2,data3$RPG,data3$APG)#ftiaxnw to teliko dataset me logarithimsenes to RPG KAI APG
data_final<-data.frame(GP, MPG,PPG,STPG,BLKPG,salary,RPG,APG)
data_final<-na.omit(data_final)
str(data_final)
attach(data_final)
pairs(data_final, upper.panel=NULL, col='blue', pch=16, cex=2) 

# EKSETAZW TIS METABLHTES ANA DYO 
boxplot(data_final$GP~data_final$salary)
boxplot(data_final$salary~data_final$MPG)
boxplot(data_final$salary~data_final$PPG)
boxplot(data_final$salary~data_final$STPG)
boxplot(data_final$salary~data_final$BLKPG)
boxplot(data_final$salary~data_final$RPG)
boxplot(data_final$salary~data_final$APG)
sxesi1<-data.frame(data_final$salary,data_final) #elegxw thn susxetish
library(sjPlot)
sjt.corr(sxesi1, corr.method = "pearson")
round(cor(data_final$salary,data_final$GP),3)
round(cor(data_final$salary,data_final$MPG),3)
round(cor(data_final$salary,data_final$PPG),3)
round(cor(data_final$salary,data_final$STPG),3)
round(cor(data_final$salary,data_final$BLKPG),3)
round(cor(data_final$salary,data_final$RPG),3)
round(cor(data_final$salary,data_final$APG),3)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}
pairs(data_final, upper.panel = panel.cor)

## EKSETAZOYME TIS POIOTIKES METABLHTES ###
# KANW DIAGRAMMATA PITES
perc <- round(100*prop.table(table(data$position)),1)
perc <- paste( '(', perc, sep='' )
perc <- paste( perc, '%)', sep='' )
pielabels<-paste(levels(data$position),perc)
pie(table(data$position),labels=pielabels,col=1:2,border=0)
library(plotrix)
pie3D(table(data$position), labelcex=1.0, col=1:2, explode=0.1, border=0,labels=pielabels)
legend('topright', legend=levels(data$position), bty='n', fill=1:2)


library(gplots) #ERROR BAR GIA TIS POIOTIKES METABLHTES
plotmeans(salary~position, data=data, connect=F)
library(car)#BOXPLOT GIA TIS POIOTIKES METABLHTES
Boxplot(data$salary~data$position, horizontal=F, cex.axis=0.55, las=1, 
        labels=as.character(data$position))
# ERWTHMA 1 
data_erwthma1<-data.frame(data$salary,data$position)
data_erwthma1<-na.omit(data_erwthma1) #diagrafoyme ta kena kelia
colnames(data_erwthma1)<-c("salary","position")
attach(data_erwthma1)
shapiro.test(aov(salary~position,data = data)$res) #elegxos kanonikothtas 
mean(data_erwthma1$salary)
median(data_erwthma1$salary)
hist(data_erwthma1$salary)# elegxo kanonikothta grafika
qqnorm(data_erwthma1$salary, main='salary ')
qqline(data_erwthma1$salary)
library(car)
leveneTest(salary~position,data=data_erwthma1) # omoskedastikothta
summary(aov(salary~position,data = data_erwthma1)) #pinakas analusis diakumanshs
pairwise.t.test(salary,position, data=data_erwthma1) #kanoume elegxo an duo
library(gplots)
plotmeans(salary~position, data = data_erwthma1, connect = F) #error bars
anova1<-aov(data_erwthma1$salary~data_erwthma1$position)
shapiro.test(anova1$residuals) #elegxos kanonikothtas
kruskal.test(salary~position, data=data_erwthma1) #telika pairnw elegxo gia diamesous kai ton dexomaste gt p-value>0,05 thn upothesh gia ises diamesous
pairwise.wilcox.test(data_erwthma1$salary,data_erwthma1$position) #elgxos ana duo
boxplot(data_erwthma1$salary~data_erwthma1$position, las=2, cex.axis=0.7)
oneway.test(data_erwthma1$salary~data_erwthma1$position, var.equal = F)
plotmeans(data_erwthma1$salary~data_erwthma1$position, las=2, cex.axis=0.7, connect = F)
# Erwthma 2 (mesos oros an lepto)
data_withoutsalary<-subset(data, is.na(data$salary)) #dhmiourgw to uposunolo xwris to salary kai diairw me to MPG
data_withsalary<-subset(data,!is.na(data$salary)) #dhmiourgw to uposunolo me to salary kai diairw me to MPG
str(data_withoutsalary)
subset2<-subset(data, is.na(salary))
subsetdivision2<-subset2[,5:9]
data_withoutsalarydivisionMPG<-cbind(subsetdivision2/subset2$MPG)
data_withoutsalarydivisionMPG<-
  data.frame(subset2$PLAYER,subset2$GP,data_withoutsalarydivisionMPG,
             subset2$position,subset2$salary)
colnames(data_withoutsalarydivisionMPG)<-c("PLAYER","GP", "PPG",
                                        "RPG","APG","STPG","BLKPG","position","salary")
attach(data_withoutsalarydivisionMPG)
subset1<-subset(data, position=="SF" & GP>=40 & !is.na(salary))
subsetdivision1<-subset1[,5:9]
data_withsalarydivisionMPG<-cbind(subsetdivision1/subset1$MPG)
data_withsalarydivisionMPG<-
  data.frame(subset1$PLAYER,subset1$GP,data_withsalarydivisionMPG,
             subset1$position,subset1$salary)
colnames(data_withsalarydivisionMPG)<-c("PLAYER","GP", "PPG",
                        "RPG","APG","STPG","BLKPG","position","salary")
var.integer<-sapply(data_withsalarydivisionMPG,class) !="factor"  #epilegw tis posotikes metablites
data_withsalarydivisionMPG<-data_withsalarydivisionMPG[var.integer]
attach(data_withsalarydivisionMPG)
# elegxos sthn kathe metablhth meta to kainourgio dataset pou dhmiourghsame me tous periorismous
summary (data_withsalarydivisionMPG)
library(moments)
skewness(data_withsalarydivisionMPG) #briskw summetria gia thn kathe metablhth
kurtosis(data_withsalarydivisionMPG) #briskw kurtothta gia thn kathe metablhth
library(lawstat)
#symmetry.test(data_withsalarydivisionMPG) #kanw test summetrias
par(mfrow=c(2,4)) # elegxw ws pros tin kanonikotita
for(i in 1:7){
  
  qqnorm(data_withsalarydivisionMPG[,i])
  qqline(data_withsalarydivisionMPG[,i])
  legend("topleft",legend=(names(data_withsalarydivisionMPG[i])),cex=0.8)
}
par(mfrow=c(1,1))
for(i in 1:7) { # meta tsekarw me eleghous ws pros tin kanonikotita
  print(names(data_withsalarydivisionMPG[i]))
  print(test_i<-shapiro.test(data_withsalarydivisionMPG[,i]))
}
par(mfrow=c(2,4)) 
for(i in 1:7){ #  istogrammata
  hist(data_withsalarydivisionMPG[,i])
  
  legend("top",legend=(names(data_withsalarydivisionMPG[i])),cex=0.5)
  
}

pairs(data_withsalarydivisionMPG, upper.panel=NULL, col='blue', pch=16, cex=2)

#logarithmizw tis metablhtes gia na eksetasw an beltiwnetai h eikona kapoiwn metablhtwn kai proseggizoun thn kanonikothta
par(mfrow=c(1,1))
qqnorm(log10(data_withsalarydivisionMPG$APG))
qqline(log10(data_withsalarydivisionMPG$APG))
shapiro.test(log10(data_withsalarydivisionMPG$APG))
hist(log10(data_withsalarydivisionMPG$APG)) # kaluterh eikona
summary(log10(data_withsalarydivisionMPG$APG))


par(mfrow=c(1,1))
qqnorm(log10(data_withsalarydivisionMPG$RPG))
qqline(log10(data_withsalarydivisionMPG$RPG))
shapiro.test(log10(data_withsalarydivisionMPG$RPG))
hist(log10(data_withsalarydivisionMPG$RPG)) # kaluterh eikona
summary(log10(data_withsalarydivisionMPG$RPG))
par(mfrow=c(1,1))
qqnorm(log10(data_withsalarydivisionMPG$salary))
qqline(log10(data_withsalarydivisionMPG$salary))
shapiro.test(log10(data_withsalarydivisionMPG$salary))
hist(log10(data_withsalarydivisionMPG$salary)) # kaluterh eikona
summary(log10(data_withsalarydivisionMPG$salary))

data_withsalarydivisionMPG_final<-data.frame(GP,PPG,log10(RPG),log10(APG),STPG,BLKPG,log10(salary))
colnames(data_withsalarydivisionMPG_final)<-c("GP","PPG","log10RPG",
                        "log10APG","STPG","BLKPG","log10salary")
attach(data_withsalarydivisionMPG_final)

pairs(data_withsalarydivisionMPG_final
    , upper.panel=NULL, col='blue', pch=16, cex=2)
# grafw to modelo kai prospathw na brw to beltisto 
model<-lm(log10salary~.,data=data_withsalarydivisionMPG_final)
summary(model)          
step(model,direction = "both")         
n=length(data_withsalarydivisionMPG_final$log10salary)
model2<-step(model,direction = "both",k=log(n))

library(car)
vif(model)
model1<-lm(formula =log10salary~.-BLKPG,data =data_withsalarydivisionMPG_final)
summary(model1) 
step(model1,direction = "both")
n=length(data_withsalarydivisionMPG_final$log10salary)
model2<-step(model1,direction = "both",k=log(n))
vif(model1)
model3<-lm(formula=log10salary~.-GP-BLKPG,data =data_withsalarydivisionMPG_final)
summary(model3) 
step(model3,direction = "both")
n=length(data_withsalarydivisionMPG_final$log10salary)
model2<-step(model3,direction = "both")
vif(model3)

model4<-lm(formula =log10salary~.-GP-BLKPG-log10RPG,data =data_withsalarydivisionMPG_final)
summary(model4)
step(model4,direction = "both")
n=length(data_withsalarydivisionMPG_final$log10salary)
model2<-step(model4,direction = "both")
vif(model4)
model5<-lm(formula=log10salary~.-GP-BLKPG-log10RPG-log10APG,data =data_withsalarydivisionMPG_final)
summary(model5)
step(model5,direction = "both")
n=length(data_withsalarydivisionMPG_final$log10salary)
model2<-step(model5,direction = "both")
vif(model5)
# elegxw an to model 5 plhrei oles tis proupotheseis gia na to dextw
#kanonikothta
par(mfrow=c(1,3)) # kanonikotita kataloipwn & akraies times
hist(rstandard(model5),border="white",col=colors()[32])
plot(model5,which=1,col="blue")
plot(model5,which=2,col="blue")
par(mfrow=c(1,3))
library(PerformanceAnalytics)
chart.Histogram( rstandard(model5), method='add.normal',
                 ylim=c(0,0.6) )
plot(model5, which=1, cex=1.5, col='blue')
plot(model5, which=2, cex=1.5, col='blue')
shapiro.test(rstandard(model5)) #kanonikotrhta apo ton elegxo

# aneksarthsia
library(car)
durbinWatsonTest(model5,max.lag=20) #thewrw aneksartisia
dwt(model5)
par(mfrow=c(1,1))
acf(rstandard(model5),lag.max = 20)
# omoskedastikotita & akraies times
par(mfrow=c(1,1))
plot( model5$fit, rstandard(model5))
abline( h=-1.96, col='red', lwd=2, lty=2 )
abline( h=1.96, col='red', lwd=2, lty=2 )
qfits<-cut(model5$fitted.values,breaks = quantile(model5$fitted.values))
leveneTest(rstandard(model5),qfits)
#tyhaiotita
library(snpar)
runs.test(model5$residuals)
par(mfrow=c(2,2)) # Get all 4 plots on one page
plot(model5)
par(mfrow=c(1,1))
### problepsi gia tous misthous me #na twn paiktwn
#data_withoutsalarydivisionMPG<-data.frame(PLAYER,GP,PPG,log10(RPG),log10(APG),STPG,BLKPG,log10(salary))
#colnames(data_withoutsalarydivisionMPG)<-c("PLAYER","GP","PPG","log10RPG",
#                                            "log10APG","STPG","BLKPG","log10salary")
predict_values<-predict(model5, data_withoutsalarydivisionMPG, interval='prediction')

## Erwthma 3#########

# dhmiourgoume to dataset me ti proupotheseis pou xreiazontai
data4<-subset (data, position=="SF")
data4<-na.omit(data4) 
str(data4)
attach(data4)
for(i in 1:65){
  if (data4[i,4]>=20.00){
    data4[i,4] = 1
  }else{
    data4[i,4] = 0
  }
}
colnames(data4)<-c("RK","PLAYER", "GP","LOG","PPG", "RPG",
                   "APG","STPG","BLKPG","position", "salary")
attach(data4)
# ftiaxnoume blox-plot gia thn log mazi me tis alles metablhtes
par(mfrow=c(3,4))
 
 boxplot(RK~LOG)
  boxplot(GP~LOG)
  boxplot(PPG~LOG)
  boxplot(RPG~LOG)
  boxplot(APG~LOG)
  boxplot(STPG~LOG)
  boxplot(BLKPG~LOG)
  boxplot(salary~LOG)
  # grafoume to mondelo mas 
model<-glm(LOG ~GP+PPG+RPG+APG+STPG+BLKPG+salary, data = data4, family = "binomial")
summary(model)
step(model,direction = "both")
#perigrafoume thn sxesh me thn xrhsh logistic regression
z.GP<-c(rep(0,65))
z.PPG<-c(rep(0,65))
z.RPG<-c(rep(0,65))
z.APG<-c(rep(0,65))
z.STPG<-c(rep(0,65))
z.BLKPG<-c(rep(0,65))
z.salary<-c(rep(0,65))
# kratame thn thn diafora stis 2 tupikes apokliseis gia diasthma 95%
for(i in 1:65){
  z.GP[i]<-(GP[i]-mean(GP))/(sd(GP)*2)
  z.PPG[i]<-(PPG[i]-mean(PPG))/(sd(PPG)*2)
  z.RPG[i]<-(RPG[i]-mean(RPG))/(sd(RPG)*2)
  z.APG[i]<-(APG[i]-mean(APG))/(sd(APG)*2)
  z.STPG[i]<-(STPG[i]-mean(STPG))/(sd(STPG)*2)
  z.BLKPG[i]<-(BLKPG[i]-mean(BLKPG))/(sd(BLKPG)*2)
  z.salary[i]<-(salary[i]-mean(salary))/(sd(salary)*2)
}
mean(GP)
mean(PPG)
mean(RPG)
mean(APG)
mean(STPG)
mean(BLKPG)
mean(salary)
#grafoume to neo modelo kai eksetazoume mexri na katliksoume sto teliko
model1<-glm(LOG ~z.GP+z.PPG+z.RPG+z.APG+z.STPG+z.BLKPG+z.salary, data = data4, family = "binomial")
summary(model1)
invlogit <- function(x) exp(x)/(1 + exp(x))
invlogit(5.6257  +  2.0577 * 0 +10.9032  *0+  5.6866 * 0  -1.5794*0+4.4035  *0 -0.5808 *0-1.7636*0 )
invlogit(5.6257  +  2.0577 * 1 +10.9032  *0+  5.6866 * 0  -1.5794*0+4.4035  *0 -0.5808 *0-1.7636*0 )-invlogit(5.6257  +  2.0577 * 0 +10.9032  *0+  5.6866 * 0  -1.5794*0+4.4035  *0 -0.5808 *0-1.7636*0 )
logistic_model1 <- glm(LOG ~ GP, data = data4, family = binomial)
logistic_model1$deviance
logistic_model2 <- glm(LOG ~ GP+PPG, data = data4, family = binomial)
logistic_model2$deviance
logistic_model3 <- glm(LOG ~ GP+PPG+RPG, data = data4, family = binomial)
logistic_model3$deviance
logistic_model4 <- glm(LOG ~ GP+PPG+RPG+APG, data = data4, family = binomial)
logistic_model4$deviance
logistic_model5 <- glm(LOG ~ GP+PPG+RPG+APG+STPG, data = data4, family = binomial)
logistic_model5$deviance
logistic_model6 <- glm(LOG ~ GP+PPG+RPG+APG+STPG+BLKPG, data = data4, family = binomial)
logistic_model6$deviance
logistic_model7 <- glm(LOG ~ GP+PPG+RPG+APG+STPG+BLKPG+salary, data = data4, family = binomial)
logistic_model7$deviance
summary(logistic_model7)
invlogit <- function(x) exp(x)/(1 + exp(x))
invlogit(-1.533e+01  +  4.682e-02 * 0 +9.910e-01  *0+  1.846e+00 * 0  -6.973e-01*0+4.941e+00  *0 -9.845e-01 *0-1.062e-07*0 )
invlogit(-1.533e+01  +  4.682e-02 * 0 +9.910e-01  *1+  1.846e+00 * 0  -6.973e-01*0+4.941e+00  *0 -9.845e-01 *0-1.062e-07*0  )-invlogit(-1.533e+01  +  4.682e-02 * 0 +9.910e-01  *0+  1.846e+00 * 0  -6.973e-01*0+4.941e+00  *0 -9.845e-01 *0-1.062e-07*0 )
# ektimw thn prosarmogh twn dedomenwn diagrammatika
library(arm)
binnedplot(fitted(logistic_model7),
           residuals(logistic_model7, type = "response"),
           nclass = NULL, xlab = "Expected Values",
           ylab = "Average residual",
           main = "Binned residual plot",
           cex.pts = 0.8, col.pts = 1, col.int = "gray")
#kanw problepsh sumfwna me to modelo 
predict_values<-predict(logistic_model7, data4 = data, type = "response")

predict_values<-sort(predict(logistic_model7, newdata = data4, type = "response",interval='prediction'))
