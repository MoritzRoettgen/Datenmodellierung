###Vorbereitung----
library(tidyverse)
library(nortest)
library(usdm)
library(car)
library(geoR)
library(cowplot)
library(PresenceAbsence)
source("f.cor.plot.R")
Datensatz <- read.csv2("C:/Users/mroet/Desktop/Uni/Master/Datenmodellierung/Hausarbeit/Datenmodellierung/RM_Hausarbeit_Gr_1.6_Prunus.CSV")
attach(Datensatz)


###Test auf Normalverteilungen, Varianzhomogenität und signifkate Unerschiede zwischen Mittelwerten----
##Transformation P
lillie.test(P)
p_log <- log(P+1)
lillie.test(p_log)
p_sqrt <- sqrt(P)
lillie.test(p_sqrt)
p_positiv <- P + 0.01
p_box <- bcPower(p_positiv, (boxcoxfit(p_positiv)$lambda))
lillie.test(aov(p_box~Prun.spi)$residuals)
bartlett.test(p_box~Prun.spi)
#Nach Boxcox normalverteilt und homogen, daher ANOVA
summary(aov(p_box~Prun.spi))

##Transformation K 
k_log <- log(K)
lillie.test(aov(k_log~Prun.spi)$residuals)
bartlett.test(k_log~Prun.spi)
#Nach log normalverteilt und homogen, daher ANOVA
summary(aov(k_log~Prun.spi))
##Transformation PH
ph_log <- log(PH)
lillie.test(aov(ph_log~Prun.spi)$residuals)
ph_log10 <- log10(PH)
lillie.test(aov(ph_log10~Prun.spi)$residuals)
ph_sqrt <- sqrt(PH)
lillie.test(aov(ph_sqrt~Prun.spi)$residuals)
ph_box <- bcPower(PH, (boxcoxfit(PH)$lambda))
lillie.test(aov(ph_box~Prun.spi)$residuals)
#Keine Transformationen hilfrich, daher H-Test
kruskal.test(PH~Prun.spi)
##Transformation KAK
kak_log <- log(KAK)
lillie.test(aov(kak_log~Prun.spi)$residuals)
kak_log10 <- log10(KAK)
lillie.test(aov(kak_log10~Prun.spi)$residuals)
kak_sqrt <- sqrt(KAK)
lillie.test(aov(kak_sqrt~Prun.spi)$residuals)
kak_box <- bcPower(KAK, (boxcoxfit(KAK)$lambda))
lillie.test(aov(kak_box~Prun.spi)$residuals)
#Keine Transformationen hilfrich, daher H-Test
kruskal.test(KAK~Prun.spi)
##Transformation Slope
slope_log <- log(SLOPE+1)
lillie.test(aov(slope_log~Prun.spi)$residuals)
slope_log10 <- log10(SLOPE+1)
lillie.test(aov(slope_log10~Prun.spi)$residuals)
slope_sqrt <- sqrt(SLOPE)
lillie.test(aov(slope_sqrt~Prun.spi)$residuals)
slope_box <- bcPower(SLOPE+0.01, (boxcoxfit(SLOPE+0.01)$lambda))
lillie.test(aov(slope_box~Prun.spi)$residuals)
#Keine Transformationen hilfrich, daher H-Test
kruskal.test(SLOPE~Prun.spi)

cor.plot(Datensatz)

#####
aggregate(Datensatz[,c(2:6)],list(PA = Datensatz$Prun.spi),mean)
aggregate(Datensatz[,c(2:6)],list(PA = Datensatz$Prun.spi),var)



names(Datensatz$Prun.spi) <- c(paste("Category 1\n n=" , length(Datensatz$Prun.spi == 0) , sep=""), paste("Category 2\n n=" , length(Datensatz$Prun.spi ==1) , sep=""))


###Boxplots
plotP <- Datensatz%>%
  select(Prun.spi, P)%>%
  ggplot()+
  geom_boxplot(aes(x = factor(Prun.spi), y = P, group = Prun.spi, fill = as.character(Prun.spi)))+
  ylab(expression('Phosphor im Boden'*'[kg/ha'^-{1}*']'))+
  scale_x_discrete(breaks=c("0","1"),labels=c("Abwesend", "Anwesend"))+
  scale_fill_brewer(name = "Prun.spi", palette = 'Greys')+
  theme(axis.title.x = element_blank(),
        legend.position = 'none',
        axis.line.y = element_line(linetype = 'solid'),
        axis.line.x = element_line(linetype = 'solid'),
        panel.background = element_blank())


plotK <- Datensatz%>%
  select(Prun.spi, K)%>%
  ggplot()+
  geom_boxplot(aes(x = factor(Prun.spi), y = K, group = Prun.spi, fill = as.character(Prun.spi)))+
  ylab(expression('Kalium im Boden'*'[kg/ha'^-{1}*']'))+
  ggtitle('Prunus spinosa')+
  scale_x_discrete(breaks=c("0","1"),labels=c("Abwesend", "Anwesend"))+
  scale_fill_brewer(name = "Prun.spi", palette = 'Greys')+
  theme(axis.title.x = element_blank(),
        legend.position = 'none',
        axis.line.y = element_line(linetype = 'solid'),
        axis.line.x = element_line(linetype = 'solid'),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

plotPH <- Datensatz%>%
  select(Prun.spi, PH)%>%
  ggplot()+
  geom_boxplot(aes(x = factor(Prun.spi), y = PH, group = Prun.spi, fill = as.character(Prun.spi)))+
  ylab('pH-Wert des Bodens[PH]')+
  scale_x_discrete(breaks=c("0","1"),labels=c("Abwesend", "Anwesend"))+
  scale_fill_brewer(name = "Prun.spi", palette = 'Greys')+
  theme(axis.title.x = element_blank(),
        legend.position = 'none',
        axis.line.y = element_line(linetype = 'solid'),
        axis.line.x = element_line(linetype = 'solid'),
        panel.background = element_blank())

plotKAK <- Datensatz%>%
  select(Prun.spi, KAK)%>%
  ggplot()+
  geom_boxplot(aes(x = factor(Prun.spi), y = KAK, group = Prun.spi, fill = as.character(Prun.spi)))+
  ylab(expression('Kationenaustauschkapazität des Bodens [cmol'[c]* '/kg'^-{1}*']'))+
  scale_x_discrete(breaks=c("0","1"),labels=c("Abwesend", "Anwesend"))+
  scale_fill_brewer(name = "Prun.spi", palette = 'Greys')+
  theme(axis.title.x = element_blank(),
        legend.position = 'none',
        axis.line.y = element_line(linetype = 'solid'),
        axis.line.x = element_line(linetype = 'solid'),
        panel.background = element_blank())

plotSLOPE <- Datensatz%>%
  select(Prun.spi, SLOPE)%>%
  ggplot()+
  geom_boxplot(aes(x = factor(Prun.spi), y = SLOPE, group = Prun.spi, fill = as.character(Prun.spi)))+
  ylab('Hangneigung[°]')+
  scale_x_discrete(breaks=c("0","1"),labels=c("Abwesend", "Anwesend"))+
  scale_fill_brewer(name = "Prun.spi", palette = 'Greys', labels = c('Abwesend','Anwesend'))+
  guides(fill = guide_legend(title = "Legende", levels = c('Abwesend','Anwesend')))+
  theme(axis.title.x = element_blank(),
        axis.line.y = element_line(linetype = 'solid'),
        axis.line.x = element_line(linetype = 'solid'),
        panel.background = element_blank())


plot_grid(plotP,plotK,plotPH,plotKAK,plotSLOPE,  ncol = 3, nrow = 2, rel_widths = 0.75)

###Aufgabe 2 ----
detach("package:car", unload = TRUE)
vif(Datensatz[,-c(1)])
#Alle Werte liegen zwischen 1 und 1.9 daher liegt keine Multikollinearität vor
library(car)

par(mfrow=c(2,3))
spineplot(P,as.factor(Prun.spi))
spineplot(K,as.factor(Prun.spi))
spineplot(PH,as.factor(Prun.spi))
spineplot(KAK,as.factor(Prun.spi))
spineplot(SLOPE,as.factor(Prun.spi))
par(mfrow=c(1,1))

m.lr.0 <- glm(Prun.spi~1, family = 'binomial')

m.lr.P <- glm(Prun.spi~P, family = 'binomial')
summary(m.lr.P)
m.lr.P2 <- glm(Prun.spi~P+I(P^2), family = 'binomial')
anova(m.lr.0,m.lr.P, test = 'Chisq')
anova(m.lr.P,m.lr.P2, test = 'Chisq')

m.lr.K <- glm(Prun.spi~K, family = 'binomial')
summary(m.lr.K)
m.lr.K2 <- glm(Prun.spi~K+I(K^2), family = 'binomial')
anova(m.lr.0,m.lr.K, test = 'Chisq')
anova(m.lr.K,m.lr.K2, test = 'Chisq') #Keine Verbesserung dur Quadratischen ter,

m.lr.PH <- glm(Prun.spi~PH, family = 'binomial')
summary(m.lr.PH)
m.lr.PH2 <- glm(Prun.spi~PH+I(PH^2), family = 'binomial')
anova(m.lr.0,m.lr.PH, test = 'Chisq')
anova(m.lr.PH, m.lr.PH2, test = 'Chisq') # Keine Verbesserung, unimodales Modell wird behalten

m.lr.KAK <- glm(Prun.spi~KAK, family = 'binomial')
summary(m.lr.KAK)
m.lr.KAK2<-glm(Prun.spi~KAK+I(KAK^2), family = 'binomial')
anova(m.lr.0,m.lr.KAK, test = 'Chisq')
anova(m.lr.KAK,m.lr.KAK2, test = 'Chisq')

m.lr.SLOPE <- glm(Prun.spi~SLOPE, family = 'binomial')
summary(m.lr.SLOPE)
m.lr.SLOPE2 <- glm(Prun.spi~SLOPE+I(SLOPE^2), family = 'binomial')
anova(m.lr.0,m.lr.SLOPE, test = 'Chisq')
anova(m.lr.SLOPE,m.lr.SLOPE2, test = 'Chisq')# Keine Verbesserung


#PH hat mit einer Likliehood Ratio von 12.124 den höchsten Einfluss auf Prunus spinosa
#Der Zusammenhang ist hoch signifikant mit postivem Einfluss
#SLOPE hat mit einer LR von 8.0999 ebenfalls einen Einfluss auf das Vrokommen von Prunus
#Der Zusammenhang ist höchst Signifikant mit postitvem Einfluss


#Visualisierung
cf<-coefficients(m.lr.PH) #Koeffizienten des Modells extrahieren
plot(PH,as.character(Prun.spi),xlab="pH",ylab="Prunus spinos",pch=20) #Streudiagramm mit 0/1-Werten
x<-seq(min(PH),max(PH),0.01) #Werte für x-Achse erzeugen
y<-1/(1+exp(-(cf[1]+cf[2]*x))) #Einsetzen der Koef. in die log. Gleichung
lines(x,y) #Plotten der gefitteten Kurve

cf<-coefficients(m.lr.SLOPE) #Koeffizienten des Modells extrahieren
plot(SLOPE,as.character(Prun.spi),xlab="Hangneigung [°]",ylab="Prunus Spinosa",pch=20) #Streudiagramm mit 0/1-Werten
x<-seq(min(SLOPE),max(SLOPE),0.01) #Werte für x-Achse erzeugen
y<-1/(1+exp(-(cf[1]+cf[2]*x))) #Einsetzen der Koef. in die log. Gleichung
lines(x,y) #Plotten der gefitteten Kurve

#Multiples modell erstellen
library(memisc)
m.lr.PH.SLOPE <- update(m.lr.PH,~.+SLOPE)
anova(m.lr.PH, m.lr.PH.SLOPE, test = 'Chisq')
mtable(m.lr.PH.SLOPE,summary.stats = c('Nagelkerke R-sq.','p','Likelihood-ratio','Deviance','AIC','BIC','N'))

m.lr.PH.P <- update(m.lr.PH,~.+P)
anova(m.lr.PH, m.lr.PH.P, test = 'Chisq')
mtable(m.lr.PH.P,summary.stats = c('Nagelkerke R-sq.','p','Likelihood-ratio','Deviance','AIC','BIC','N'))



#Erweitern durch andere Parameter
m.lr.PH.SLOPE.P <- update(m.lr.PH.SLOPE,~.+P)
anova(m.lr.PH.SLOPE.P,m.lr.PH.SLOPE, test = 'Chisq')# Keine signifikante Modellverbesserung durch P
mtable(m.lr.PH.SLOPE, m.lr.PH.SLOPE.P,summary.stats = c('Nagelkerke R-sq.','p','Likelihood-ratio','Deviance','AIC','BIC','N') )
m.lr.PH.SLOPE.K <- update(m.lr.PH.SLOPE,~.+K)
anova(m.lr.PH.SLOPE.K,m.lr.PH.SLOPE, test = 'Chisq')# Signifikante Modellverbesserung durch K
mtable(m.lr.PH.SLOPE, m.lr.PH.SLOPE.K,summary.stats = c('Nagelkerke R-sq.','p','Likelihood-ratio','Deviance','AIC','BIC','N') )
m.lr.PH.SLOPE.KAK <- update(m.lr.PH.SLOPE,~.+KAK)
anova(m.lr.PH.SLOPE.KAK,m.lr.PH.SLOPE, test = 'Chisq')# Keine signifikante Modellverbesserung durch KAK
mtable(m.lr.PH.SLOPE, m.lr.PH.SLOPE.KAK,summary.stats = c('Nagelkerke R-sq.','p','Likelihood-ratio','Deviance','AIC','BIC','N') )


m.lr.PH.SLOPE.K.P <- update(m.lr.PH.SLOPE.K,~.+P)
anova(m.lr.PH.SLOPE.K,m.lr.PH.SLOPE.K.P, test = 'Chisq')# Keine signifikante Modellverbesserung durch P
mtable(m.lr.PH.SLOPE.K, m.lr.PH.SLOPE.K.P,summary.stats = c('Nagelkerke R-sq.','p','Likelihood-ratio','Deviance','AIC','BIC','N') )

m.lr.PH.SLOPE.K.KAK <- update(m.lr.PH.SLOPE.K,~.+KAK)
anova(m.lr.PH.SLOPE.K,m.lr.PH.SLOPE.K.KAK, test = 'Chisq')# Keine signifikante Modellverbesserung durch P
mtable(m.lr.PH.SLOPE.K, m.lr.PH.SLOPE.K.KAK,summary.stats = c('Nagelkerke R-sq.','p','Likelihood-ratio','Deviance','AIC','BIC','N') )


#Automatiesierte Methode
m.full<-glm(Prun.spi~P+K+KAK+PH+SLOPE,family=binomial)
m.step.bw<-step(m.full,k=2)
summary(m.step.bw)
#Die Automatisierte Methode kommt auf das selbe Ergebnis wie die manuelle Methode
#Das minimal adäquate Modell besteht aus den Parametern PH, Slope und K



#Visualisierung des minimal adäquaten Modells
library(lattice)
x<-do.breaks(c(min(PH),max(PH)),12) #x-Achsenwerte (ph) erzeugen
y<-do.breaks(c(min(SLOPE),max(SLOPE)),12) #y-Achsenwerte (hoehe) erzeugen
g<-expand.grid(PH=x,SLOPE=y) #Grid erzeugen mit Wertekombinationen aus x und y
g$z<-predict(m.lr.PH.SLOPE,type="response",newdata = g) #z- Werte (Vorhersagen) errechnen
#wireframe-Grafik erzeugen
#screen steuert Betrachtungswinkel der 3D-Grafik
wireframe(z~PH+SLOPE,scales=list(arrows=FALSE),screen = list(z = -30, x = -60),
          xlab="pH",ylab="Hangneigung",zlab=list("Prunus spinosa",rot=90),data=g)

x<-do.breaks(c(min(K),max(K)),12) #x-Achsenwerte (ph) erzeugen
y<-do.breaks(c(min(PH),max(PH)),12) #y-Achsenwerte (hoehe) erzeugen
g<-expand.grid(K=x,PH=y) #Grid erzeugen mit Wertekombinationen aus x und y
g$z<-predict(m.lr.PH.K,type="response",newdata = g) #z- Werte (Vorhersagen) errechnen
#wireframe-Grafik erzeugen
#screen steuert Betrachtungswinkel der 3D-Grafik
wireframe(z~K+PH,scales=list(arrows=FALSE),screen = list(z = -30, x = -60),
          xlab="K",ylab="pH",zlab=list("Prunus spinosa",rot=90),data=g)


x<-do.breaks(c(min(K),max(K)),12) #x-Achsenwerte (ph) erzeugen
y<-do.breaks(c(min(SLOPE),max(SLOPE)),12) #y-Achsenwerte (hoehe) erzeugen
g<-expand.grid(K=x,SLOPE=y) #Grid erzeugen mit Wertekombinationen aus x und y
g$z<-predict(m.lr.K.SLOPE,type="response",newdata = g) #z- Werte (Vorhersagen) errechnen
#wireframe-Grafik erzeugen
#screen steuert Betrachtungswinkel der 3D-Grafik
wireframe(z~K+SLOPE,scales=list(arrows=FALSE),screen = list(z = -30, x = -60),
          xlab="K",ylab="Hangneigung",zlab=list("Prunus spinosa",rot=90),data=g)



###Pkrit Aufgabe 4-----

prun.pH<-glm(Prun.spi~PH,family='binomial')
x<-seq(min(PH),max(PH),0.01)
y<-predict(prun.pH, newdata = list(PH=x),type="response")
plot(y~x,xlab="ph",ylab="p (Prunus spinosa)",type="l")
abline(0.2,0,lty=2)


prun.lr.m<-glm(Prun.spi~PH+SLOPE+K,family='binomial')
prun.pred<-prun.lr.m$fitted.values
prun.pred.0.5<-as.factor(as.numeric(prun.pred>0.5))

table(prun.pred.0.5,Prun.spi)
prun.eval<-data.frame(rownames(Datensatz),Prun.spi,prun.pred)
cmx(prun.eval)
presence.absence.accuracy(prun.eval)

error.threshold.plot(prun.eval,opt.thresholds=TRUE,vert.lines=TRUE,
                     opt.methods=c(1,2,4,5,7),line.type=c(2,2,1,1),col=c("black","blue","red","green"),
                     main="pkrit-Optimierung",ylab="Modellgüte",xlab="Schwellenwert pkrit")

auc.roc.plot(prun.eval,opt.thresholds = TRUE,opt.methods=c(1,2,4,5))
