names(Cars93)

str(Cars93)

summary(Cars93$Price)

summary(Cars93$MPG.city)

summary(Cars93$MPG.highway)

summary(Cars93$Type)

summary(Cars93$AirBags)

summary(Cars93$Man.trans.avail)

summary(Cars93)

fivenum(Cars93$Price)

fivenum(Cars93$MPG.city)

fivenum(Cars93$MPG.highway)

library(Hmisc)

describe(Cars93)


n_cars93<-Cars93[,c(6,8,9)]
c_cars93<-Cars93[,c(3,9,16)]

apply(n_cars93,2,mean)

library(e1071)

apply(n_cars93,2,skewness)

skewness<-function(x){
   m3<-sum((x-mean(x))^3)/length(x)
   s3<-sqrt(var(x))^3
   m3/s3 }

apply(n_cars93,2,skewness)

boxplot(Cars93$Min.Price,Cars93$MPG.city,Cars93$MPG.highway)


library(ggplot2)
library(gridExtra)

ggplot(Cars93,aes(Cars93$Price,Cars93$MPG.city))+geom_point(aes(colour=(Cars93$Type)))+
        geom_smooth()


ggplot(Cars93,aes(Cars93$Price,Cars93$MPG.highway))+geom_point(aes(colour=(Cars93$Type)))+
      geom_smooth()
##################################

pairs(n_cars93,main="Correlation Plot", col="blue")

#####################################

mean(Cars93$Price)

median(Cars93$Price)

sd(Cars93$Price)

var(Cars93$Price)

skewness(Cars93$Price)

ggplot(data=Cars93, aes(Cars93$Price)) + geom_density(fill="blue")

pnorm(35,mean(Cars93$MPG.highway),sd(Cars93$MPG.highway),lower.tail = F)


pbinom(1,93,prob = 0.1)

ppois(250,200,lower.tail = F)

##########################################

library(fitdistrplus)

x<-fitdistr(Cars93$MPG.highway,densfun = "t")

x$estimate

x$sd

x$vcov

x$loglik

x$n

x<-fitdistr(Cars93$MPG.highway,densfun = "normal")

x$estimate

x$sd

x$vcov

x$loglik

x$n

qqnorm(Cars93$MPG.highway)

qqline(Cars93$MPG.highway)

table(Cars93$Type)

freq<-table(Cars93$Type)

rel.freq<-freq/nrow(Cars93)*100

options(digits = 2)

rel.freq

cbind(freq,rel.freq)

barplot(freq, main = "Distribution of Categorical Variable")

range(Cars93$Fuel.tank.capacity)

cat<-seq(9.2,27.0,by = 4)
cat

options(digits = 2)

t<-cut(Cars93$Fuel.tank.capacity,cat)

as.data.frame(cbind(table(t)))
#########################################

table(Cars93$Type)

table(Cars93$AirBags)

contTable<-table(Cars93$Type,Cars93$AirBags)

contTable

prop.table(contTable)

prop.table(contTable,1)

prop.table(contTable,2)

summary(contTable)

contTable<-table(Cars93$Type,Cars93$AirBags,Cars93$Origin)
contTable

summary(contTable)


library(nortest)

ad.test(Cars93$Price) # Anderson-Darling test

cvm.test(Cars93$Price) # Cramer-von Mises test

lillie.test(Cars93$Price) # Lilliefors (KS) test

pearson.test(Cars93$Price) # Pearson chi-square

sf.test(Cars93$Price) # Shapiro-Francia test


library(corrplot)

o<-cor(Cars93[,c("Horsepower","Length")])

corrplot(o,method = "circle",main="Correlation Plot")

t<-cor(Cars93[,c("Price","MPG.city","RPM","Rev.per.mile","Width","Weight",
      "Horsepower","Length")])

corrplot(t,method = "ellipse")

##################################################


#Null Hypothesis: mean = 35
#Alternative hypothesis= mean > 35

mu<-mean(Cars93$MPG.highway)
mu

sigma<-sd(Cars93$MPG.highway)
sigma

n<-length(Cars93$MPG.highway)
n

xbar= 35

z<-(xbar-mu)/(sigma/sqrt(n))
z

#computing the critical value at 5% alpha level
alpha = .05

z1 = qnorm(1-alpha)
z1

ifelse(z > z1,"Reject the Null Hypothesis","Accept the Null Hypothesis")


#Null Hypothesis: mean = 35
#Alternative hypothesis= mean < 35

mu<-mean(Cars93$MPG.highway)
mu

sigma<-sd(Cars93$MPG.highway)
sigma

n<-length(Cars93$MPG.highway)
n

z<-(xbar-mu)/(sigma/sqrt(n))
z

#computing the critical value at 5% alpha level
alpha = .05

z1 = qnorm(1-alpha/2)

c(-z1,z1)

ifelse(z > z1 | z < -z1,"Reject the Null Hypothesis","Accept the Null Hypothesis")

mileage<-subset(Cars93,Cars93$RPM > 5000)

table(mileage$Origin)

p1<-17/57

p0<- 0.4

n <- length(mileage)

z <- (p1-p0)/sqrt(p0*(1-p0)/n)
z

#computing the critical value at 5% alpha level
alpha = .05
z1 = qnorm(1-alpha)
z1

ifelse(z > z1,"Reject the Null Hypothesis","Accept the Null Hypothesis")

mileage<-subset(Cars93,Cars93$RPM > 5000)
table(mileage$Origin)

p1<-17/57

p0<- 0.4

n <- length(mileage)
z <- (p1-p0)/sqrt(p0*(1-p0)/n)
z

#computing the critical value at 5% alpha level
alpha = .05

z1 = qnorm(1-alpha/2)
c(-z1,z1)

ifelse(z > z1 | z < -z1,"Reject the Null Hypothesis","Accept the Null Hypothesis")

t.test(Cars93$Min.Price, Cars93$Max.Price, paired = T)

t.test(Cars93$MPG.city, Cars93$MPG.highway, paired = F)  

t.test(Cars93$MPG.city~Cars93$Man.trans.avail, data=Cars93)  


shapiro.test(Cars93$MPG.city)

hist(Cars93$MPG.city,breaks=25)

qqnorm(Cars93$MPG.city,pch="*")
qqline(Cars93$MPG.city)


var.test(Cars93$MPG.highway~Cars93$Man.trans.avail, data=Cars93)

bartlett.test(Cars93$MPG.highway~Cars93$Man.trans.avail, data=Cars93)


aov(Cars93$RPM~Cars93$Cylinders)

summary(aov(Cars93$RPM~Cars93$Cylinders))

TukeyHSD(aov(Cars93$RPM~Cars93$Cylinders))

aov(Cars93$RPM~Cars93$Origin + Cars93$AirBags)

TukeyHSD(aov(Cars93$RPM~Cars93$Origin + Cars93$AirBags))
############################################################

wilcox.test(Cars93$MPG.city~Cars93$Man.trans.avail, correct = F)

wilcox.test(Cars93$MPG.city, Cars93$MPG.highway, paired = T)

wilcox.test(Cars93$MPG.city~Cars93$Man.trans.avail, data=Cars93)

kruskal.test(Cars93$MPG.city~Cars93$Cylinders, data= Cars93)





