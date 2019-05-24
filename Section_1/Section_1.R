x1<-c(2.5,1.4,6.3,4.6,9.0)
class(x1)
mode(x1)

x2<-c(TRUE,FALSE,TRUE,FALSE,FALSE)
class(x2)
mode(x2)

x3<-c("DataMining","Statistics","Analytics","Projects","MachineLearning")
class(x3)
mode(x3)

domains<-c("DataMining","Statistics","Analytics","Projects","MachineLearning",
           "DataMining","Statistics","Analytics","Projects","MachineLearning",
           "DataMining","Statistics","Analytics","Projects","MachineLearning",
           "DataMining","Statistics","Analytics","Projects","MachineLearning",
           "DataMining","Statistics","Analytics","Projects","MachineLearning")

as.factor(domains)
table(domains)

x<-data.frame(x1,x2,x3)
class(x)
print(x)

x1<-c(2.5,1.4,6.3,4.6,9.0)
class(x1)
x1<-c(2.5,1.4,6.3,4.6,9.0,"cat")
class(x1)

ls()

mylist<-list(custid=112233, custname="John R", mobile="989-101-1011",email="JohnR@gmail.com")
mylist
mylist[[2]]
mylist[2]

mylist1<-list(custid=112233, custname="John R",
              mobile="989-101-1011",email="JohnR@gmail.com")

mylist2<-list(custid=443322, custname="Frank S",
              mobile="781-101-6211",email="SFranks@hotmail.com")

mylist<-cbind(mylist1,mylist2)

mylist

domains<-c("DataMining","Statistics","Analytics","Projects","MachineLearning",
           "DataMining","Statistics","Analytics","Projects","MachineLearning",
           "DataMining","Statistics","Analytics","Projects","MachineLearning",
           "DataMining","Statistics","Analytics","Projects","MachineLearning",
           "DataMining","Statistics","Analytics","Projects","MachineLearning")
factor(domains)

seq(from=1,to=5,length=4)

seq(length=10,from=-2,by=.2)

rep(15,10)

gl(2,5,labels=c('Buy','DontBuy'))


getwd()
setwd("C://Users//sandhyao//Documents")

dt<-read.csv("C:\\Users\\sandhyao\\Desktop\\R Data Mining Projects\\Section_1\\hs0.csv")
names(dt)

data<-read.table("C:\\Users\\sandhyao\\Desktop\\R Data Mining Projects\\Section_1\\hs0.csv",
                 header= T, sep=",")
names(data)

library(xlsx)

library(xlsxjars)

dat<-read.xlsx("C:\\Users\\sandhyao\\Desktop\\R Data Mining Projects\\Section_1\\hs0.xlsx",
               "hs0")
head(dat)

library(Hmisc)

mydata <- spss.get("C:\\Users\\sandhyao\\Desktop\\R Data Mining Projects\\Section_1\\wage.sav",
                   use.value.labels=TRUE)

head(mydata)

library(sas7bdat)

mydata <- read.sas7bdat("C:\\Users\\sandhyao\\Desktop\\R Data Mining Projects\\Section_1\\sales.sas7bdat")

head(mydata)



################################################################################

is.numeric(x1)

is.character(x3)

is.vector(x1)

is.matrix(x)

is.data.frame(x)

as.numeric(x1)

as.vector(x2)

as.matrix(x)

as.data.frame(x)

as.character(x2)

as.factor(x2)

#################################################################################

ArtPiece<-read.csv("C:\\Users\\sandhyao\\Desktop\\R Data Mining Projects\\Section_1\\ArtPiece.csv")

names(ArtPiece)

attach(ArtPiece)

sort(Critic.Ratings)

sort(Critic.Ratings, decreasing = T)

i2<-ArtPiece[order(Critic.Ratings,Acq.Cost),1:5]
head(i2)

head(i2, 10)

i2<-ArtPiece[order(Border.of.art.piece, na.last = F),2:6]

head(i2)



audit <- read.csv("C:/Users/sandhyao/Desktop/R Data Mining Projects/Section_1/audit.csv")

A<-audit[1:10,c(1,2,3,7,9)]
names(A)

B<-audit[5:15,c(1,3,4,5,6)]
names(B)

head(merge(A,B),3)

head(merge(A,B, all=F),3)

head(merge(A,B, all=T),3)

head(merge(A,B, all.x=T),3)

head(merge(A,B, all.y=T),3)

head(merge(A,B,by="ID"),3)

head(merge(A,B,by=c("ID","Employment")),3)

A<-audit[,c(2,7,9)]
names(A)

B<-audit[,c(4,5,6)]
names(B)

head(cbind(A,B),3)


newdata <- audit[ which(audit$Gender=="Female" & audit$Age > 65), ]

rownames(newdata)

newdata <- subset(audit, Gender=="Female" & Age > 65,select=Employment:Income)
rownames(newdata)


################################################################################

Sys.time()

dt<-as.Date(Sys.time())

class(dt)

weekdays(as.Date(Sys.time()))

months(as.Date(Sys.time()))

quarters(as.Date(Sys.time()))

substr(as.POSIXct(as.Date(Sys.time())),1,4)

format(Sys.time(),format = "%m %d %y")



####################################################

#newFunc <- function(x){define function}

int<-seq(1:20)

int

myfunc<-function(x){x*x}

myfunc(int)


x<-100:200

y <- NULL # NULL vector as placeholder

for(i in seq(along=x)) {
  if(x[i] < 150) {
    y <- c(y, x[i] - 50)
  } else {
    y <- c(y, x[i] + 50)
  }
}

print(y)





x <- 100
repeat {
  print(x)
  x = sqrt(x)+10
  if (x > 2.6){
    break
  }
}

x <- 10
while (x < 60) {
  print(x)
  x = x+10
}

apply(ArtPiece[,2:3],2,mean)


apply(ArtPiece[,2:3],1,mean)


lapply(ArtPiece[,2:3],mean)


sapply(ArtPiece[,2:3],mean)

#head(tapply(Critic.Ratings,Acq.Cost,summary),3)

head(tapply(ArtPiece$Critic.Ratings,ArtPiece$Acq.Cost,summary),3)


x<-"data Mining is not a difficult subject, anyone can master the subject"

class(x)

substr(x, 1, 12)

sub("data mining", "The Data Mining", x, ignore.case =T, fixed=FALSE)

strsplit(x, "")

x<-c(12,13,14,21,23,24,NA,25,NA,0,NA)

is.na(x)

mean(x,na.rm=TRUE)












