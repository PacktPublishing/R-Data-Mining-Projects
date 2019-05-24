## create a basket format
data <- paste(
   "Bread, Butter, Jam",
   "Bread, Butter",
   "Bread",
   "Butter, Banana",sep="\n")
cat(data)

write(data, file = "basket_format")

library(arules)

tr <- read.transactions("basket_format", format = "basket", sep=",")

inspect(tr)


## create single format
data <- paste(
   "trans1 Bread",
   "trans2 Bread",
   "trans2 Butter",
   "trans3 Jam",
   sep ="\n")
cat(data)

write(data, file = "single_format")

tr <- read.transactions("single_format", format = "single", cols =c(1,2))
inspect(tr)

#######################################################################


library(arules)
library(arulesViz)

data(Groceries) #directly reading from library

#reading from local computer
Groceries<-read.transactions("C:\\Users\\sandhyao\\Desktop\\R Data Mining Projects\\Section_5\\groceries.csv",
                             sep=",")

summary(Groceries)
inspect(Groceries[1:3])

cbind(itemFrequency(Groceries[,1:10])*100)

itemFrequencyPlot(Groceries, support=0.01, main="Relative ItemFreq Plot",
                  type="absolute")

itemFrequencyPlot(Groceries,topN=50,type="relative",main="Relative Freq Plot")

# Get the association rules based on apriori algo
rules <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.10))
summary(rules)

inspect(rules[1:8])

rules <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.10,
                                             minlen=2))
summary(rules)


support<-seq(0.01,0.1,0.01)
support

rules_count<-c(435,128,46,26,14, 10, 10,8,8,8)
rules_count


plot(support,rules_count,type = "l",main="Number of rules at different
        support %", col="darkred",lwd=3)

conf<-seq(0.10,1.0,0.10)
conf

rules_count<-c(427,231,125,62,15,0,0,0,0,0)
rules_count

plot(conf,rules_count,type = "l",main="Number of rules at different
      confidence %",col="darkred",lwd=3)


#Eclat Algorithm

rules_ec <- eclat(Groceries, parameter = list(supp = 0.05))

summary(rules_ec)

#sorting out the most relevant rules
rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:5])

rules<-sort(rules, by="lift", decreasing=TRUE)
inspect(rules[1:5])


#Visualizing association rules

plot(rules,method='graph',interactive = T,shading = T)

plot(rules_ec,method='graph',interactive = T,shading = N)

#########################################################################

#Implementation of arules


rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.8),
                appearance = list(default="lhs",rhs="yogurt"),
                control = list(verbose=F))

rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.10,minlen=2),
                appearance = list(default="rhs",lhs="yogurt"),
               control = list(verbose=F))

rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

# sorting grocery rules by lift
inspect(sort(rules, by = "lift")[1:5])

# finding subsets of rules containing any berry items
berryrules <- subset(rules, items %in% c("berries","yogurt"))
inspect(berryrules[1:5])

# finding subsets of rules that precede soda purchases
sodarules <- subset(rules, rhs %ain% "whole milk")
inspect(sodarules)

top.soda.rules <- head(sort(sodarules, by = "lift"), 5)
inspect(top.soda.rules)

#writing the rules to a CSV file
write(rules, file = "groceryrules.csv", sep = ",", quote = TRUE,row.names = FALSE)

# converting the rule set to a data frame
groceryrules_df <- as(rules, "data.frame")







































