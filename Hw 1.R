#Importing Homework1_Bonds.csv :D
Hw<-read.csv('Homework1_Bonds.csv') 

#Number of bonds approved by voters
sum(Hw$Result == "Carried")

#Number of bonds defeated by voters
sum(Hw$Result == "Defeated")

#Proportional table for the results of the 4 different types of government
prop.table(table(Hw$Type,Hw$Result),1)

#Creation of Vote Total Variable
Hw$Votes_Total <- (Hw$VotesFor + Hw$VotesAgainst)

#Finding highest voter turnout
Hw[Hw$Votes_Total== max(Hw$Votes_Total),]

#Creating a subset that contains bonds that are approved and have at least 100 total votes
Passed_Bonds <- Hw[Hw$Result == 'Carried' & Hw$Votes_Total >= 100,]

# Creating new variable: percentage of For votes for Approved Bonds
Passed_Bonds$percentage_for <- (Passed_Bonds$VotesFor/Passed_Bonds$Votes_Total)*100
       
#Box plot for the new variable of For votes for Approved Bonds
boxplot(Passed_Bonds$percentage_for, horizontal = TRUE, pch = 20, col = 'lightyellow', xlab = 'percentages of votes ', main = "Box plot of votes in favor of approved bonds")

#Summary & IQR of the new variable of For votes for Approved Bonds
summary(Passed_Bonds$percentage_for)
IQR(Passed_Bonds$percentage_for)

#Creating a graph to display the relationship between margin of approved bonds and their cost
library(scales)
plot(Passed_Bonds$percentage_for,Passed_Bonds$Amount, pch=20, xlab = 'margin a bond was approved in %', ylab = 'cost ',col = alpha('gold',0.5), main = "Cost compared to margin of approved bonds")

#Line that best fits to show relation between cost and % of votes
abline(lm(Passed_Bonds$percentage_for~Passed_Bonds$Amount),col='hotpink', lwd = 3) 
