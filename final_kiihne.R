#vccination rate analysis for data science in R final project
#by Avery Kiihne, December 2021
rm(list = ls())  #clears any leftover ata from other projects
setwd("/Users/AveryKiihne1/Desktop/R_code")
#install.packages("gridExtra")
library(gridExtra)
library(grid)
#load data
vacdata <- read.csv("data/vaccination.csv")
#check data 
dim(vacdata)


''' questions of interest: race, q6a (does mask stop covid), 
q12 (covid impact on mental health), vacc1 (gotten vaccine), 
recemploy (employment status), q28a (get vaccine if work provided), q35 (if trump asked, get vac)
, recage2 (age), voted2 (party indicator), q33 (rural?), religionimp (religous importance),
essential (essential worker), lgbt, flu (got flue vaccine), party3 (politics),
receduc (education level), income, recincome (ranged income), bornagain (christian)
'''
# select variables and subset them
myvars <- c("vacc1","race", "q6a","recemploy","recage2","q33","bornagain", "flu", "party3","receduc","recincome" )
maindata <- vacdata[myvars]
#change vaccination to yes or no. Don't care about number of shots.
for(i in 1:1862) {
  if (maindata$vacc1[i] =="No, have not gotten the vaccine"){
    maindata$vacc1[i] = 0#"not vaccinated"

}
  else{
      maindata$vacc1[i] =  1 #"vaccinated"
}}
#subset based on age range to take out young people and elderly
finaldata <- subset(maindata,recage2== "30-49" | recage2 =="50-64" )
#look at vaccination data
vaccinated  <- as.numeric(finaldata$vacc1)
vacplot <- c(length(vaccinated)-sum(vaccinated),sum(vaccinated))
barplot(vacplot,
        col="black",
        #ylim =  c(-2, 2), # y-axis dimensions
        border = NA, # removes bar borders
        main = "number of vaccinated to unvaccinated individuals in data", # plot title
        cex.main = 2, # size of plot title
        ylab = "# of responses", # yaxis label
        xlab = "vaccinated?",
        cex.lab = 1.5,# size of yaxis label
        names.arg =c("no","yes"),
        las = 1) # controls angle of axis labels)

#tables used to compare data
#table plot for flue shot - good indicator
dev.off(dev.list()["RStudioGD"]) #clears plot. grid will appear over plot if present
d <- head(table(finaldata$vacc1, finaldata$flu))
rownames(d) <- c("unvaccinated", "vaccinated")
colnames(d) <- c("NA", "flu shot", "no flu shot")
grid.table(d)

#rural table- bad indicator
dev.off(dev.list()["RStudioGD"])
dd <- head(table(finaldata$vacc1, finaldata$q33))
rownames(dd) <- c("unvaccinated", "vaccinated")
grid.table(dd)

#race table
dev.off(dev.list()["RStudioGD"])
dd <- head(table(finaldata$vacc1, finaldata$race))
rownames(dd) <- c("unvaccinated", "vaccinated")
grid.table(dd)
