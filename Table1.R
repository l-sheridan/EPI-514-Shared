setwd("C:/Users/fangc/OneDrive - UW/Desktop/EPI 514")
df <- read.csv("brfssClean.csv", header = TRUE)
raw <- read.csv("brfssRaw.csv")
library(survey)
library(tidyverse)
#count number of nas in job category
sum(is.na(df$job))
#survey design
options(survey.lonely.psu = "adjust") 
brfssdsgn <- svydesign(data=df, id=~1, strata=~X_ststr, weight=~X_llcpwt)
#age groups
prop.table(svytable(~X_age_g, design =  brfssdsgn))
tb1 <- svytable(~X_age_g+job, design =  brfssdsgn)
print(tb1/colSums(tb1), digits = 3)
#race
prop.table(svytable(~X_imprace, design =  brfssdsgn)
tb2 <- svytable(~X_imprace+job, design =  brfssdsgn)
print(tb2/colSums(tb2), digits = 3)
#opioid use
prop.table(svytable(~op_any, design =  brfssdsgn))
tb3 <- svytable(~op_any+job, design =  brfssdsgn)
print(tb3/colSums(tb3), digits = 3)
#work related injuries
prop.table(svytable(~emplinjm, design =  brfssdsgn))
tb3 <- svytable(~emplinjm+job, design =  brfssdsgn)
print(tb3/colSums(tb3), digits = 3)



