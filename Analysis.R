setwd("C:/Users/12063/Desktop/EPI 514/Datasets")
dat <- read.csv("brfssClean.csv", header = TRUE)
raw <- read.csv("brfssRaw.csv")
library(survey)
library(tidyverse)
library(epiR)


# Label NA's for educa
dat$educa[dat$educa==" "] <- NA

#### create categorical variable for education ####
dat$educateCat[dat$educa=="Grade 1-8"] <- "Less than Secondary Education"
dat$educateCat[dat$educa=="Grade 9-11"] <- "Less than Secondary Education"
dat$educateCat[dat$educa=="HS Grad"] <- "Secondary Education or Above"
dat$educateCat[dat$educa=="Some College"] <- "Secondary Education or Above"
dat$educateCat[dat$educa=="College Grad"] <- "Secondary Education or Above"

table(dat$educateCat)
sum(is.na(dat$educateCat))

### Remove weird top row ##########
dat <- dat %>% slice(-c(1))

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

############ education ###########
           
prop.table(svytable(~educateCat, design = brfssdsgn))*100
           tb4 <- svytable(~educateCat+job, design = brfssdsgn)
            print(tb4/rowSums(tb4), digits=3)*100
           
 # print to manually check proportions in education
            print(tb4)

           
####### health insurance #######
prop.table(svytable(~hlthpln1, design =  brfssdsgn))
tb5 <- svytable(~hlthpln1+job, design =  brfssdsgn)
print(tb5/colSums(tb5), digits = 3)           


#table 2 
epi.2by2(dat=table(df$job12, df$opAny12),method = "cohort.count")
#Confounding by age 
df$over65_12 <- ifelse(df$X_age_g >=5, 1, 2 )
epi.2by2(dat=table(df$job12, df$opAny12, df$over65_12),method = "cohort.count")
#counfounding by education
df$hsComplete[df$hsComplete==0] <- 2
epi.2by2(dat=table(df$job12, df$opAny12, df$hsComplete),method = "cohort.count")
           
#confounding by race
df$race <- ifelse(df$X_imprace=="White NH", 2, 1)
df$X_imprace <- as.factor(df$X_imprace)

epi.2by2(dat=table(df$job12, df$opAny12, df$X_imprace),method = "cohort.count")
table(df$job12, df$opAny12, df$X_imprace, deparse.level=2)

