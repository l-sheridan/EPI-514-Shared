##epi 514 merging and cleaning##
rm(list=ls())
library(tidyverse)
library(epiR)
setwd("/Users/keelyparis/Documents/EPI 514")

########################################################################################
##change big data set to be smaller, dont run this code if ur running the whole script##
########################################################################################
      #if u rerun it won't change  anything but it's unnecessary to do it twice 
combo <- read.csv("comboData.csv")
smaller <- combo[combo$year=="2019" | combo$year=="2020",]
write.csv(smaller, file = "comboData.csv", row.names = F)
    #replaced file name


#######################
#load smaller data set#
#######################
combo <- read.csv("comboData.csv")


###########################################################
#delete unnecessary variables by keeping variables we want#
###########################################################
variable.names(combo)
    #note: if you find a variable in the codebook that is NOT included in the subset
    #below but you want it, just add it to the subset code

brfss <- subset(combo, select = c(year, X_ststr, sex, age,
                                  X_age_g, educa, X_race, X_imprace,
                                  genhlth, X_racegr3, X_mrace1, X_prace1,
                                  hlthpln1,
                                  smokday2, X_smoker3, X_rfsmok3, X_rfdrhv7,
                                  mjever, mj30day, op_any, op_days, op_more,
                                  op_norx, op_less, occ11, ind11, 
                                  jobactiv, emplinjm,
                                  X_llcpwt, use_wt, use_comb, agestd, agestdwt))
        #only included some weighting variables, probably a good idea to double
        #check before we weight things
brfss <- brfss[brfss$year=="2019",]

      #delete big  data frame for space (after subsetting)
combo <- NA

###############################################
##explore data, tabulate exposure and outcome##
###############################################
View(brfss)

  #exposure tabulations
table(brfss$occ11, useNA="ifany")
table(brfss$ind11, useNA="ifany") 

   #outcome tabulations
table(brfss$op_any, useNA="ifany")
table(brfss$op_more, useNA="ifany")

table(brfss$X_rfdrhv7, useNA="ifany")

    #exposure tabulation by year
brfss %>% filter(year=="2019") %>% pull(occ11) %>% table()
brfss %>% filter(year=="2020") %>% pull(occ11) %>% table()

    #amount of heavy drinkers who also answered opioid module
brfss %>% filter(year=="2019") %>% filter(op_any!= "Refused" & op_any!="") %>% pull(X_rfdrhv7) %>% table()
brfss %>% filter(year=="2020") %>% filter(op_any!= "Refused"& op_any!="") %>% pull(X_rfdrhv7) %>% table()


########################################################################################
##create exposure variables (one using 3 sector model, one using manual vs non-manual)##
########################################################################################

brfss$sector[brfss$occ11=="Farming, Fishing, Forestry" | brfss$occ11== "Production"] <- "primary"
brfss$sector[brfss$occ11=="Construction & extraction" | brfss$occ11=="Install, repair, maintain" |
            brfss$occ11=="Transport & material moving"] <- "secondary"
brfss$sector[brfss$occ11=="Mgt, business/finance" | brfss$occ11=="Professional & related" |
            brfss$occ11=="Sales & related"| brfss$occ11=="Office & admin" |
            brfss$occ11=="Service"] <- "tertiary"
  
  

brfss$job[brfss$occ11=="Farming, Fishing, Forestry" | brfss$occ11== "Production" |
               brfss$occ11=="Construction & extraction" | brfss$occ11=="Install, repair, maintain"|
               brfss$occ11=="Transport & material moving" | brfss$occ11=="military" |
               brfss$occ11=="Service"] <- "manual"
brfss$job[brfss$occ11=="Mgt, business/finance" | brfss$occ11=="Professional & related" |
               brfss$occ11=="Sales & related"| brfss$occ11=="Office & admin"] <- "nonManual"


    #creating 0/1 version of job with manual as exposed
brfss$job01[brfss$job=="manual"] <- 1
brfss$job01[brfss$job=="nonManual"] <- 0
     

    #creating 1/2 version of job with manual as exposed (1)
brfss$job12[brfss$job=="manual"] <- 1
brfss$job12[brfss$job=="nonManual"] <- 2

############################
##create outcome variables##
############################

brfss$outcome[brfss$op_any=="Yes" & brfss$X_rfdrhv7=="Yes"] <- 1
        #they both used op in the past year and are a heavy drinker
brfss$outcome[brfss$op_any=="No" & brfss$X_rfdrhv7=="No"] <- 0
        #they neither used op in the past year nor are a heavy drinker 

    #cleaning op_any variable and op_more variable
brfss$op_any[brfss$op_any=="DK" | brfss$op_any=="Refused" | brfss$op_any==""] <- NA
brfss$op_more[brfss$op_more=="DK" | brfss$op_more=="Refused" | brfss$op_more==""] <- NA


    #creating 0/1 version of op_any and op_more
brfss$opAny01[brfss$op_any=="Yes"] <- 1
brfss$opAny01[brfss$op_any=="No"] <- 0

brfss$opMore01[brfss$op_more=="Yes"] <- 1
brfss$opMore01[brfss$op_more=="No"] <- 0


    #creating 1/2 version of op_any and op_more
brfss$opAny12[brfss$op_any=="Yes"] <- 1
brfss$opAny12[brfss$op_any=="No"] <- 2

brfss$opMore12[brfss$op_more=="Yes"] <- 1
brfss$opMore12[brfss$op_more=="No"] <- 2

#################
#Reorder Columns#
#################
order <- c("year", "sex", "occ11", "ind11", "job", "job01", "job12", "op_any", "opAny01",
           "opAny12", "op_more", "opMore01", "opMore12", "age", "X_age_g", "hlthpln1",
           "X_racegr3", "X_race", "X_prace1","X_mrace1", "X_imprace", "smokday2", "X_smoker3",
           "X_rfsmok3","educa" ,"hsComplete", "X_ststr", "X_llcpwt", "op_days", "op_more", "op_norx", "op_less",
           "genhlth", "mjever", "mj30day", "jobactiv", "emplinjm", "use_wt", "use_comb",
           "agestd", "agestdwt", "sector", "X_rfdrhv7", "outcome")

brfss <- brfss[, order]

#########################################
#Recoding Missings & Recoding covariates#
#########################################
brfss$emplinjm[brfss$emplinjm=="Refused" | brfss$emplinjm=="DK" | brfss$emplinjm==""]<- NA
brfss$hlthpln1[brfss$hlthpln1=="Refused" | brfss$hlthpln1=="DK"]<- NA

brfss$educa[brfss$educa=="Refused"] <- NA
brfss$hsComplete <- ifelse(brfss$educa=="Grade 1-8" | brfss$educa=="Grade 9-11", 0, 1)



#####################################
##tabulate new variables we created##
#####################################
table(brfss$sector, useNA="ifany")
table(brfss$job, useNA="ifany")

    #crosstabs
table(brfss$op_any, brfss$job, deparse.level=2)
table(brfss$op_more, brfss$job, deparse.level=2)


table(brfss$job, brfss$outcome, deparse.level = 2)

######################################
##stratifying by different variables##
######################################
    #work injury
brfss$emplinjm[brfss$emplinjm=="Refused" | brfss$emplinjm=="DK" | brfss$emplinjm==""]<- NA

(rrTabStrat <- table(brfss$job, brfss$outcome, 
                     brfss$emplinjm, deparse.level=2))
(stratInj <- table(brfss$job, brfss$op_any, brfss$emplinjm, deparse.level=2))


    #are they insured
brfss$hlthpln1[brfss$hlthpln1=="Refused" | brfss$hlthpln1=="DK"]<- NA

(stratInsr <- table(brfss$job, brfss$outcome, brfss$hlthpln1, deparse.level=2))
(stratInsrOp <- table(brfss$job, brfss$op_any, brfss$hlthpln1, deparse.level=2))

    #race group
(stratRace <- table(brfss$job, brfss$outcome, brfss$X_prace1, deparse.level=2))


    #age
(stratAge <- table(brfss$job, brfss$op_any, brfss$X_age_g, deparse.level=2))

    #educa
brfss$educa[brfss$educa=="Refused"] <- NA
table(brfss$educa) 

(stratEd <- table(brfss$job, brfss$op_any, brfss$educa, deparse.level=2))

#######################
#Export Clean-ish Data#
#######################
write.csv(brfss, "brfssClean.csv")


##############
##power calc##
##############
    #prev of op non manual of 12%, manual sample size of 1500
epi.ssxsectn(
  pdexp1=NA,
  pdexp0=.12,
  n=5710,
  power=0.8,
  r=1500/4210
)

  #prev of op non manual of 12.5%, manual sample size of 1500
epi.ssxsectn(
  pdexp1=NA,
  pdexp0=.125,
  n=5710,
  power=0.8,
  r=1500/4210
)

  #prev of op non manual of 13%, manual sample size of 1500
epi.ssxsectn(
  pdexp1=NA,
  pdexp0=.13,
  n=5710,
  power=0.8,
  r=1500/4210
)

  #prev of op non manual of 12%, manual sample size of 2000
epi.ssxsectn(
  pdexp1=NA,
  pdexp0=.12,
  n=5710,
  power=0.8,
  r=2000/3710
)

  #prev of op non manual of 12.5%, manual sample size of 2000
epi.ssxsectn(
  pdexp1=NA,
  pdexp0=.125,
  n=5710,
  power=0.8,
  r=2000/3710
)

  #prev of op non manual of 13%, manual sample size of 2000
epi.ssxsectn(
  pdexp1=NA,
  pdexp0=.13,
  n=5710,
  power=0.8,
  r=2000/3710
)

  #prev of op non manual of 12%, manual sample size of 2500
epi.ssxsectn(
  pdexp1=NA,
  pdexp0=.12,
  n=5710,
  power=0.8,
  r=2500/3210
)

  #prev of op non manual of 12.5%, manual sample size of 2500
epi.ssxsectn(
  pdexp1=NA,
  pdexp0=.125,
  n=5710,
  power=0.8,
  r=2500/3210
)

  #prev of op non manual of 13%, manual sample size of 2500
epi.ssxsectn(
  pdexp1=NA,
  pdexp0=.13,
  n=5710,
  power=0.8,
  r=2500/3210
)


    #2018 manual laborers (to calc prev in unexposed)
326+200+188+264+11+789+117
  #total 5494; nonmanual 3599


)


