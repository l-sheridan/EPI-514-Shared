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

brfss <- subset(combo, select = c(year, X_ststr, sex, age, X_ageg5yr, X_age65yr,
                                  X_age_g, X_hispanc, educa, X_race, X_imprace,
                                  employ1, genhlth, X_racegr3, X_m_race, X_prace1,
                                  X_phys2, hlthpln1, medicare,
                                  hlthcvr1,smoke100, smokday2, X_smoker3, X_rfsmok3, X_rfdrhv7,
                                  mjever, mj30day, op_any, op_days, op_more,
                                  op_norx, op_less, occ11, ind11, 
                                  jobactiv, emplinjm, injpay, injpay1,
                                  X_llcpwt, use_wt, use_comb, agestd, agestdwt))
        #only included some weighting variables, probably a good idea to double
        #check before we weight things


      #delete big  data frame for space (after subsetting)
combo <- NA

##################################################
##explore data, tabulate industry and occupation##
##################################################
View(brfss)
table(brfss$op_more, useNA="ifany")

table(brfss$occ11, useNA="ifany")
table(brfss$ind11, useNA="ifany")

########################################################################################
##create exposure variables (one using 3 sector model, one using blue vs white collar)##
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
     

############################
##create outcome variables##
############################

brfss$outcome[brfss$op_any=="Yes" & brfss$X_rfdrhv7=="Yes"] <- 1
        #they both used op in the past year and are a heavy drinker
brfss$outcome[brfss$op_any=="No" & brfss$X_rfdrhv7=="No"] <- 0
        #they neither used op in the past year nor are a heavy drinker 


#####################################
##tabulate new variables we created##
#####################################
table(brfss$sector, useNA="ifany")
table(brfss$job, useNA="ifany")

    #crosstabs
table(brfss$op_more,  brfss$job, deparse.level=2)
table(brfss$op_any, brfss$job, deparse.level=2)

table(brfss$job, brfss$outcome, deparse.level = 2)

######################################
##stratifying by different variables##
######################################
    #work injury
brfss$emplinjm[brfss$emplinjm=="Refused" | brfss$emplinjm=="DK" | brfss$emplinjm==""]<- NA

(rrTabStrat <- table(brfss$job, brfss$outcome, 
                     brfss$emplinjm, deparse.level=2))

    #hispanic
brfss$X_hispanc[brfss$X_hispanc=="Refused"] <- NA
(stratHisp <- table(brfss$job, brfss$outcome, brfss$X_hispanc, deparse.level = 2))

    #are they insured
brfss$hlthpln1[brfss$hlthpln1=="Refused" | brfss$hlthpln1=="DK"]<- NA
(stratInsr <- table(brfss$job, brfss$outcome, brfss$hlthpln1, deparse.level=2))

    #race group
(stratRace <- table(brfss$job, brfss$outcome, brfss$X_prace1, deparse.level=2))

##############
##power calc##
##############
    #prev in unexposed (white collar) is like 0.6%, but I am eyeballing the table I found
epi.ssxsectn(
  pdexp1=NA,
  pdexp0=.006,
  n=5710,
  power=0.8,
  r=1198/4512
)

#####################################################################################
#separate data frame with no missing/refused/DK data for any opioid use in past year# 
#####################################################################################
    #note: not entirely necessary now that we have a separate outcome variable
    #that already counted these as missing, but I wanted to keep the code just in
    #case, plus the power calc is slightly different so I want that somewhere
complete <- brfss[brfss$op_any!="Refused" & brfss$op_any!="DK",]
table(complete$job, useNA="ifany")

epi.ssxsectn(
  pdexp1=NA,
  pdexp0=.006,
  n=5694,
  power=0.8,
  r=1192/4502
)


