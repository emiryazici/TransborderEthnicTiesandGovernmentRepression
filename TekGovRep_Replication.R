################################################################################
### Transborder Ethnic Ties and Repression of Ethnic Minorities
### 2024
### Replication Code
################################################################################

################################################################################
library(tidyverse)
library(survival)
library(dplyr) 

################################################################################
### Population and Sample
################################################################################

List_of_Disadvantaged_Minorities_with_at_least_one_TEK <- read_excel("List of Disadvantaged Minorities with at least one TEK.xlsx")
summary(List_of_Disadvantaged_Minorities_with_at_least_one_TEK)
ls(List_of_Disadvantaged_Minorities_with_at_least_one_TEK)

# Sampling Process

# First, find the number of groups and EKs in the population 

data_count_1 <- List_of_Disadvantaged_Minorities_with_at_least_one_TEK %>%                              
  group_by(gwgroupid) %>%
  summarise(count = n_distinct(gwgroupid))
data_count_1 

data_count_2 <- List_of_Disadvantaged_Minorities_with_at_least_one_TEK %>%                              
  group_by(EK1_ccode) %>%
  summarise(count = n_distinct(EK1_ccode))
data_count_2 

#Sampling Step 1: Keep only the minority groups at risk
SamplingStep1<-  subset(List_of_Disadvantaged_Minorities_with_at_least_one_TEK,  statusname=="POWERLESS" | statusname=="DISCRIMINATED" | statusname=="SELF EXCLUSION") 

#Sampling Step 2: Keep only the minority groups with at least one Powerful TEK
SamplingStep2 <- subset(SamplingStep1, `EK1 Status`=="MONOPOLY" | `EK1 Status`=="DOMINANT" | `EK1 Status`=="SENIOR PARTNER")                              

# See the total number of Minority groups and EKs in the Sample 
data_count_3 <- SamplingStep2 %>%                              
  group_by(gwgroupid) %>%
  summarise(count = n_distinct(gwgroupid))
data_count_3 

data_count_4 <- SamplingStep2 %>%                              
  group_by(EK1_ccode) %>%
  summarise(count = n_distinct(EK1_ccode))
data_count_4 

#195 minority groups with 69 EKs.

# Manually eliminated some of groups and EKs (see appendix for explanation)

# The remanining EKs are the following ones. 
TekGovRepData <- subset(SamplingStep2, Largesample$EK1_ccode==640 | 
  EK1_ccode==517 |
  EK1_ccode==365 | 
  EK1_ccode==666 | 
  EK1_ccode==255 |
  EK1_ccode==350 |
  EK1_ccode==710 |
  EK1_ccode==750 |
  EK1_ccode==770 |
  EK1_ccode==800 |
  EK1_ccode==732 |
  EK1_ccode==432 |
  EK1_ccode==516 |
  EK1_ccode==436 |
  EK1_ccode==475)

# I use stratified sampling to pick a sample from these EKs (see the manuscript and appendix).

# Final Check: Compare the groupsize in the population and the sample

summary(List_of_Disadvantaged_Minorities_with_at_least_one_TEK$groupsize)

SampleGroupSize <-  subset(List_of_Disadvantaged_Minorities_with_at_least_one_TEK, gwgroupid==33902000 |    
                             gwgroupid==34305000 | 
                             gwgroupid==34705000 |
                             gwgroupid==35002000 |
                             gwgroupid==35502000 |   
                             gwgroupid==35903000 |  
                             gwgroupid==36505000 |    
                             gwgroupid==36549000 |    
                             gwgroupid==36602000 |    
                             gwgroupid==36702000 |    
                             gwgroupid==36803000 |    
                             gwgroupid==36902000 |    
                             gwgroupid==36905000 |    
                             gwgroupid==49013000 |    
                             gwgroupid==51702000 |    
                             gwgroupid==64505000 |    
                             gwgroupid==65205000 |    
                             gwgroupid==70302000 |    
                             gwgroupid==70403000 |    
                             gwgroupid==70503000 |    
                             gwgroupid==71036000)  

summary(SampleGroupSize$groupsize)
################################################################################
############################# Clean the Data ############################# 
################################################################################

TekGovRepData <- read.csv("~/Library/CloudStorage/OneDrive-Kişisel/Masaüstü/Research/Transborder Ethnic Ties and Government Repression/II Submission/Data, Codebok, Research Design/TekGovRep/TekGovRepData.csv", sep=";")
summary(TekGovRepData)
TekGovRepData <- TekGovRepData %>% drop_na(dyadid)
summary(TekGovRepData)

# This TekGovRepData includes the original data added to the dataset in excel manually

# Remove the unnecessary variables from the Master Data
TekGovRepData2 <- subset(TekGovRepData, select = -c(distcap, domesticviolence, EK_support, EK_supportlevel, G_rep_cul, G_rep_econ, G_rep_phys, G_rep_pol,
                            G_repressionlevel, MDR, MDR1, MID_DV, MID_DV1, mutualdem,peaceyears_dv, peaceyears_dv2, peaceyears_dv3,            
                            peaceyears_mid, peaceyears_mid2, peaceyears_mid3, revstatb, SQ_Predicted, SQ1))

TekGovRepDataMAsterData <- TekGovRepData
TekGovRepData <- TekGovRepData2
TekGovRepData2 <- NULL
write.csv(TekGovRepData, "TekGovRepData")


### Convert character to numeric
TekGovRepData$groupsize  <- gsub(",", ".", TekGovRepData$groupsize )  
TekGovRepData$EK1_size  <- gsub(",", ".", TekGovRepData$EK1_size )  
TekGovRepData$G_EthnicFract  <- gsub(",", ".", TekGovRepData$G_EthnicFract )  
TekGovRepData$EK_EthnicFract  <- gsub(",", ".", TekGovRepData$EK_EthnicFract )  
TekGovRepData$g_gdp  <- gsub(",", ".", TekGovRepData$g_gdp )  
TekGovRepData$ek_gdp  <- gsub(",", ".", TekGovRepData$ek_gdp )  
TekGovRepData$distcap  <- gsub(",", ".", TekGovRepData$distcap )  

TekGovRepData$groupsize <- as.numeric(TekGovRepData$groupsize)
TekGovRepData$EK1_size <- as.numeric(TekGovRepData$EK1_size)
TekGovRepData$G_EthnicFract <- as.numeric(TekGovRepData$G_EthnicFract)
TekGovRepData$EK_EthnicFract <- as.numeric(TekGovRepData$EK_EthnicFract)
TekGovRepData$g_gdp <- as.numeric(TekGovRepData$g_gdp)
TekGovRepData$ek_gdp <- as.numeric(TekGovRepData$ek_gdp)
TekGovRepData$distcap <- as.numeric(TekGovRepData$distcap)

################################################################################
########################### Merge - Trade and CINC ############################
################################################################################

### COW Dydic Trade Data
newcowtrade <- select(Dyadic_COW_4.0, ccode1, ccode2, year, smoothtotrade)
 
## Create dyadid variables to match with the dyads in the main dataset
newcowtrade <- newcowtrade %>%
  +   mutate(newcowtrade, dyadid = ifelse(newcowtrade$ccode2 > newcowtrade$ccode1, ccode1*1000 + ccode2, ccode2*1000 + ccode1))

## Clean unnecessary variables
newcowtrade <- select(newcowtrade, dyadid, year, smoothtotrade)
newcowtrade <- newcowtrade %>% 
  +   mutate(smoothtotrade = ifelse(smoothtotrade == -9, NA, smoothtotrade))

summary(newcowtrade)

sapply(newcowtrade,function(x) sum(is.na(x)))

TekGovRepData <- left_join(TekGovRepData, newcowtrade, by = c("dyadid", "year"))
View(TekGovRepData)

### PROBLEM: 433 (OUT OF 675) MİSSİNG OBS FOR TRADE DATA
summary(TekGovRepData$smoothtotrade)

### COW CINC 
NMC_60_abridged <- read_csv("NMC-60-abridged.csv")

# Merge for G
nmcdata <- select(NMC_60_abridged, ccode, year, cinc)
names(nmcdata)[names(nmcdata) == 'ccode'] <- 'countries_gwid'
names(nmcdata)[names(nmcdata) == 'cinc'] <- 'G_Cinc'
TekGovRepData <- left_join(TekGovRepData, nmcdata, by = c("countries_gwid", "year"))

# Merge for EK
nmcdata <- select(NMC_60_abridged, ccode, year, cinc)
names(nmcdata)[names(nmcdata) == 'ccode'] <- 'EK1_ccode'
names(nmcdata)[names(nmcdata) == 'cinc'] <- 'EK_Cinc'
TekGovRepData <- left_join(TekGovRepData, nmcdata, by = c("EK1_ccode", "year"))

summary(TekGovRepData$EK_Cinc)


TekGovRepData$EK_Cinc <- as.numeric(TekGovRepData$EK_Cinc)
TekGovRepData$G_Cinc <- as.numeric(TekGovRepData$G_Cinc)

################################################################################
################# Create the necessary variables for the models ################
################################################################################

install.packages("foreign")
library(foreign)
# A single alliance variable equal to 1 for any alliance between G and EK
TekGovRepData$GEKalliance <- ifelse(TekGovRepData$G_EK_DefensiveAlliance == 1 | TekGovRepData$G_EK_NeutralityAlliance == 1 | TekGovRepData$G_EK_NonaggressionAlliance == 1 , 1, 0)

# EK_sup_dip is between 0 and 1. Some observations are coded as 2 or 3 in the excelsheet of the original data collection process - 
# when they were supposed to be 1. Fix it.
TekGovRepData$EK_sup_dip <- ifelse(TekGovRepData$EK_sup_dip>0, 1, 0)

# Cinc Ratio
TekGovRepData$CincRatio <- NA
TekGovRepData$CincRatio <- TekGovRepData$EK_Cinc/TekGovRepData$G_Cinc

# Disaggregated Dispersion Variable
TekGovRepData$Dispersion2 <- ifelse(TekGovRepData$Dispersion == 1 | TekGovRepData$Dispersion == 2 | TekGovRepData$Dispersion == 3, 1, 0)
summary(TekGovRepData$Dispersion2)

## DV: Repression based on EPR's Data
TekGovRepData$REP <- ifelse(TekGovRepData$statusname=="DISCRIMINATED" , 1, 0)
summary(TekGovRepData$REP)

write.csv(TekGovRepData, "TekGovRepData")

################################################################################
##########################Summary Statistics####################################
library(stargazer)
stargazer(TekGovRepData)
################################################################################

################################################################################
################################MODELS#########################################
################################################################################

install.packages("glmmML")
library(glmmML)

install.packages("miceadds")
library(miceadds)

install.packages("Matrix")
library(Matrix)

install.packages("lme4")
library(lme4)


ls(TekGovRepData)

#Pooled Logit
Model1Pooled <- glm(REP ~  CincRatio + EK_sup_dip + EK_sup_econ + G_EK_Contiguity +  
                      Dispersion2 + M_EK_LangRel + Dual_citizenship + Diaspora + 
                      EK_EthnicFract + EK_RegimeType  + GEKalliance + G_RegimeType +
                       log(g_gdp) + 
                      Job_security + M_goal + M_coherence + M_support + peaceyears_rep + 
                      peaceyears_rep2 + peaceyears_rep3, family=binomial(link="logit"), data=TekGovRepData)
summary(Model1Pooled)


#Clustered SE
library(lmtest)
library(sandwich)
coeftest(Model1Pooled, vcov. = vcovCL(Model1Pooled, cluster = TekGovRepData$dyadid, type = "HC0"))

##### Logit with Cluster Robust Standard Errors
Model2Clustered <- miceadds::glm.cluster( data=TekGovRepData, formula=REP ~ CincRatio + EK_sup_dip + EK_sup_econ + G_EK_Contiguity +  
                                            Dispersion2 + M_EK_LangRel + Dual_citizenship + Diaspora + 
                                            EK_EthnicFract + EK_RegimeType +  GEKalliance + G_RegimeType + log(g_gdp) +
                                            Job_security  + M_goal + M_coherence + M_support + peaceyears_rep + 
                                            peaceyears_rep2 + peaceyears_rep3,
                                          cluster="dyadid", family=binomial(link="logit"))
coef(Model2Clustered)
vcov(Model2Clustered)
summary(Model2Clustered)


## Multilevel Model - Mixed Effects (dyadid)
# Variables that do not vary over time are excluded (as well as the duration variables)
model4ML <- glmer(REP ~  REP ~ CincRatio + EK_sup_dip + EK_sup_econ + G_EK_Contiguity +  
                    Dispersion2 + M_EK_LangRel + Dual_citizenship + Diaspora + 
                    EK_EthnicFract + EK_RegimeType +  GEKalliance + G_RegimeType + log(g_gdp) +
                    Job_security  + M_goal + M_coherence + M_support  
                    + (1|dyadid), data=TekGovRepData,
                    family=binomial(link="logit"), control = glmerControl(tolPwrss=1e-3))

summary(model4ML)


se <- sqrt(diag(vcov(model4ML)))
# table of estimates with 90% CI
(tab <- cbind(Est = fixef(model4ML), LL = fixef(model4ML) - 1.64 * se, UL = fixef(model4ML) + 1.64 *
                se))
options(scipen = 999)
exp(tab)

# Tex Tables
install.packages("stargazer") 
library(stargazer)
install.packages("texreg")
library(texreg)


texreg(list(Model1Pooled, Model2Clustered, model4ML), title=" Results", align=TRUE,
       no.space=TRUE)

################################################################################
######################## ROBUSTNESS TESTS ######################## 
################################################################################

' Omitted Variable Bias Sensitivity Test
install.packages("sensemakr")
library(sensemakr)
linear1.sensitivity <- sensemakr(model = linear1,
                                   treatment = "CincRatio",
                                   benchmark_covariates = "Diaspora",
                                   kd = 1:3)
linear1.sensitivity 
#Error: no applicable method for 'sensemakr' applied to an object of class "c('plm', 'panelmodel')"
'

### Try OLS  
library(plm)
#Random effects linear model (clustered)
linear1 <- plm(REP ~  CincRatio + EK_sup_dip + EK_sup_econ + G_EK_Contiguity +  
                 Dispersion2 + M_EK_LangRel + Dual_citizenship + Diaspora + 
                 EK_RegimeType + EK_EthnicFract + GEKalliance + G_RegimeType + log(g_gdp) +
                 Job_security  + M_goal + M_coherence + M_support + peaceyears_rep + 
                 peaceyears_rep2 + peaceyears_rep3, data=TekGovRepData, 
               model = "random", index= c("dyadid", "year"), 
               effect="individual")

summary(linear1)


#One-way fixed effects linear model
linear2fe <- plm(REP ~  CincRatio + EK_sup_dip + EK_sup_econ + G_EK_Contiguity +  
                   Dispersion2 + M_EK_LangRel + Dual_citizenship + Diaspora + 
                   EK_RegimeType + EK_EthnicFract + GEKalliance + G_RegimeType + log(g_gdp) +
                   Job_security  + M_goal + M_coherence + M_support + peaceyears_rep + 
                   peaceyears_rep2 + peaceyears_rep3, data=TekGovRepData, 
               model = "within", index= c("dyadid", "year"), 
               effect="individual")
summary(linear2fe)

phtest(linear1, linear2fe)



# Tex Tables
install.packages("stargazer") 
library(stargazer)

stargazer(linear1, linear2fe,title=" Results", align=TRUE,
                    dep.var.labels=c("Random Effects", "One-Way Fixed Effects", "Two-Way Fixed Effects"), 
                    omit.stat=c("LL","ser","f"),no.space=TRUE)

texreg(list(linear1, linear2fe), title=" Results", align=TRUE,
       no.space=TRUE)

################################################################################
###Substantive Effect Table
################################################################################
library(clarify)
library(ggplot2)

set.seed(123)
sim_coefs <- sim(Model1Pooled)

#Combined Credibility and Capability Effects

# Average Capability, Average Credibility
sim_est <- sim_setx(sim_coefs, x = list(CincRatio = 4), verbose = FALSE)
summary(sim_est)
round(sim_est, digits=2)
plot(sim_est) 

#High Capability, High Credibility
sim_est <- sim_setx(sim_coefs, x = list(CincRatio = c(44), EK_sup_econ = c(1), EK_sup_dip = c(1), Diaspora = c(1), 
                                        M_EK_LangRel = c(2), EK_EthnicFract = c(0.059), Dual_citizenship = c(1),
                                        G_EK_Contiguity = c(1), EK_RegimeType = c(3), 
                                        Dispersion2 = c(1), GEKalliance = c(0)), verbose = FALSE)
summary(sim_est)
round(sim_est, digits=2)
plot(sim_est) 

### Turkey Examples
IraqiT <-  subset(TekGovRepData, gwgroupid==64505000)
SyrianT <-  subset(TekGovRepData, gwgroupid==65205000)
Uyghurs <-  subset(TekGovRepData, gwgroupid==71036000)

summary(IraqiT$REP)
summary(IraqiT$CincRatio)
summary(IraqiT$EK_sup_dip)
summary(IraqiT$EK_sup_econ)

summary(SyrianT$REP)
summary(SyrianT$CincRatio)
summary(SyrianT$EK_sup_dip)
summary(SyrianT$EK_sup_econ)

summary(Uyghurs$REP)
summary(Uyghurs$CincRatio)
summary(Uyghurs$EK_sup_dip)
summary(Uyghurs$EK_sup_econ)

write.csv(TekGovRepData, file = "TekGovRepData.csv")


'
################################################################################
###Extra Graphs
################################################################################
# Simulate coefficients from a multivariate normal distribution
sim_coefs <- sim(Model1Pooled)

# Cinc Ratio is heavily skewed to the right.

# Graph 1 Capability from 1st Q to 3rd with Average Credibility
sim_est <- sim_setx(sim_coefs, x = list(CincRatio = 1:44), verbose = FALSE)
summary(sim_est)

plot(sim_est) + geom_smooth(conf.int = FALSE) + xlab("Capability (from 1st Quartile to 3rd Quartile)") + ylab("pr(Repression)")


# Predictions across the CincRatio (median to 3rd quartile) and Economic and Diplomatic Support to M (0 to 1) at typical values of the other predictors
# Median for Cinc ratio because it is heavily skewed to the right (mean is way higher than median). Min to mean shows biased effects.
sim_est <- sim_setx(sim_coefs, x = list(CincRatio = c(4:44), EK_sup_econ = 0:1, EK_sup_dip = 0:1),
                    verbose = FALSE)
summary(sim_est)
round(sim_est, 2)

#Plot of predicted values 
plot(sim_est)

# Graph 2
# Predictions across the CincRatio (median to 3rd quartile) and Diaspora (0 to 1) at typical values
# of the other predictors
sim_est <- sim_setx(sim_coefs, x = list(CincRatio = c(4,44), Diaspora = c(0,1)),
                    verbose = FALSE)
summary(sim_est)

#Plot of predicted values 
plot(sim_est)

# Graph 3
# Predictions across the CincRatio (median to 3rd quartile) and Minority-EK Language and Religious Ties (1 to 2) at typical values
# of the other predictors
sim_est <- sim_setx(sim_coefs, x = list(CincRatio = c(4,44), M_EK_LangRel = c(1,2)),
                    verbose = FALSE)
summary(sim_est)

#Plot of predicted values 
plot(sim_est)

# Graph 4
# Predictions across the CincRatio (median to 3rd quartile) and EK's Ethnic Fractionalization (min to max) at typical values
# of the other predictors

'

sim_est <- sim_setx(sim_coefs, x = list(CincRatio = c(4,44), EK_EthnicFract = c(0.06,0.33)) ,
                    verbose = FALSE)
summary(sim_est)

#Plot of predicted values 
plot(sim_est)


# Graph 5
# Predictions across the CincRatio (median to 3rd quartilen) and Dual Citizenship (min to max) at typical values
# of the other predictors
sim_est <- sim_setx(sim_coefs, x = list(CincRatio = c(4,44), Dual_citizenship = c(0,1)) ,
                    verbose = FALSE)
summary(sim_est)

#Plot of predicted values 
plot(sim_est)


# Graph 6
# Predictions across the CincRatio (median to 3rd quartile) and Dual Citizenship (min to max) at typical values
# of the other predictors
sim_est <- sim_setx(sim_coefs, x = list(CincRatio = c(4,44), G_EK_Contiguity = c(0,1)) ,
                    verbose = FALSE)
summary(sim_est)

#Plot of predicted values 
plot(sim_est)


# Graph 7
# Predictions across the CincRatio (min to median) and Dual Citizenship (min to max) at typical values
# of the other predictors
sim_est <- sim_setx(sim_coefs, x = list(CincRatio = c(4,44), Dispersion2 = c(0,1)) ,
                    verbose = FALSE)
summary(sim_est)

#Plot of predicted values 
plot(sim_est)



# Graph 8
# Predictions across the CincRatio (median to 3rd quartile) and Dual Citizenship (min to max) at typical values
# of the other predictors
sim_est <- sim_setx(sim_coefs, x = list(CincRatio = c(4,44), EK_RegimeType = c(1.46,3)) ,
                    verbose = FALSE)
summary(sim_est)

#Plot of predicted values 
plot(sim_est)


# Graph 9
# Predictions across the CincRatio (min to median) and Dual Citizenship (min to max) at typical values
# of the other predictors
sim_est <- sim_setx(sim_coefs, x = list(CincRatio = c(4,44), GEKalliance = c(0,1)) ,
                    verbose = FALSE)
summary(sim_est)
