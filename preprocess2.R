#### Load Library ####
library(tidyverse)


#### Load data ####
load("./data/WVS_Cross-National_Wave_7_rData_v5_0.rdata")
names(`WVS_Cross-National_Wave_7_v5_0`)


#### Retrieve date when the survey are carried out ####
# select year and country code
timedata <- `WVS_Cross-National_Wave_7_v5_0`%>%
    select(A_YEAR,B_COUNTRY_ALPHA) %>% 
    mutate(country = as.factor(B_COUNTRY_ALPHA))
# calculate the mean of year for each country
time_sum <- timedata %>%
    group_by(country) %>% 
    summarize(mean_year = mean(A_YEAR))
# save 
write.csv(time_sum, file = "./data/SurveyDate.csv")


#### Select data I need ####
mydata1 <- `WVS_Cross-National_Wave_7_v5_0`%>%
    mutate(id = seq(1, length(B_COUNTRY))) %>%
    select(B_COUNTRY, id, giniWB, GDPpercap2, v2x_corr, 
           freestfh, popWB2019, unemploytotal, 
           Q275, Q262, Q260, Q288, Q94R, Q95R, Q96R, Q97R, Q98R, Q99R,
           Q100R, Q101R, Q102R, Q103R, Q104R, Q105R, Q279, Q273, 
           Q171, Q287, Q47, Q289, Q263, H_URBRURAL, Q65, Q69,
           Q70, Q71, Q72, Q73, Q74, Q49, Q46, Q144, Q51, Q54, Q270, 
           Q131, Q240, Q121, Q112, Q50, Q271, 
           Q201, Q277, Q278, Q252, Q48, Q57, Q60, Q61, Q58, Q59) 
write.csv(mydata1, "./data/raw_data.csv")

#### Replace the negative values with NA ####
transmissing <- function(x) {
    x[which(x<0)] <- NA 
    return(x)
}


#### Get the replaced data ####
mydata1_r <- as.data.frame(lapply(mydata1,transmissing))
#unique(mydata1_r$Q102)
#sum(is.na(mydata1_r$Q102)) == sum(mydata1$Q102<0)
#table(mydata1$freestfh)
#table(mydata1_r$freestfh)


#### Computing ####
# religion share
Reli_share_data <- mydata1_r %>%
    #mutate(B_COUNTRY = as.factor(B_COUNTRY)) %>%
    group_by(B_COUNTRY) %>%
    summarize(Reli_share = sum(Q289>0, na.rm = TRUE)/length(Q289))
print(Reli_share_data, n = length(Reli_share_data$B_COUNTRY))


# Membership
memberdata <- mydata1_r %>%
    select(Q94R, Q95R, Q96R, Q97R, Q98R, Q99R,
           Q100R, Q101R, Q102R, Q103R, Q104R, Q105R)
count1 <- numeric(0) # find cases that are missing values on all predictors
for (i in 1:length(memberdata$Q94R)) {
    if (all(is.na(memberdata[i,])==1)) {
        count1 <- c(count1, i)
    }
}
membership <- rep(0, length(memberdata$Q94R)) # store the sum of all predictors
for (i in 1:length(memberdata$Q94R)) {
    if (all(is.na(memberdata[i,])==1)) {
        membership[i] <- NA
    } else {
        membership[i] <- rowSums(memberdata[i,], na.rm = TRUE)
    }
}


# Political confidence
PCdata <- mydata1_r %>%
    select(Q65, Q69, Q70, Q71, Q72, Q73, Q74) %>%
    mutate(Q65 = max(Q65, na.rm = TRUE)-Q65+1,
           Q69 = max(Q69, na.rm = TRUE)-Q69+1,
           Q70 = max(Q70, na.rm = TRUE)-Q70+1,
           Q71 = max(Q71, na.rm = TRUE)-Q71+1,
           Q72 = max(Q72, na.rm = TRUE)-Q72+1,
           Q73 = max(Q73, na.rm = TRUE)-Q73+1,
           Q74 = max(Q74, na.rm = TRUE)-Q74+1)
count2 <- numeric(0) # find cases that are missing values on all predictors
for (i in 1:length(PCdata$Q65)) {
    if (all(is.na(PCdata[i,])==1)) {
        count2 <- c(count2, i)
    }
}
Poli_confidence <- rep(0, length(PCdata$Q65)) # store the mean of all predictors
for (i in 1:length(PCdata$Q65)) {
    if (all(is.na(PCdata[i,])==1)) {
        Poli_confidence[i] <- NA
    } else {
        Poli_confidence[i] <- rowMeans(PCdata[i,], na.rm = TRUE)
    }
}


# Financial problems
FPdata <- mydata1_r %>%
    select(Q51, Q54) %>%
    mutate(Q51 = max(Q51, na.rm = TRUE)-Q51+1,
           Q54 = max(Q54, na.rm = TRUE)-Q54+1)
# find cases that are missing values on all predictors
count3 <- numeric(0)
for (i in 1:length(FPdata$Q51)) {
    if (all(is.na(FPdata[i,])==1)) {
        count3 <- c(count3, i)
    }
}
# create a variable to store the mean of all predictors
Finan_problem <- rep(0, length(FPdata$Q51))
for (i in 1:length(FPdata$Q51)) {
    if (all(is.na(FPdata[i,])==1)) {
        Finan_problem[i] <- NA
    } else {
        Finan_problem[i] <- rowMeans(FPdata[i,], na.rm = TRUE)
    }
}

mydata1_r$membership <- membership
mydata1_r$Poli_confidence <- Poli_confidence
mydata1_r$Finan_problem <- Finan_problem


#### Inverted coding ####
mydata2 <- mydata1_r %>%
    select(B_COUNTRY, id, giniWB, GDPpercap2, v2x_corr, 
           freestfh, popWB2019, unemploytotal, 
           Q275, Q262, Q260, Q288, membership, Q279, Q273, 
           Q171, Q287, Q47, Q289, Q263, H_URBRURAL, 
           Poli_confidence, Q49, Q46, Q144, Finan_problem, Q270, 
           Q131, Q240, Q121, Q112, Q50, Q271, 
           Q201, Q277, Q278, Q252, Q48, Q57, Q60, Q61, Q58, Q59) %>%
    mutate(Q171 = max(Q171, na.rm = TRUE)-Q171+1,
           Q287 = max(Q287, na.rm = TRUE)-Q287+1,
           Q47 = max(Q47, na.rm = TRUE)-Q47+1,
           Q46 = max(Q46, na.rm = TRUE)-Q46+1,
           Q131 = max(Q131, na.rm = TRUE)-Q131+1,
           Q201 = max(Q201, na.rm = TRUE)-Q201+1,
           Q60 = max(Q60, na.rm = TRUE)-Q60+1,
           Q61 = max(Q61, na.rm = TRUE)-Q61+1,
           Q58 = max(Q58, na.rm = TRUE)-Q58+1,
           Q59 = max(Q59, na.rm = TRUE)-Q59+1)


#### Dummy coding part 1 ####
mydata3 <- mydata2

mydata3$Q260[which(mydata3$Q260==2)] <- 0
mydata3$H_URBRURAL[which(mydata3$H_URBRURAL==2)] <- 0
mydata3$Q144[which(mydata3$Q144==2)] <- 0
mydata3$Q57[which(mydata3$Q57==2)] <- 0
mydata3$Q263[which(mydata3$Q263==1)] <- 0
mydata3$Q263[which(mydata3$Q263==2)] <- 1

table(mydata2$Q260)
table(mydata3$Q260)
table(mydata2$H_URBRURAL)
table(mydata3$H_URBRURAL)
table(mydata2$Q144)
table(mydata3$Q144)
table(mydata2$Q57)
table(mydata3$Q57)
table(mydata2$Q263)
table(mydata3$Q263)


#### Dummy coding part 2 ####
mydata4 <- mydata3 %>%
    mutate(Part_time       = ifelse(Q279==2, 1, 0),
           Self_employ     = ifelse(Q279==3, 1, 0),
           Retired         = ifelse(Q279==4, 1, 0),
           Housewife       = ifelse(Q279==5, 1, 0),
           Student         = ifelse(Q279==6, 1, 0),
           Unemployed      = ifelse(Q279==7, 1, 0),
           Other_employ    = ifelse(Q279==8, 1, 0), # employment
           
           Living_together = ifelse(Q273==2, 1, 0),
           Divorced        = ifelse(Q273==3, 1, 0),
           Separated       = ifelse(Q273==4, 1, 0),
           Widowed         = ifelse(Q273==5, 1, 0),
           Single_person   = ifelse(Q273==6, 1, 0), # marriage status
           
           Catholic        = ifelse(Q289==1, 1, 0),
           Protestant      = ifelse(Q289==2, 1, 0),
           Orthodox        = ifelse(Q289==3, 1, 0),
           Jew             = ifelse(Q289==4, 1, 0),
           Muslim          = ifelse(Q289==5, 1, 0),
           Hindu           = ifelse(Q289==6, 1, 0),
           Buddhist        = ifelse(Q289==7, 1, 0),
           Other_Christian = ifelse(Q289==8, 1, 0),
           Other_religion  = ifelse(Q289==9, 1, 0), # religious
           
           With_parents      = ifelse(Q271==2, 1, 0),
           With_parentsinLaw = ifelse(Q271==3, 1, 0),
           With_both         = ifelse(Q271==4, 1, 0), # Living with 
           
           ) %>%
    select(B_COUNTRY, id, giniWB, GDPpercap2, v2x_corr, 
           freestfh, popWB2019, unemploytotal, 
           Q275, Q262, Q260, Q288, membership, 
           Part_time, Self_employ, Retired, Housewife,
           Student, Unemployed, Other_employ, 
           Living_together, Divorced,
           Separated, Widowed, Single_person,
           Q171, Q287, Q47, 
           Catholic, Protestant, Orthodox, Jew, Muslim, 
           Hindu, Buddhist, Other_Christian, Other_religion,
           Q263, H_URBRURAL, Poli_confidence, Q49, Q46, 
           Q144, Finan_problem, Q270, 
           Q131, Q240, Q121, Q112, Q50, 
           With_parents, With_parentsinLaw, With_both,
           Q201, Q277, Q278, Q252, Q48, 
           Q57, Q60, Q61, Q58, Q59)

table(mydata3$Q279)
table(mydata4$Part_time)
table(mydata4$Self_employ)
table(mydata4$Retired)
table(mydata4$Housewife)
table(mydata4$Student)
table(mydata4$Unemployed)
table(mydata4$Other)

table(mydata3$Q273)
table(mydata4$Living_together)
table(mydata4$Divorced)
table(mydata4$Separated)
table(mydata4$Widowed)
table(mydata4$Single_person)

table(mydata3$Q289)
table(mydata3$Q271)


#### Merge 3 datasets ####
contextualData <- read.csv("./data/contextualdata.csv", header = TRUE)
mydata5 <- left_join(mydata4,contextualData,by="B_COUNTRY")
mydata6 <- left_join(mydata5,Reli_share_data,by="B_COUNTRY")

write.csv(mydata6, "preprocessed_data.csv")


#### Get the sample size of each variable ####
getvalidlength <- function(x) {
    length(x) - sum(is.na(x))
}
validlength <- sapply(mydata6,getvalidlength)
max(validlength) # maximum sample size
min(validlength) # minimun sample size
validlength[which(validlength==min(validlength))] # variable with minimum sample size 
write.csv(validlength, file = "./data/samplesize0509.csv")

#### Retrieve complete cases ####
mydata6$complete_data <- complete.cases(mydata6)
sum(mydata6$complete_data)
finaldata <- mydata6 %>%
    filter(complete_data==1) %>%
    select(B_COUNTRY, id, giniWB, GDPpercap2, v2x_corr, 
           freestfh, popWB2019, unemploytotal, Reli_share,
           ROL, GE, Edu, Ethnic, Language, Religion,
           Q275, Q262, Q260, Q288, membership, 
           Part_time, Self_employ, Retired, Housewife,
           Student, Unemployed, Other_employ, 
           Living_together, Divorced,
           Separated, Widowed, Single_person,
           Q171, Q287, Q47, 
           Catholic, Protestant, Orthodox, Jew, Muslim, 
           Hindu, Buddhist, Other_Christian, Other_religion,
           Q263, H_URBRURAL, Poli_confidence, Q49, Q46, 
           Q144, Finan_problem, Q270, 
           Q131, Q240, Q121, Q112, Q50, 
           With_parents, With_parentsinLaw, With_both,
           Q201, Q277, Q278, Q252, Q48, 
           Q57, Q60, Q61, Q58, Q59) 
    
write.csv(finaldata, "final_data.csv")

#### if we don't use education ####
mydata7 <- mydata6 %>% select(-Edu)
mydata7$complete_data <- complete.cases(mydata7)
sum(complete.cases(mydata7))
finaldata2 <- mydata7 %>%
    filter(complete_data==1) %>%
    select(B_COUNTRY, id, giniWB, GDPpercap2, v2x_corr, 
           freestfh, popWB2019, unemploytotal, Reli_share,
           ROL, GE, Ethnic, Language, Religion,
           Q275, Q262, Q260, Q288, membership, 
           Part_time, Self_employ, Retired, Housewife,
           Student, Unemployed, Other_employ, 
           Living_together, Divorced,
           Separated, Widowed, Single_person,
           Q171, Q287, Q47, 
           Catholic, Protestant, Orthodox, Jew, Muslim, 
           Hindu, Buddhist, Other_Christian, Other_religion,
           Q263, H_URBRURAL, Poli_confidence, Q49, Q46, 
           Q144, Finan_problem, Q270, 
           Q131, Q240, Q121, Q112, Q50, 
           With_parents, With_parentsinLaw, With_both,
           Q201, Q277, Q278, Q252, Q48, 
           Q57, Q60, Q61, Q58, Q59) 
write.csv(finaldata2, "final_data2.csv")
xlength <- sapply(lapply(finaldata2,unique),length)

desdata <- finaldata2 %>%
    group_by(B_COUNTRY) %>%
    summarise(SampleSize = length(B_COUNTRY),
              GINI = unique(giniWB),
              GDPpc = unique(GDPpercap2),
              Corruption = unique(v2x_corr),
              Democracy = unique(freestfh),
              Population = unique(popWB2019),
              UnemplyRate = unique(unemploytotal),
              ReligionShare = unique(Reli_share),
              RuleOfLaw = unique(ROL),
              GovernEffect = unique(GE),
              EthinicFrac = unique(Ethnic),
              ReliFrac = unique(Religion),
              Education = mean(Q275),
              Age = mean(Q262),
              Female = 1-mean(Q260),
              Income = mean(Q288),
              Membership = mean(membership),
              ReliAttendance = mean(Q171),
              SocialClass = mean(Q287),
              Health = mean(Q47),
              Immigrant = mean(Q263),
              UrbanResidence = mean(H_URBRURAL),
              PoliConfidence = mean(Poli_confidence),
              LifeSatisfaction = mean(Q49),
              SubHappiness = mean(Q46),
              Victim = mean(Q144),
              FinanProblem = mean(Finan_problem),
              HouseholdSize = mean(Q270),
              PerSecurity = mean(Q131),
              Ideology = mean(Q240),
              AttitudeMigrants = mean(Q121),
              PerCorruption = mean(Q112),
              FinanSatisfaction = mean(Q50),
              NewspaperUse = mean(Q201),
              MotherEdu = mean(Q277),
              FatherEdu = mean(Q278),
              PoliSatisfaction = mean(Q252),
              SenseofControl = mean(Q48),
              GeneralizedTrust = mean(Q57),
              Trust1stMeet = mean(Q61),
              TrustFamily = mean(Q58),
              TrustNeighborhood = mean(Q59),
              TrustKnow = mean(Q60))
write.csv(desdata, "./data/descriptive.csv")

#### calculate the retention rate of each country
ori_SS <- mydata1 %>%
    group_by(B_COUNTRY) %>%
    summarise(SampleSize = length(B_COUNTRY))
re_SS <- finaldata2 %>%
    group_by(B_COUNTRY) %>%
    summarise(SampleSize = length(B_COUNTRY))
retention <- left_join(ori_SS,re_SS,by="B_COUNTRY")
write.csv(retention, "./data/retention.csv")

ori_SS <- mydata1 %>%
    group_by(B_COUNTRY) %>%
    summarise(SampleSize = length(B_COUNTRY))
re_SS1 <- finaldata %>%
    group_by(B_COUNTRY) %>%
    summarise(SampleSize = length(B_COUNTRY))
re_SS2 <- finaldata2 %>%
    group_by(B_COUNTRY) %>%
    summarise(SampleSize = length(B_COUNTRY))
retention <- left_join(ori_SS,re_SS,by="B_COUNTRY")
write.csv(retention, "./data/retention.csv")