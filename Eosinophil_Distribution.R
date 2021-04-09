rm(list=ls())
setwd("~/Desktop/Economics_Work/Allakos_Work/nhanes")
library("tidyverse")
library("openxlsx")
library("tidymodels")
library("stats")
library("psych")
library("arsenal")
library("spatstat")
library("Hmisc")

options(scipen=999)

#Import Combined NHANES Datasets
nhanes_00_18 <- read.csv("nhanes_00_18.csv") %>%
  mutate(
    age = ridageyr,
    eosinophils = lbxwbcsi * lbxeopct * 10,
    hist_eosins = ifelse(eosinophils >= 1000, 1000, eosinophils),
    lab_wts = ifelse(cycle == 0 | cycle == 1, wtmec4yr, wtmec2yr),
    age_group = ifelse( age %>% between( 0, 11), "0-11 years", 
                        ifelse( age %>% between( 12, 17), "12-17 years", 
                                ifelse(age %>% between( 18, 64), "18-64 years", 
                                       ifelse(age %>% between( 65, 80), "over 65 years", NA
                                              )))))

#Create Age Bracket Datasets
AgeGroup_0_to_11 <- nhanes_00_18 %>% 
  filter( age %>% between( 1, 11)
          )

AgeGroup_12_to_17 <- nhanes_00_18 %>% 
  filter( age %>% between( 12, 17)
  )

AgeGroup_18_to_64 <- nhanes_00_18 %>% 
  filter( age %>% between( 18, 64)
  )

AgeGroup_65_plus <- nhanes_00_18 %>% 
  filter( age %>% between( 65, 80)
  )

#####Distribution of Entire Age Group

#Weighted Distribution
Eos_Dist_All_Ages <- nhanes_00_18 %>% ggplot(aes(x=hist_eosins)) +
  geom_area(bins=100, aes(y = ..density.., weight = lab_wts), stat = "bin",
            color = "black", 
            fill = "steelblue") +
  labs(#title= "Blood Eosinophil Count Distribution (cells/uL)", 
    #subtitle="All Ages",
    caption= "2000-2018 NHANES Cycles",
    x="Eosinophil Count", 
    y="Count")
Eos_Dist_All_Ages

#Summary Stats (Make Weighted)
nhanes_00_18 %>% 
  select(eosinophils) %>% 
  summarise_all(list(mean = mean, min = min, max = max, sd = sd), na.rm = TRUE)

#Calculate Sample Size (Removing NA's)
nhanes_00_18 %>% 
  select(eosinophils) %>%
  filter(!is.na(eosinophils)) %>%
  summarise(n = n())

AgeGroup_0_to_11 %>% 
  select(eosinophils) %>%
  filter(!is.na(eosinophils)) %>%
  summarise(n = n())

AgeGroup_12_to_17 %>% 
  select(eosinophils) %>%
  filter(!is.na(eosinophils)) %>%
  summarise(n = n())

AgeGroup_18_to_64 %>% 
  select(eosinophils) %>%
  filter(!is.na(eosinophils)) %>%
  summarise(n = n())

AgeGroup_65_plus %>% 
  select(eosinophils) %>%
  filter(!is.na(eosinophils)) %>%
  summarise(n = n())

#####Distribution of Full Sample

#Weighted Summary Stats
weighted.mean(nhanes_00_18$eosinophils, nhanes_00_18$lab_wts, na.rm=TRUE)
weighted.median(nhanes_00_18$eosinophils, nhanes_00_18$lab_wts, na.rm=TRUE)
sqrt(wtd.var(nhanes_00_18$eosinophils, nhanes_00_18$lab_wts, na.rm=TRUE))

#Summary Stats (Make Weighted)
nhanes_00_18 %>% 
  select(eosinophils) %>% 
  summarise_all(list(mean = mean, min = min, max = max, sd = sd), na.rm = TRUE)

#Distribution
weighted_cumsum_All_Ages <- function(a){nhanes_00_18 %>% 
    filter( eosinophils <= a ) %>%
    summarise( weighted_sum = sum(lab_wts)/sum(nhanes_00_18$lab_wts[nhanes_00_18$eosinophils>=0 & nhanes_00_18$eosinophils <= 10000], na.rm=TRUE) )
}

weighted_cumsum_All_Ages(0)
weighted_cumsum_All_Ages(100)
weighted_cumsum_All_Ages(200)
weighted_cumsum_All_Ages(300)
weighted_cumsum_All_Ages(400)
weighted_cumsum_All_Ages(500)
weighted_cumsum_All_Ages(600)
weighted_cumsum_All_Ages(700)
weighted_cumsum_All_Ages(800)
weighted_cumsum_All_Ages(900)
weighted_cumsum_All_Ages(1000)


#####Distribution of 0-11 Age Group

#Weighted Distribution
Eos_Dist_0_to_11_years <- AgeGroup_0_to_11 %>% ggplot(aes(x=hist_eosins)) +
  geom_area(bins=100, aes(y = ..density.., weight = lab_wts), stat = "bin",
            color = "black", 
            fill = "steelblue") +
  labs(#title= "Blood Eosinophil Count Distribution (cells/uL)", 
    #subtitle="Ages 0-11",
    caption= "2000-2018 NHANES Cycles",
    x="Eosinophil Count", 
    y="Count")
Eos_Dist_0_to_11_years

#Summary Stats (Make Weighted)
AgeGroup_0_to_11 %>% 
  select(eosinophils) %>% 
  summarise_all(list(mean = mean, min = min, max = max, sd = sd), na.rm = TRUE)

#Calculate Sample Size (Removing NA's)
AgeGroup_0_to_11 %>% 
  select(eosinophils) %>%
  filter(!is.na(eosinophils)) %>%
  summarise(n = n())


#Weighted Summary Stats
weighted.mean(AgeGroup_0_to_11$eosinophils, AgeGroup_0_to_11$lab_wts, na.rm=TRUE)
weighted.median(AgeGroup_0_to_11$eosinophils, AgeGroup_0_to_11$lab_wts, na.rm=TRUE)
sqrt(wtd.var(AgeGroup_0_to_11$eosinophils, AgeGroup_0_to_11$lab_wts, na.rm=TRUE))

#Distribution
weighted_cumsum_AgeGroup_0_to_11 <- function(a){AgeGroup_0_to_11 %>% 
    filter( eosinophils <= a ) %>%
    summarise( weighted_sum = sum(lab_wts)/sum(AgeGroup_0_to_11$lab_wts[AgeGroup_0_to_11$eosinophils>=0 & AgeGroup_0_to_11$eosinophils <= 10000], na.rm=TRUE) )
}

weighted_cumsum_AgeGroup_0_to_11(0)
weighted_cumsum_AgeGroup_0_to_11(100)
weighted_cumsum_AgeGroup_0_to_11(200)
weighted_cumsum_AgeGroup_0_to_11(300)
weighted_cumsum_AgeGroup_0_to_11(400)
weighted_cumsum_AgeGroup_0_to_11(500)
weighted_cumsum_AgeGroup_0_to_11(600)
weighted_cumsum_AgeGroup_0_to_11(700)
weighted_cumsum_AgeGroup_0_to_11(800)
weighted_cumsum_AgeGroup_0_to_11(900)
weighted_cumsum_AgeGroup_0_to_11(1000)


#####Distribution of 12-17 Age Group

#Weighted Distribution
Eos_Dist_12_to_17_years <- AgeGroup_12_to_17 %>% ggplot(aes(x=hist_eosins)) +
  geom_area(bins=100, aes(y = ..density.., weight =lab_wts), stat = "bin",
            color = "black", 
            fill = "steelblue") +
  labs(#title= "Blood Eosinophil Count Distribution (cells/uL)", 
    #subtitle="Ages 0-11",
    caption= "2000-2018 NHANES Cycles",
    x="Eosinophil Count", 
    y="Count")
Eos_Dist_12_to_17_years

#Summary Stats (Make Weighted)
AgeGroup_12_to_17 %>% 
  select(eosinophils) %>% 
  filter(!is.na(eosinophils)) %>%
  summarise_all(list(mean = mean, min = min, max = max, sd = sd), na.rm = TRUE)

#Calculate Sample Size (Removing NA's)
AgeGroup_12_to_17 %>% 
  select(eosinophils) %>%
  filter(!is.na(eosinophils)) %>%
  summarise(n = n())

#Weighted Summary Stats
weighted.mean(AgeGroup_12_to_17$eosinophils, AgeGroup_12_to_17$lab_wts, na.rm=TRUE)
weighted.median(AgeGroup_12_to_17$eosinophils, AgeGroup_12_to_17$lab_wts, na.rm=TRUE)
sqrt(wtd.var(AgeGroup_12_to_17$eosinophils, AgeGroup_12_to_17$lab_wts, na.rm=TRUE))

#Distribution
weighted_cumsum_AgeGroup_12_to_17 <- function(a){AgeGroup_12_to_17 %>% 
    filter( eosinophils <= a ) %>%
    summarise( weighted_sum = sum(lab_wts)/sum(AgeGroup_12_to_17$lab_wts[AgeGroup_12_to_17$eosinophils>=0 & AgeGroup_12_to_17$eosinophils <= 10000], na.rm=TRUE) )
}

weighted_cumsum_AgeGroup_12_to_17(0)
weighted_cumsum_AgeGroup_12_to_17(100)
weighted_cumsum_AgeGroup_12_to_17(200)
weighted_cumsum_AgeGroup_12_to_17(300)
weighted_cumsum_AgeGroup_12_to_17(400)
weighted_cumsum_AgeGroup_12_to_17(500)
weighted_cumsum_AgeGroup_12_to_17(600)
weighted_cumsum_AgeGroup_12_to_17(700)
weighted_cumsum_AgeGroup_12_to_17(800)
weighted_cumsum_AgeGroup_12_to_17(900)
weighted_cumsum_AgeGroup_12_to_17(1000)


#####Distribution of 18-64 Age Group

#Weighted Distribution
Eos_Dist_18_to_64_years <- AgeGroup_18_to_64 %>% ggplot(aes(x=hist_eosins)) +
  geom_area(bins=100, aes(y = ..density.., weight = lab_wts), stat = "bin",
            color = "black", 
            fill = "steelblue") +
  labs(#title= "Blood Eosinophil Count Distribution (cells/uL)", 
    #subtitle="Ages 0-11",
    caption= "2000-2018 NHANES Cycles",
    x="Eosinophil Count", 
    y="Count")
Eos_Dist_18_to_64_years

#Summary Stats (Make Weighted)
AgeGroup_18_to_64 %>% 
  select(eosinophils) %>% 
  summarise_all(list(mean = mean, min = min, max = max, sd = sd), na.rm = TRUE)

#Calculate Sample Size (Removing NA's)
AgeGroup_18_to_64 %>% 
  select(eosinophils) %>%
  filter(!is.na(eosinophils)) %>%
  summarise(n = n())

#Weighted Summary Stats
weighted.mean(AgeGroup_18_to_64$eosinophils, AgeGroup_18_to_64$lab_wts, na.rm=TRUE)
weighted.median(AgeGroup_18_to_64$eosinophils, AgeGroup_18_to_64$lab_wts, na.rm=TRUE)
sqrt(wtd.var(AgeGroup_18_to_64$eosinophils, AgeGroup_18_to_64$lab_wts, na.rm=TRUE))


#Distribution
weighted_cumsum_AgeGroup_18_to_64 <- function(a){AgeGroup_18_to_64 %>% 
    filter( eosinophils <= a ) %>%
    summarise( weighted_sum = sum(lab_wts)/sum(AgeGroup_18_to_64$lab_wts[AgeGroup_18_to_64$eosinophils>=0 & AgeGroup_18_to_64$eosinophils <= 10000], na.rm=TRUE) )
}

weighted_cumsum_AgeGroup_18_to_64(0)
weighted_cumsum_AgeGroup_18_to_64(100)
weighted_cumsum_AgeGroup_18_to_64(200)
weighted_cumsum_AgeGroup_18_to_64(300)
weighted_cumsum_AgeGroup_18_to_64(400)
weighted_cumsum_AgeGroup_18_to_64(500)
weighted_cumsum_AgeGroup_18_to_64(600)
weighted_cumsum_AgeGroup_18_to_64(700)
weighted_cumsum_AgeGroup_18_to_64(800)
weighted_cumsum_AgeGroup_18_to_64(900)
weighted_cumsum_AgeGroup_18_to_64(1000)


#####Distribution of 65+ Age Group

#Weighted Distribution
Eos_Dist_65_plus_years <- AgeGroup_65_plus %>% ggplot(aes(x=hist_eosins)) +
  geom_area(bins=0, aes(y = ..density.., weight = lab_wts), stat = "bin",
            color = "black", 
            fill = "steelblue") +
  labs(#title= "Blood Eosinophil Count Distribution (cells/uL)", 
    #subtitle="Ages 0-11",
    caption= "2000-2018 NHANES Cycles",
    x="Eosinophil Count", 
    y="Count")
Eos_Dist_65_plus_years

#Summary Stats (Make Weighted)
AgeGroup_65_plus %>% 
  select(eosinophils) %>% 
  summarise_all(list(mean = mean, min = min, max = max, sd = sd), na.rm = TRUE)

#Calculate Sample Size (Removing NA's)
AgeGroup_65_plus %>% 
  select(eosinophils) %>%
  filter(!is.na(eosinophils)) %>%
  summarise(n = n())

#Weighted Summary Stats
weighted.mean(AgeGroup_65_plus$eosinophils, AgeGroup_65_plus$lab_wts, na.rm=TRUE)
weighted.median(AgeGroup_65_plus$eosinophils, AgeGroup_65_plus$lab_wts, na.rm=TRUE)
sqrt(wtd.var(AgeGroup_65_plus$eosinophils, AgeGroup_65_plus$lab_wts, na.rm=TRUE))

#Distribution
weighted_cumsum_AgeGroup_65_plus <- function(a){AgeGroup_65_plus %>% 
    filter( eosinophils <= a ) %>%
    summarise( weighted_sum = sum(lab_wts)/sum(AgeGroup_65_plus$lab_wts[AgeGroup_65_plus$eosinophils>=0 & AgeGroup_65_plus$eosinophils <= 10000], na.rm=TRUE) )
}

weighted_cumsum_AgeGroup_65_plus(0)
weighted_cumsum_AgeGroup_65_plus(100)
weighted_cumsum_AgeGroup_65_plus(200)
weighted_cumsum_AgeGroup_65_plus(300)
weighted_cumsum_AgeGroup_65_plus(400)
weighted_cumsum_AgeGroup_65_plus(500)
weighted_cumsum_AgeGroup_65_plus(600)
weighted_cumsum_AgeGroup_65_plus(700)
weighted_cumsum_AgeGroup_65_plus(800)
weighted_cumsum_AgeGroup_65_plus(900)
weighted_cumsum_AgeGroup_65_plus(1000)




#####Overlay Distribution

#Weighted Distribution
Overlay_Distribution <- AgeGroup_0_to_11 %>% ggplot(aes(x=hist_eosins)) +
  geom_area(bins = 100, 
            aes(y = ..density.., weight = lab_wts), 
            stat = "bin",
            color = "blue",
            fill = NA) +
  geom_area(data = AgeGroup_12_to_17, 
            bins = 100,
            aes(y = ..density.., weight = lab_wts), 
            stat = "bin", 
            color = "black",
            fill = NA) +
  geom_area(data = AgeGroup_18_to_64, 
            bins = 100,
            aes(y = ..density.., weight = lab_wts), 
            stat = "bin", 
            color = "red",
            fill = NA) +
  geom_area(data = AgeGroup_65_plus, 
            bins = 100,
            aes(y = ..density.., weight = lab_wts), 
            stat = "bin", 
            color = "green",
            fill = NA) +
  labs(#title= "Blood Eosinophil Count Distribution (cells/uL)", 
    caption= "2000-2018 NHANES Cycles",
    x="Eosinophil Count", 
    y="Count")

#Type 2
Overlay_Distribution2 <- AgeGroup_0_to_11 %>% 
  ggplot(aes(x=hist_eosins, fill = age_group)) +
  geom_area(bins = 100, 
            aes(y = ..density.., color = age_group, weight = lab_wts), 
            stat = "bin",
            color = "blue",
            fill = NA) +
  labs(#title= "Blood Eosinophil Count Distribution (cells/uL)", 
    caption= "2000-2018 NHANES Cycles",
    x="Eosinophil Count", 
    y="Count") 
Overlay_Distribution2


#Type 3
Overlay_Distribution3 <- nhanes_00_18 %>% 
  ggplot(aes(x=hist_eosins, fill = age_group, color = age_group)) +
  geom_line(bins = 100, 
            aes(y = ..density.., color = age_group, weight = lab_wts), 
            stat = "bin") +
  labs(#title= "Blood Eosinophil Count Distribution (cells/uL)", 
    caption= "2000-2018 NHANES Cycles",
    x="Eosinophil Count", 
    y="Count",
    color = "Age Group")
Overlay_Distribution3

