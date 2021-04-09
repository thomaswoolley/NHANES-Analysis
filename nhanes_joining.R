rm(list=ls())
setwd("~/Desktop/health_econ/nhanes")
library(tidyverse)
library(openxlsx)
library(haven)
library(dplyr)
library(Hmisc)
library(knitr)
library(rio)

#Demographics Survey Data

#2000 Cycle
data_DEMO <- sasxport.get("DEMO.XPT") %>% 
   select(seqn, riagendr, ridageyr, ridreth1, wtint2yr, wtmec2yr, wtmec4yr) %>%
   mutate(ridageyr = ifelse(ridageyr >= 80, 80, ridageyr))

data_MCQ <- sasxport.get("MCQ.XPT") %>%
   select(seqn, mcq010, mcq030, mcq040, mcq050, mcq080, mcq250c) %>% 
   rename(mcq300b = mcq250c,
          mcq035 = mcq030) %>%
   mutate(mcq025 = NA)

data_HSQ <- sasxport.get("HSQ.XPT") %>% 
   select(seqn, hsq510) %>%
   mutate(hsd010 = NA)

data_CDQ <- sasxport.get("CDQ.XPT") %>% 
   select(seqn) %>%
   mutate(cdq009h = NA)

data_BMX <- sasxport.get("BMX.XPT") %>%
   select(seqn, bmxwt, bmxht, bmxbmi, bmxrecum)

data_CBC <- sasxport.get("LAB25.XPT") %>%
   select(seqn, lbdeono, lbxeopct, lbxwbcsi)

   
data_PFQ <- sasxport.get("PFQ.XPT") %>%
   select(seqn, pfq030, pfq048, pfq050, pfq060g, pfq060r) %>%
   rename(pfq049 = pfq048,
          pfq051 = pfq050,
          pfq061g = pfq060g,
          pfq061r = pfq060r)

data_HUQ <- sasxport.get("HUQ.XPT") %>%
   select(seqn, huq010, huq070) %>%
   rename(huq071 = huq070)



merged00 <- left_join(data_DEMO, data_MCQ, by = "seqn")
merged00 <- left_join(merged00, data_HSQ, by = "seqn")
merged00 <- left_join(merged00, data_CDQ, by = "seqn")
merged00 <- left_join(merged00, data_CBC, by = "seqn")
merged00 <- left_join(merged00, data_BMX, by = "seqn")
merged00 <- left_join(merged00, data_PFQ, by = "seqn")
merged00 <- left_join(merged00, data_HUQ, by = "seqn")

rm(data_CBC, 
   data_CDQ, 
   data_DEMO, 
   data_HSQ, 
   data_MCQ,
   data_BMX,
   data_PFQ,
   data_HUQ
)

merged00 <- merged00 %>% 
   mutate(cycle = "00")


#2002 Cycle
data_DEMO <- sasxport.get("DEMO_B.XPT") %>%
   select(seqn, riagendr, ridageyr, ridreth1, wtint2yr, wtmec2yr, wtmec4yr) %>%
   mutate(ridageyr = ifelse(ridageyr >= 80, 80, ridageyr))

data_MCQ <- sasxport.get("MCQ_B.XPT") %>%
   select(seqn, mcq010, mcq025, mcq035, mcq040, mcq050, mcq080, mcq250c) %>%
   rename(mcq300b = mcq250c) %>%
   mutate(mcq025 = ifelse( mcq025 >= 80 & mcq025 <=85, 80, mcq025))

data_HSQ <- sasxport.get("HSQ_B.XPT") %>%
   select(seqn, hsd010, hsq510)

data_CDQ <- sasxport.get("CDQ_B.XPT") %>% 
   select(seqn, cdq009h)

data_BMX <- sasxport.get("BMX_B.XPT") %>%
   select(seqn, bmxwt, bmxht, bmxbmi, bmxrecum)

data_CBC <- sasxport.get("L25_B.XPT") %>%
   select(seqn, lbdeono, lbxeopct, lbxwbcsi)

data_PFQ <- sasxport.get("PFQ_B.XPT") %>%
   select(seqn, pfq030, pfq048, pfq050, pfq060g, pfq060r) %>%
   rename(pfq049 = pfq048,
          pfq051 = pfq050,
          pfq061g = pfq060g,
          pfq061r = pfq060r)

data_HUQ <- sasxport.get("HUQ_B.XPT") %>%
   select(seqn, huq010, hud070) %>%
   rename(huq071 = hud070)

merged02 <- left_join(data_DEMO, data_MCQ, by = "seqn")
merged02 <- left_join(merged02, data_HSQ, by = "seqn")
merged02 <- left_join(merged02, data_CDQ, by = "seqn")
merged02 <- left_join(merged02, data_CBC, by = "seqn")
merged02 <- left_join(merged02, data_BMX, by = "seqn")
merged02 <- left_join(merged02, data_PFQ, by = "seqn")
merged02 <- left_join(merged02, data_HUQ, by = "seqn")

rm(data_CBC, 
   data_CDQ, 
   data_DEMO, 
   data_HSQ, 
   data_MCQ,
   data_BMX,
   data_PFQ,
   data_HUQ
)

merged02 <- merged02 %>% 
   mutate(cycle = "02")


#2004 Cycle
data_DEMO <- sasxport.get("DEMO_C.XPT") %>% 
   select(seqn, riagendr, ridageyr, ridreth1, wtint2yr, wtmec2yr) %>%
   mutate(ridageyr = ifelse(ridageyr >= 80, 80, ridageyr),
          wtmec4yr = NA)

data_MCQ <- sasxport.get("MCQ_C.XPT") %>% 
   select(seqn, mcq010, mcq025, mcq035, mcq040, mcq050, mcq080, mcq250c) %>% 
   rename(mcq300b = mcq250c) %>%
   mutate(mcq025 = ifelse( mcq025 >= 80 & mcq025 <=85, 80, mcq025))

data_HSQ <- sasxport.get("HSQ_C.XPT") %>%
   select(seqn, hsd010, hsq510)

data_CDQ <- sasxport.get("CDQ_C.XPT") %>%
   select(seqn, cdq009h)

data_BMX <- sasxport.get("BMX_C.XPT") %>%
   select(seqn, bmxwt, bmxht, bmxbmi, bmxrecum)

data_CBC <- sasxport.get("L25_C.XPT") %>%
   select(seqn, lbdeono, lbxeopct, lbxwbcsi)

data_PFQ <- sasxport.get("PFQ_C.XPT") %>%
   select(seqn, pfq030, pfq049, pfq051, pfq061g, pfq061r)

data_HUQ <- sasxport.get("HUQ_C.XPT") %>%
   select(seqn, huq010, huq071)

merged04 <- left_join(data_DEMO, data_MCQ, by = "seqn")
merged04 <- left_join(merged04, data_HSQ, by = "seqn")
merged04 <- left_join(merged04, data_CDQ, by = "seqn")
merged04 <- left_join(merged04, data_CBC, by = "seqn")
merged04 <- left_join(merged04, data_BMX, by = "seqn")
merged04 <- left_join(merged04, data_PFQ, by = "seqn")
merged04 <- left_join(merged04, data_HUQ, by = "seqn")

rm(data_CBC, 
   data_CDQ, 
   data_DEMO, 
   data_HSQ, 
   data_MCQ,
   data_BMX,
   data_PFQ,
   data_HUQ
)

merged04 <- merged04 %>% 
   mutate(cycle = "04")


#2006 Cycle
data_DEMO <- sasxport.get("DEMO_D.XPT") %>%
   select(seqn, riagendr, ridageyr, ridreth1, wtint2yr, wtmec2yr) %>%
   mutate(ridageyr = ifelse(ridageyr >= 80, 80, ridageyr),
          wtmec4yr = NA)

data_MCQ <- sasxport.get("MCQ_D.XPT") %>%
   select(seqn, mcq010, mcq025, mcq035, mcq040, mcq050, mcq080, mcq300b) %>%
   mutate(mcq025 = ifelse( mcq025 >= 80 & mcq025 <=85, 80, mcq025))

data_HSQ <- sasxport.get("HSQ_D.XPT") %>%
   select(seqn, hsd010, hsq510)

data_CDQ <- sasxport.get("CDQ_D.XPT") %>%
   select(seqn, cdq009h)

data_BMX <- sasxport.get("BMX_D.XPT") %>%
   select(seqn, bmxwt, bmxht, bmxbmi, bmxrecum)

data_CBC <- sasxport.get("CBC_D.XPT") %>%
   select(seqn, lbdeono, lbxeopct, lbxwbcsi)

data_PFQ <- sasxport.get("PFQ_D.XPT") %>%
   select(seqn, pfq030, pfq049, pfq051, pfq061g, pfq061r)

data_HUQ <- sasxport.get("HUQ_D.XPT") %>%
   select(seqn, huq010, huq071)

merged06 <- left_join(data_DEMO, data_MCQ, by = "seqn")
merged06 <- left_join(merged06, data_HSQ, by = "seqn")
merged06 <- left_join(merged06, data_CDQ, by = "seqn")
merged06 <- left_join(merged06, data_CBC, by = "seqn")
merged06 <- left_join(merged06, data_BMX, by = "seqn")
merged06 <- left_join(merged06, data_PFQ, by = "seqn")
merged06 <- left_join(merged06, data_HUQ, by = "seqn")

rm(data_CBC, 
   data_CDQ, 
   data_DEMO, 
   data_HSQ, 
   data_MCQ,
   data_BMX,
   data_PFQ,
   data_HUQ
)

merged06 <- merged06 %>% 
   mutate(cycle = "06")


#2008 Cycle
data_DEMO <- sasxport.get("DEMO_E.XPT") %>%
   select(seqn, riagendr, ridageyr, ridreth1, wtint2yr, wtmec2yr) %>%
   mutate(wtmec4yr = NA)

data_MCQ <- sasxport.get("MCQ_E.XPT") %>%
   select(seqn, mcq010, mcq025, mcq035, mcq040, mcq050, mcq080, mcq300b)

data_HSQ <- sasxport.get("HSQ_E.XPT") %>%
   select(seqn, hsd010, hsq510)

data_CDQ <- sasxport.get("CDQ_E.XPT") %>%
   select(seqn, cdq009h)

data_BMX <- sasxport.get("BMX_E.XPT") %>%
   select(seqn, bmxwt, bmxht, bmxbmi, bmxrecum)

data_CBC <- sasxport.get("CBC_E.XPT") %>%
   select(seqn, lbdeono, lbxeopct, lbxwbcsi)

data_PFQ <- sasxport.get("PFQ_E.XPT") %>%
   select(seqn, pfq030, pfq049, pfq051, pfq061g, pfq061r)

data_HUQ <- sasxport.get("HUQ_E.XPT") %>%
   select(seqn, huq010, huq071)

merged08 <- left_join(data_DEMO, data_MCQ, by = "seqn")
merged08 <- left_join(merged08, data_HSQ, by = "seqn")
merged08 <- left_join(merged08, data_CDQ, by = "seqn")
merged08 <- left_join(merged08, data_CBC, by = "seqn")
merged08 <- left_join(merged08, data_BMX, by = "seqn")
merged08 <- left_join(merged08, data_PFQ, by = "seqn")
merged08 <- left_join(merged08, data_HUQ, by = "seqn")

rm(data_CBC, 
   data_CDQ, 
   data_DEMO, 
   data_HSQ, 
   data_MCQ,
   data_BMX,
   data_PFQ,
   data_HUQ
)

merged08 <- merged08 %>% 
   mutate(cycle = "08")

#2010 Cycle
data_DEMO <- sasxport.get("DEMO_F.XPT") %>%
   select(seqn, riagendr, ridageyr, ridreth1, wtint2yr, wtmec2yr) %>%
   mutate(wtmec4yr = NA)

data_MCQ <- sasxport.get("MCQ_F.XPT") %>%
   select(seqn, mcq010, mcq025, mcq035, mcq040, mcq050, mcq080, mcq300b)

data_HSQ <- sasxport.get("HSQ_F.XPT") %>%
   select(seqn, hsd010, hsq510)

data_CDQ <- sasxport.get("CDQ_F.XPT") %>%
   select(seqn, cdq009h)

data_BMX <- sasxport.get("BMX_F.XPT") %>%
   select(seqn, bmxwt, bmxht, bmxbmi, bmxrecum)

data_CBC <- sasxport.get("CBC_F.XPT") %>%
   select(seqn, lbdeono, lbxeopct, lbxwbcsi)

data_PFQ <- sasxport.get("PFQ_F.XPT") %>%
   select(seqn, pfq030, pfq049, pfq051, pfq061g, pfq061r)

data_HUQ <- sasxport.get("HUQ_F.XPT") %>%
   select(seqn, huq010, huq071)


merged10 <- left_join(data_DEMO, data_MCQ, by = "seqn")
merged10 <- left_join(merged10, data_HSQ, by = "seqn")
merged10 <- left_join(merged10, data_CDQ, by = "seqn")
merged10 <- left_join(merged10, data_CBC, by = "seqn")
merged10 <- left_join(merged10, data_BMX, by = "seqn")
merged10 <- left_join(merged10, data_PFQ, by = "seqn")
merged10 <- left_join(merged10, data_HUQ, by = "seqn")

rm(data_CBC, 
   data_CDQ, 
   data_DEMO, 
   data_HSQ, 
   data_MCQ,
   data_BMX,
   data_PFQ,
   data_HUQ
)

merged10 <- merged10 %>% 
   mutate(cycle = "10")

#2012 Cycle
data_DEMO <- sasxport.get("DEMO_G.XPT") %>%
   select(seqn, riagendr, ridageyr, ridreth1, wtint2yr, wtmec2yr) %>%
   mutate(wtmec4yr = NA)

data_MCQ <- sasxport.get("MCQ_G.XPT") %>% 
   select(seqn, mcq010, mcq025, mcq035, mcq040, mcq050, mcq080, mcq300b)

data_HSQ <- sasxport.get("HSQ_G.XPT") %>%
   select(seqn, hsd010, hsq510)

data_CDQ <- sasxport.get("CDQ_G.XPT") %>%
   select(seqn, cdq009h)

data_BMX <- sasxport.get("BMX_G.XPT") %>%
   select(seqn, bmxwt, bmxht, bmxbmi, bmxrecum)

data_CBC <- sasxport.get("CBC_G.XPT") %>%
   select(seqn, lbdeono, lbxeopct, lbxwbcsi)

data_PFQ <- sasxport.get("PFQ_G.XPT") %>%
   select(seqn, pfq030, pfq049, pfq051, pfq061g, pfq061r)

data_HUQ <- sasxport.get("HUQ_G.XPT") %>%
   select(seqn, huq010, huq071)

merged12 <- left_join(data_DEMO, data_MCQ, by = "seqn")
merged12 <- left_join(merged12, data_HSQ, by = "seqn")
merged12 <- left_join(merged12, data_CDQ, by = "seqn")
merged12 <- left_join(merged12, data_CBC, by = "seqn")
merged12 <- left_join(merged12, data_BMX, by = "seqn")
merged12 <- left_join(merged12, data_PFQ, by = "seqn")
merged12 <- left_join(merged12, data_HUQ, by = "seqn")

rm(data_CBC, 
   data_CDQ, 
   data_DEMO, 
   data_HSQ, 
   data_MCQ,
   data_BMX,
   data_PFQ,
   data_HUQ
)

merged12 <- merged12 %>% 
   mutate(cycle = "12")

#2014 Cycle
data_DEMO <- sasxport.get("DEMO_H.XPT") %>%
   select(seqn, riagendr, ridageyr, ridreth1, wtint2yr, wtmec2yr) %>%
   mutate(wtmec4yr = NA)

data_MCQ <- sasxport.get("MCQ_H.XPT") %>%
   select(seqn, mcq010, mcq025, mcq035, mcq040, mcq050, mcq080, mcq300b)

data_HSQ <- sasxport.get("HSQ_H.XPT") %>%
   select(seqn, hsd010, hsq510)

data_CDQ <- sasxport.get("CDQ_H.XPT") %>%
   select(seqn, cdq009h)

data_BMX <- sasxport.get("BMX_H.XPT") %>%
   select(seqn, bmxwt, bmxht, bmxbmi, bmxrecum)

data_CBC <- sasxport.get("CBC_H.XPT") %>%
   select(seqn, lbdeono, lbxeopct, lbxwbcsi)

data_PFQ <- sasxport.get("PFQ_H.XPT") %>%
   select(seqn, pfq030, pfq049, pfq051, pfq061g, pfq061r)

data_HUQ <- sasxport.get("HUQ_H.XPT") %>%
   select(seqn, huq010, huq071)

merged14 <- left_join(data_DEMO, data_MCQ, by = "seqn")
merged14 <- left_join(merged14, data_HSQ, by = "seqn")
merged14 <- left_join(merged14, data_CDQ, by = "seqn")
merged14 <- left_join(merged14, data_CBC, by = "seqn")
merged14 <- left_join(merged14, data_BMX, by = "seqn")
merged14 <- left_join(merged14, data_PFQ, by = "seqn")
merged14 <- left_join(merged14, data_HUQ, by = "seqn")

rm(data_CBC, 
   data_CDQ, 
   data_DEMO, 
   data_HSQ, 
   data_MCQ,
   data_BMX,
   data_PFQ,
   data_HUQ
)

merged14 <- merged14 %>% 
   mutate(cycle = "14")

#2016 Cycle
data_DEMO <- sasxport.get("DEMO_I.XPT") %>%
   select(seqn, riagendr, ridageyr, ridreth1, wtint2yr, wtmec2yr) %>%
   mutate(wtmec4yr = NA)

data_MCQ <- sasxport.get("MCQ_I.XPT") %>% 
   select(seqn, mcq010, mcq025, mcq035, mcq040, mcq050, mcq080, mcq300b)

data_HSQ <- sasxport.get("HSQ_I.XPT") %>%
   select(seqn, hsd010, hsq510)

data_CDQ <- sasxport.get("CDQ_I.XPT") %>%
   select(seqn, cdq009h)

data_BMX <- sasxport.get("BMX_I.XPT") %>%
   select(seqn, bmxwt, bmxht, bmxbmi, bmxrecum)

data_CBC <- sasxport.get("CBC_I.XPT") %>% 
   select(seqn, lbdeono, lbxeopct, lbxwbcsi)

data_PFQ <- sasxport.get("PFQ_I.XPT") %>%
   select(seqn, pfq030, pfq049, pfq051, pfq061g, pfq061r)

data_HUQ <- sasxport.get("HUQ_I.XPT") %>%
   select(seqn, huq010, huq071)


merged16 <- left_join(data_DEMO, data_MCQ, by = "seqn")
merged16 <- left_join(merged16, data_HSQ, by = "seqn")
merged16 <- left_join(merged16, data_CDQ, by = "seqn")
merged16 <- left_join(merged16, data_CBC, by = "seqn")
merged16 <- left_join(merged16, data_BMX, by = "seqn")
merged16 <- left_join(merged16, data_PFQ, by = "seqn")
merged16 <- left_join(merged16, data_HUQ, by = "seqn")


rm(data_CBC, 
   data_CDQ, 
   data_DEMO, 
   data_HSQ, 
   data_MCQ,
   data_BMX,
   data_PFQ,
   data_HUQ
)

merged16 <- merged16 %>% 
   mutate(cycle = "16")

#2018 Cycle
data_DEMO <- sasxport.get("DEMO_J.XPT") %>%
   select(seqn, riagendr, ridageyr, ridreth1, wtint2yr, wtmec2yr) %>%
   mutate(wtmec4yr = NA)

data_MCQ <- sasxport.get("MCQ_J.XPT") %>%
   select(seqn, mcq010, mcq025, mcq035, mcq040, mcq050, mcq080, mcq300b)

data_HSQ <- sasxport.get("HSQ_J.XPT") %>%
   select(seqn, hsd010, hsq510)

data_CDQ <- sasxport.get("CDQ_J.XPT") %>%
   select(seqn, cdq009h)

data_BMX <- sasxport.get("BMX_J.XPT") %>% 
   select(seqn, bmxwt, bmxht, bmxbmi, bmxrecum)

data_CBC <- sasxport.get("CBC_J.XPT") %>%
   select(seqn, lbdeono, lbxeopct, lbxwbcsi)

data_PFQ <- sasxport.get("PFQ_J.XPT") %>%
   select(seqn, pfq030, pfq049, pfq051, pfq061g, pfq061r)

data_HUQ <- sasxport.get("HUQ_J.XPT") %>%
   select(seqn, huq010, huq071)

merged18 <- left_join(data_DEMO, data_MCQ, by = "seqn")
merged18 <- left_join(merged18, data_HSQ, by = "seqn")
merged18 <- left_join(merged18, data_CDQ, by = "seqn")
merged18 <- left_join(merged18, data_CBC, by = "seqn")
merged18 <- left_join(merged18, data_BMX, by = "seqn")
merged18 <- left_join(merged18, data_PFQ, by = "seqn")
merged18 <- left_join(merged18, data_HUQ, by = "seqn")

rm(data_CBC, 
   data_CDQ, 
   data_DEMO, 
   data_HSQ, 
   data_MCQ,
   data_BMX,
   data_PFQ,
   data_HUQ
)

merged18 <- merged18 %>% 
   mutate(cycle = "18")

####Combine all Datasets
nhanes_00_18 <- rbind(merged00, 
                      merged02, 
                      merged04,
                      merged06,
                      merged08,
                      merged10,
                      merged12,
                      merged14,
                      merged16,
                      merged18)

nhanes_00_18 %>% write.csv('nhanes_00_18.csv', row.names= F)
