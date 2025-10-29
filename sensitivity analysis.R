library(tidyverse)
#Excluded vs included samples baseline characteristics
tse <- readRDS("../../data/tse_mgs-20241118_104759.rds") %>%
  mia::transformAssay(assay.type = "counts", method = "relabundance") %>% 
  mia::addAlpha(assay.type = "counts", index = c("shannon", "observed"), name = c("shannon", "observed")) %>%
  tse_add_food_score(HFC) %>%
  tse_mutate(PREVAL_HTN = ifelse(PREVAL_HIBP + PREVAL_RX_RR_SPECIFIC > 0, 1, 0)) %>%
  tse_mutate(INCIDENT_HTN = ifelse(INCIDENT_HIBP + INCIDENT_RX_RR_SPECIFIC > 0, 1, 0)) %>%
  tse_mutate(HTN_AGEDIFF = pmin(HIBP_AGEDIFF, RX_RR_SPECIFIC_AGEDIFF)) %>% 
  tse_mutate(dplyr::across(c(MEN,
                             EAST,
                             BP_TREAT,
                             CURR_SMOKE,
                             dplyr::contains("INCIDENT"),
                             dplyr::contains("PREVAL")), as.factor)) %>%
  tse_filter(GRAVID %in% c(1, NA), BL_USE_RX_J01_1mo %in% c(0, NA)) %>% 
  tse_filter(SYSTM < 140, DIASM < 90, PREVAL_HTN == 0, BP_TREAT == 0) %>%
  tse_filter(dplyr::if_all(dplyr::one_of(names(vars)), not_na)) %>%
  tse_filter(total_reads > 50000) %>%
  tse_select(PREVAL_HTN, INCIDENT_HTN, HTN_AGEDIFF, names(vars))

tse_full <- readRDS("../../data/tse_mgs-20241118_104759.rds") %>%
  mia::transformAssay(assay.type = "counts", method = "relabundance") %>% 
  mia::addAlpha(assay.type = "counts", index = c("shannon", "observed"), name = c("shannon", "observed")) %>%
  tse_add_food_score(HFC) %>%
  tse_mutate(PREVAL_HTN = ifelse(PREVAL_HIBP + PREVAL_RX_RR_SPECIFIC > 0, 1, 0)) %>%
  tse_mutate(INCIDENT_HTN = ifelse(INCIDENT_HIBP + INCIDENT_RX_RR_SPECIFIC > 0, 1, 0)) %>%
  tse_mutate(HTN_AGEDIFF = pmin(HIBP_AGEDIFF, RX_RR_SPECIFIC_AGEDIFF)) %>% 
  tse_mutate(dplyr::across(c(MEN,
                             EAST,
                             BP_TREAT,
                             CURR_SMOKE,
                             dplyr::contains("INCIDENT"),
                             dplyr::contains("PREVAL")), as.factor)) %>%
  tse_filter(SYSTM < 140, DIASM < 90, PREVAL_HTN == 0, BP_TREAT == 0) %>%
  tse_filter(GRAVID %in% c(1, NA), BL_USE_RX_J01_1mo %in% c(0, NA)) %>% 
  tse_select(PREVAL_HTN, INCIDENT_HTN, HTN_AGEDIFF, names(vars))
  

#Prepare dataframe
df <- colData(tse) %>% as.data.frame()
df <- df %>% rownames_to_column("SampleID")
df_full <- colData(tse_full) %>% as.data.frame()
df_full <- df_full %>% rownames_to_column("SampleID")

df_excluded <- df_full %>% dplyr::anti_join(df, join_by(SampleID == SampleID))
df_excluded <- df_excluded %>% mutate(Exclude = 1)
df <- df %>% mutate(Exclude = 0)

#recombine all
df_test <- rbind(df, df_excluded)
df_test <- df_test %>% 
  column_to_rownames(var="SampleID")
  
#Reviewer three
#Comment 4
library(tableone)
strata <- CreateTableOne(data = df_test,
                         vars = c("BL_AGE", "HTN_AGEDIFF", "BMI", "SYSTM", "DIASM", "HFC", "shannon", "RX_RR_SPECIFIC_AGEDIFF", "observed",
                                  "total_reads"), 
                         factorVars = c("MEN","PREVAL_HTN", "INCIDENT_HTN", "PREVAL_DIAB", "BP_TREAT", "PREVAL_HIBP", "INCIDENT_HIBP", "PREVAL_RX_RR_SPECIFIC",
                                        "INCIDENT_RX_RR_SPECIFIC", "CURR_SMOKE", "EAST", "PREVAL_CVD"), 
                         strata = "Exclude"
)
tab_csv <- print(strata)

write.csv(tab_csv, "Exclude_characteristics.csv")

#Comment 7
df <- colData(tse) %>% as.data.frame()

strata <- CreateTableOne(data = df,
                         vars = c("BL_AGE", "HTN_AGEDIFF", "BMI", "SYSTM", "DIASM", "HFC", "shannon", "RX_RR_SPECIFIC_AGEDIFF", "observed",
                                  "total_reads"), 
                         factorVars = c("MEN","PREVAL_HTN",  "PREVAL_DIAB", "BP_TREAT", "PREVAL_HIBP", "INCIDENT_HIBP", "PREVAL_RX_RR_SPECIFIC",
                                        "INCIDENT_RX_RR_SPECIFIC", "CURR_SMOKE", "EAST", "PREVAL_CVD"), 
                         strata = "INCIDENT_HTN"
)
tab_csv <- print(strata, showAllLevels = TRUE, smd = TRUE)
write.csv(tab_csv, "TableOne.csv")

#Reviewer 4, Comment 5 - 95% CI
#calculated manually using online calculator

#Reviewer 1, comment 1 - power calculations 
library(devtools)
devtools::install_github("LucyMcGowan/survivalpwr")
library(survivalpwr)

#Shannon
pwr_coxph(
  hr = 1.294,
  eventprob = 0.2038659,
  stddev = 0.42,
  n = 3311)

#Systolic blood pressure
pwr_coxph(
  hr = 1.011,
  eventprob = 0.2038659,
  stddev = 9.9,
  n = 3311)

##Reviewer 4, comment 8
library(StepReg)

formula = my_surv(HTN_AGEDIFF, INCIDENT_HTN) ~
   shannon + BL_AGE + MEN + BMI + PREVAL_DIAB + HFC + PREVAL_CVD + CURR_SMOKE

step <- tse %>%
  tse_meta(rownames = FALSE) %>% 
  StepReg::stepwise(formula = formula, data = . , type = "cox", strategy = "bidirection", metric = c("AIC"), sle = 0.05, sls = 0.05, 
                    test_method_cox = "breslow") 

plot(step, strategy = "bidirection", process = "details")
