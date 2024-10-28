#Alpha forest plot 
library(ggplot2)
library(survminer)
library(survival)
library(tibble)
library(mia)
library(survivalAnalysis)
#Change into dataframe and format Incident into integer, otherwise error
a <- as.data.frame(colData(tse))
a$INCIDENT_HTN <- as.integer(a$INCIDENT_HTN)
##As reference to the original function
#survival::coxph(my_surv(HTN_AGEDIFF, INCIDENT_HTN) ~
                  #scale(shannon) + BL_AGE + MEN + BMI + PREVAL_DIAB.col_from_pheno + HFC +
                  #PREVAL_HFAIL_STRICT.col_from_pheno + CURR_SMOKE, data = .) 

#Change the row names
a <- a %>% dplyr::mutate_at(vars(BMI, BL_AGE, SYSTM), ~./10) %>%
  dplyr::rename("Age" = BL_AGE ,
                "Sex"= MEN ,
                BMI = "BMI",
                "Diabetes" = PREVAL_DIAB.col_from_pheno,
                "Systolic blood pressure" = SYSTM,
                "Diastolic blood pressure" = DIASM,
                "Healthy Food Choices" = HFC,
                "Antihypertensive medication" = BP_TREAT ,
                "Heart failure" = PREVAL_HFAIL_STRICT.col_from_pheno,
                "Prevalent HTN" = PREVAL_HTN,
                "Incident HTN" = INCIDENT_HTN,
                "Hypertension time (yrs)" = HTN_AGEDIFF ,
                "Smoker" = CURR_SMOKE,
                "Eastern Finland" = EAST,
                "Shannon diversity" = shannon)

b<- coxph(Surv(`Hypertension time (yrs)`, `Incident HTN`) ~
            `Shannon diversity` + Age + Sex,
            data = a)
ggforest(b) + theme_classic()

#Cox for incident htn vs taxa
bx <- df_cox_results[[3]][[81]]
summary(bx)

hs <- df_cox_results[[3]][[213]]
summary(hs)

#PERMANOVA table
rda_info %>%
knitr::kable() %>%
write.csv("permanova-full.csv")

##calculate p-value from tableone
library(tableone)
CreateTableOne(data = a, strata = "INCIDENT_HTN")
