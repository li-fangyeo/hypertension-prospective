#Alpha forest plot 
library(ggplot2)
library(survminer)
library(survival)
library(tibble)
library(mia)
library(survivalAnalysis)
#Change into dataframe and format Incident into integer, otherwise error
a <- as.data.frame(colData(tse))
a$INCIDENT_CKD <- as.integer(a$INCIDENT_CKD)
#Change the row names
a <- a %>% dplyr::mutate_at(vars(BMI, BL_AGE, SYSTM), ~./10) %>%
  dplyr::rename("Age" = BL_AGE ,
              "Sex"= MEN ,
              BMI = "BMI",
              "Diabetes" = PREVAL_DIAB.col_from_endpoints,
              "Systolic blood pressure" = SYSTM,
              "Antihypertensive medication" = BP_TREAT ,
              "Heart failure" = PREVAL_HFAIL_STRICT.col_from_endpoints,
              "Prevalent CKD" = PREVAL_CKD,
              "Incident CKD" = INCIDENT_CKD,
              "CKD time (yrs)" = CKD_AGEDIFF ,
              "Smoker" = CURR_SMOKE,
              "Eastern Finland" = EAST,
              "Shannon diversity" = shannon,
              "Autoimmune Disease" = PREVAL_AUTOIMMUN.col_from_endpoints)
             

b<- coxph(Surv(`CKD time (yrs)`, `Incident CKD`) ~
            `Shannon diversity` + Age + Sex + BMI +
            Diabetes + `Systolic blood pressure` + `Antihypertensive medication` +
            `Heart failure` + Smoker +
            `Autoimmune Disease`, data = a)
ggforest(b) + theme_classic()
