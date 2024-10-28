tse %>%
  tse_meta(rownames = FALSE) %>%
  dplyr::mutate(INCIDENT_HTN = factor(ifelse(INCIDENT_HTN == 1, "Incident HTN" , "Without HTN"))) %>%
  mytableone(vars,fo =  ~ .| INCIDENT_HTN ) %>%
  write.csv("tableOneHTN.csv")
tse %>%
  tse_meta(rownames = FALSE) %>% 
  survival::coxph(my_surv(HTN_AGEDIFF, INCIDENT_HTN) ~
                    scale(shannon) + BL_AGE + MEN + BMI + PREVAL_DIAB.col_from_pheno + HFC +
                    PREVAL_HFAIL_STRICT.col_from_pheno + CURR_SMOKE, data = .) %>% 
  broom::tidy() %>%
  round_numeric_columns() %>% 
  write.table("alphaCoxHTN-full.csv")
rda_info %>%
  knitr::kable() %>%
  write.csv("permanova-full.csv")

#RDA plot
beta2 <-readRDS("beta-fullHTN.rds")

r <- miaViz::plotRDA(beta2,
                  "RDA",
                  add.vectors = FALSE,
                  add.ellipse = "colour",
                  colour_by = "INCIDENT_HTN") +
    ggplot2::scale_colour_manual(name = "Incident hypertension",
                                 labels = c("0" = "Normotensive",
                                            "1" = "Hypertensive"),
                                 values = c("0" = "darkslategrey",
                                            "1" = "orange")) +
  ggplot2::theme_classic() 

r + ggplot2::theme(
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 14),
  axis.text = element_text(size = 14),
  axis.title = element_text(size = 16))

##ANCOM-BC2 table
df %>%
  round_numeric_columns() %>% 
  write.csv("ancombc2-htn.csv")

#missing data
# Characteristics
```{r}
tse %>%
  tse_meta(rownames = FALSE) %>%
  mytableone(vars)
```
