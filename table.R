##Table for top 10 species
#mini model
sp_tbl <- df_cox_results %>%
  dplyr::select(-results) %>% 
  tidyr::unnest(tidy) %>%
  dplyr::filter(stringr::str_detect(term, "taxon")) %>%
  dplyr::mutate(qval_fdr = p.adjust(p.value, method = "BH"))%>%
  dplyr::arrange(qval_fdr) %>%
  #dplyr::filter(qval_fdr < 0.05) %>%
  dplyr::slice(1:10) %>%
  dplyr::select(taxa,estimate,conf.low, conf.high, p.value, qval_fdr) %>%
  dplyr::mutate(across(everything(),~ gsub("GUT_Family:","", .))) %>%
  dplyr::mutate_at(c(2:6), as.numeric) %>%
  dplyr::mutate_at(c(2:4),round,2) %>%
  dplyr::mutate_at(c(5:6), round, 3)


head(sp_tbl)

sp_tbl$`95% CI` <-  sprintf("%s - %s",
                                      sp_tbl$`conf.low`, sp_tbl$`conf.high`)
write.table(sp_tbl, file = "taxa-coxHTN-mini-G.csv", sep=",", row.names=FALSE)

#write.csv(sp_tbl$"../HypertensionMicrobiome/taxa-coxHTN-mini.csv", sp_tbl$tbl)
sp_tbl

#Function table
tbl <- try%>% 
  dplyr::filter(!is.na(results)) %>%
  dplyr::mutate(results = purrr::map(results, ~broom::tidy(.x, exponentiate = TRUE, conf.int = TRUE)))  %>%
  tidyr::unnest(results) %>%
  dplyr::filter(term == "term") %>%
  dplyr::mutate(qval_fdr = p.adjust(p.value, method = "BH")) %>%
  dplyr::arrange(p.value) %>%
  dplyr::mutate_if(is.numeric, ~round(.x, digits = 3)) %>%
  dplyr::slice(1:10) %>%
  dplyr::select(Pathway,estimate,conf.low, conf.high, p.value, qval_fdr) %>%
  dplyr::mutate_at(c(2:6), as.numeric) %>%
  dplyr::mutate_at(c(2:4),round,2) %>%
  dplyr::mutate_at(c(5:6), round, 2)
head(tbl)

tbl$`95% CI` <-  sprintf("%s - %s",
                            tbl$`conf.low`, tbl$`conf.high`)
write.table(tbl, file = "pathway-rank-mini.csv", sep=",", row.names=FALSE)
  