##Table for top 10 species
#mini model
sp_tbl <- df_cox_results %>%
  dplyr::select(-results) %>% 
  tidyr::unnest(tidy) %>%
  dplyr::filter(stringr::str_detect(term, "taxon")) %>%
  dplyr::mutate(qval_fdr = p.adjust(p.value, method = "BH"))%>%
  dplyr::arrange(qval_fdr) %>%
  dplyr::slice(1:10) %>%
  dplyr::select(taxa,estimate,conf.low, conf.high, p.value, qval_fdr) %>%
  dplyr::mutate(across(everything(),~ gsub("GUT_","", .))) %>%
  dplyr::mutate_at(c(2:6), as.numeric) %>%
  dplyr::mutate_at(c(2:4),round,2) %>%
  dplyr::mutate_at(c(5:6), round, 3)


head(sp_tbl)

sp_tbl$`95% CI` <-  sprintf("%s - %s",
                                      sp_tbl$`conf.low`, sp_tbl$`conf.high`)
write.table(sp_tbl, file = "taxa-coxHTN-full.csv", sep=",", row.names=FALSE)

write.csv(sp_tbl$"../HypertensionMicrobiome/taxa-coxHTN-mini.csv", sp_tbl$tbl)
sp_tbl
