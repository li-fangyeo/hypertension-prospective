
library(dplyr)
a <- as.data.frame(colData(tse))
str(a)
head(a)

#Test code
calculate_beta_diversity <- function(tse) {
  mia::mergeFeaturesByRank(tse, rank = "Species") %>% 
    mia::transformAssay(method = "relabundance") %>%
    dplyr::sample_n(100, replace = FALSE) }

library(mia)
#use adonis
############ USE TSE ################
test<-tse_species[,tse_species$BL_AGE > 60]
test

# Agglomerate data to Species level
test <- agglomerateByRank(test,
                           rank = "Family")
library(vegan)
library(mia)
# Set seed for reproducibility
set.seed(1576)
# Perform dbRDA
##This dbrda needs to be re-run!! It is only 99 permutations
dbrda <- dbrda(t(assay(test,"relabundance")) ~ BL_AGE + MEN +INCIDENT_HTN, 
               data = colData(test))

# Perform permutational analysis
#run this at night
permanova_HTN <- anova.cca(dbrda,
                        by = "margin", # each term (here only 'Group') analyzed individually
                        method = "bray",
                        permutations = 99,
                        parallel = 8)

# Get p-values
p_values <- c(permanova_HTN["INCIDENT_HTN", "Pr(>F)"])
p_values <-as.data.frame(p_values)
rownames(p_values) <- "dbRDA+anova.cca"
p_values

# Add taxa info
sppscores(dbrda) <- t(assay(test, "relabundance"))
# Get coefficients
coef <- dbrda$CCA$v
# Get the taxa with biggest weights
top.coef <- head(coef[rev(order(abs(coef))) ,, drop = TRUE], 20)
# Sort weights in increasing order
top.coef <- top.coef[order(top.coef), ]
# Get top names
top_names <- names(top.coef)[order(abs(top.coef), decreasing = TRUE)]
df <- data.frame(x = top.coef,
                 y = factor(names(top.coef), unique(names(top.coef))))
library(ggplot2)
ggplot(df, aes(x = x, y = y)) +
  geom_bar(stat = "identity") +
  labs(x = "", y= "", title = "Top Taxa") +
  theme_bw()

calculate_beta_diversity <- function(tse) {
  mia::mergeFeaturesByRank(tse, rank = "Species") %>% 
    mia::transformAssay(method = "relabundance") %>%
    mia::runRDA(assay.type = "relabundance",
                formula = assay ~ BL_AGE + MEN + BMI + PREVAL_DIAB.col_from_pheno + HFC +
                  PREVAL_HFAIL_STRICT.col_from_pheno + CURR_SMOKE + INCIDENT_HTN,
                distance = "bray",
                na.action = na.exclude)
}
