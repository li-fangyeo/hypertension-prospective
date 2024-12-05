#PERMANOVA table
rda_info %>%
knitr::kable() %>%
write.csv("permanova-full.csv")

##calculate p-value from tableone
library(tableone)
CreateTableOne(data = a, strata = "INCIDENT_CKD")


##12072024
#forest plot
library(readr)
library(grid)
library(forestploter)
library(gridExtra)
alpha <- read_csv("cox-HTN.csv")
#alpha$'' <- cut(alpha$'P', breaks=c(-Inf, 0.001, 0.01, 0.05, Inf), label=c("***", "**", "*", "")) 
alpha
alpha$` ` <- paste(rep(" ", 20), collapse = " ")

# Create a confidence interval column to display
alpha$`HR (95% CI)` <- ifelse(is.na(alpha$std.error), "",
                                     sprintf("%.2f (%.2f to %.2f)",
                                             alpha$estimate, alpha$conf.low, alpha$conf.high))
alpha

#make the words center
tm <- forest_theme(base_size = 15,
                   rowhead=list(fg_params=list(hjust=0, x=0)),
                   colhead=list(fg_params=list(hjust=0.5, x=0.5)),
                  #core=list(fg_params=list(hjust=0.5, x=0.5)),
                   ci_Theight = 0.3,
                   ci_lwd = 2)

alpha_ft <- forest(alpha[,c(2, 10, 9, 6)],
                est = alpha$estimate,
                lower = alpha$conf.low, 
                upper = alpha$conf.high,
                #sizes = alpha$se,
                ci_column = 3,
                ref_line = 1,
                xlim = c(0, 3),
                x_trans = c("log10"),
                xlab = "Hazard ratio",
                theme = tm)

# Print plot
plot(alpha_ft)

# Save plots to PDF, specifying dimensions
pdf("coxHTN.pdf", width = 9, height = 6)
print(alpha_ft)
dev.off()

