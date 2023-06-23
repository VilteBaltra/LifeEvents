
######################################
###             SET UP             ###
######################################
# load libraries
require(nnet)
library(mice)
library(openxlsx)

# define data path
data_path = "/Volumes/Files/Psychology/ResearchProjects/Ewalton/EarlyCause/WP4/LifeEvents/neuroticism-2023-03-30/"

# read in imputed data (obtained with '4.life-events-interaction.R' script)
full_imp <- readRDS(file.path(data_path,'LE_imputation_list_with_z_prs_stress_groups.rds'))

######################################
## MULTINOMIAL LOGISTIC REGRESSION  ##
######################################
# multinomial model (using positive residuals group as the reference level)
format.multinomial <- function(model){
  fit <- as.data.frame(summary(pool(model))) 
  # obtain SEs by 1.96×SE either side of the mean
  fit$OR <- round(exp(fit$estimate), 3)
  fit$'95% CI' <- paste0(round(exp(fit$estimate - 1.96 * fit$std.error), 3), ", ", round(exp(fit$estimate + 1.96 * fit$std.error), 3))
  fit$sign <- ifelse(fit$p.value < 0.05, "*", NA)
  output <- fit[, c('y.level', 'term', 'OR', '95% CI', 'p.value', 'sign')]
  return(output)
}

# half SD
set.seed(2023)
multinom_model <- with(full_imp, multinom(relevel(stress_group_halfsd, ref=3) ~ scale(PRS) + emot_symp_age_16y + sex + ethnicity + mum_uni))
out <- format.multinomial(multinom_model)
# one SD
set.seed(2023)
multinom_model2 <- with(full_imp, multinom(relevel(stress_group_onesd, ref=3) ~ scale(PRS) + emot_symp_age_16y + sex + ethnicity + mum_uni))
out2 <- format.multinomial(multinom_model2)

# save output
openxlsx::write.xlsx(list('halfsd' = out, 'onesd' = out2), file = paste0("multinomial-results-prs-stress-groups-", Sys.Date(),'.xlsx'), rowNames = F, overwrite=T)


############## end script #################




