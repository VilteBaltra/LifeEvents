
######################################
###             SET UP             ###
######################################
# load libraries
require(nnet)
library(mice)
library(openxlsx)

# define data path
data_path =  "path to data"

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

# half SD - low sensitivity as reference
set.seed(2023)
multinom_model <- with(full_imp, multinom(relevel(stress_group_halfsd, ref=1) ~ scale(PRS) + smfq_age_16y + sex + ethnicity + mum_uni, na.action=na.exclude))
out1 <- format.multinomial(multinom_model)

# half SD - typical sensitivity as reference
set.seed(2023)
multinom_model <- with(full_imp, multinom(relevel(stress_group_halfsd, ref=2) ~ scale(PRS) + smfq_age_16y + sex + ethnicity + mum_uni, na.action=na.exclude))
out2 <- format.multinomial(multinom_model)

# one SD - low sensitivity as reference
set.seed(2023)
multinom_model2 <- with(full_imp, multinom(relevel(stress_group_onesd, ref=1) ~ scale(PRS) + smfq_age_16y + sex + ethnicity + mum_uni, na.action=na.exclude))
out3 <- format.multinomial(multinom_model2)

# one SD - typical sensitivity as reference
set.seed(2023)
multinom_model2 <- with(full_imp, multinom(relevel(stress_group_onesd, ref=2) ~ scale(PRS) + smfq_age_16y + sex + ethnicity + mum_uni, na.action=na.exclude))
out4 <- format.multinomial(multinom_model2)

# save output
openxlsx::write.xlsx(list('halfsd1' = out1, 'halfsd2' = out2, 'onesd1' = out3, 'onesd2' = out4), file = paste0("multinomial-results-prs-stress-groups-", Sys.Date(),'.xlsx'), rowNames = F, overwrite=T)


############## end script #################




