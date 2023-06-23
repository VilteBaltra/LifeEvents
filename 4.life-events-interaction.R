
######################################
###             SET UP             ###
######################################
# load libraries
library(mice)
library(tidyverse)
library(openxlsx)

# define data path
data_path = "/Volumes/Files/Psychology/ResearchProjects/Ewalton/EarlyCause/WP4/LifeEvents/neuroticism-2023-03-30/"

# read in imputed data (obtained with '3.imputed-data-regressions.R' script)
imp <- readRDS(file.path(data_path,'LE_imputation_list_with_z_prs.rds'))

######################################
###         OBTAIN RSIDUALS        ###
######################################

# convert mids object into list
datlist <- miceadds::mids2datlist(imp)

# obtain residuals and predicted values for each of the imputed datasets
for(i in 1:length(datlist)) {
  print(i)
  model <- lm(weighted_LE_sum ~ unweighted_LE_sum, data = datlist[[i]]) # specify model
  print(summary(model))
  set.seed(2023)
  datlist[[i]]$residuals <- residuals(model) # save the residual values
  datlist[[i]]$predicted <- predict(model) # save the predicted values
}

# define stress groups 
for(i in 1:length(datlist)) {
  # half SD = threshold
  datlist[[i]]$stress_group_halfsd <- as.factor(ifelse(datlist[[i]]$residuals > (sd(datlist[[i]]$residuals)/2), 1, # high emotional reactivity = 1
                                                        ifelse(datlist[[i]]$residuals < -(sd(datlist[[i]]$residuals)/2), -1,  # low emotional reactivity = -1
                                                               0)))
  # one SD = threshold
  datlist[[i]]$stress_group_onesd <- as.factor(ifelse(datlist[[i]]$residuals > sd(datlist[[i]]$residuals), 1, # high emotional reactivity = 1
                                                      ifelse(datlist[[i]]$residuals < -sd(datlist[[i]]$residuals), -1,  # low emotional reactivity = -1
                                                             0)))
}

# convert back to mids object 
full_imp <- miceadds::datlist2mids( datlist )

######################################
###   EXPLORE DATA & ASSUMPTIONS   ###
######################################

# correlate unweighted LE with weighted LE using all imputed datasets
with(full_imp, cor(weighted_LE_sum, unweighted_LE_sum)) # 0.93

# histograms (all are right skewed)
hist(full_imp$data$weighted_LE_sum) 
hist(full_imp$data$unweighted_LE_sum)
hist(full_imp$data$emot_symp_16y) 

#assumption checks
original <- complete(full_imp, 0) # select original dataset 
model <- lm(weighted_LE_sum~unweighted_LE_sum, data = original)
pdf("life-events-assumption-checks.pdf")
print(summary(model))
par(mfrow=c(3,2))
hist(model$residuals, main="Histogram of Residuals")
plot(model)
plot(model, 4)
mtext("Weighted LE regressed on unweighted LE", outer=TRUE,  cex=1, line=-1.4, font=2, col='blue')
dev.off()

######################################
###        PLOT RESIDUALS          ###
######################################

## Plot all imputed datasets ##
# define stress groups based on half SD residuals
pdf("unweighted-weighted-figure-all-imputed-datasets.pdf")
for(i in 1:length(datlist)) {
p <- ggplot(datlist[[i]], aes(x = unweighted_LE_sum, y = weighted_LE_sum)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = unweighted_LE_sum, yend = predicted), alpha = .2) +
  # color adjustments
  geom_point(aes(color = stress_group_halfsd), size = 2, shape = 20) +  # Color mapped here
  #scale_color_gradient2(low = '#009999', high = "#CC0066") + # "#d7191c"
  scale_color_brewer(palette="Set2") +
  guides(color = 'none') +
  
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()  + xlab("Unweighted life events score at 16 years (sum)") + ylab("Weighted life events score at 16 years (sum)")
print(p)
}
dev.off()

## Only plot 30th dataset for manuscript ##
dataset_30 <- complete(full_imp, 30)

p <- ggplot(dataset_30, aes(x = unweighted_LE_sum, y = weighted_LE_sum)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = unweighted_LE_sum, yend = predicted), alpha = .2) +
  # color adjustments
  geom_point(aes(color = stress_group_halfsd), size = 2, shape = 20) +  # Color mapped here
  #scale_color_gradient2(low = '#009999', high = "#CC0066") + # "#d7191c"
  scale_color_brewer(palette="Set2") +
  guides(color = 'none') +
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw() 
p + xlab("Unweighted life events score at 16 years (sum)") + ylab("Weighted life events score at 16 years (sum)")
# save
ggsave(paste0('residuals_plot_30th_imp_dataset_', Sys.Date(), ".png"), scale = 0.7)

# check number of individuals in each group
table(dataset_30$stress_group_halfsd) 
# -1    0    1 
# 1130 1632 1110 


####################################
##     REGR WITH INTERACTION      ## 
####################################

# stress group predicting emotional symptoms
model1 <- with(full_imp, lm(emot_symp_16y ~ stress_group_halfsd))
summary(pool(model1))
# unweighted LE predicting emotional symptoms
model2 <- with(full_imp, lm(emot_symp_16y ~ unweighted_LE_mean))
summary(pool(model2))

# function to model output and save as an excel file
my_table <- function(fit) {
  model <- summary(fit) # extract estimates
  model$sign <- ifelse(model$p.value < 0.05, '*', '') # add a column to highlight significant terms
  model$`95% CI` <- paste0(round((model$estimate - 1.96*model$std.error), 3), ", ", round((model$estimate + 1.96*model$std.error), 3))
  model$`r.squared` <- c(pool.r.squared(fit)[1], rep(NA, nrow(model)-1)) # add a column for R2
  model$`adj.r.squared` <- c(pool.r.squared(fit, adjusted = TRUE)[1], rep(NA, nrow(model)-1)) # adjusted R2
  return(model)
}

# INTERACTION WITHOUT COVARIATES: HALF SD
# stress group interaction with number of LE predicting emotional symptoms
model <- with(full_imp, lm(emot_symp_16y ~ relevel(stress_group_halfsd, ref = 3)*unweighted_LE_sum))
fit_pool <- mice::pool(model)
model_no_covars <- my_table(fit_pool) # apply formatting function

# INTERACTION WITHOUT COVARIATES: ONE SD
# stress group interaction with number of LE predicting emotional symptoms
model_onesd <- with(full_imp, lm(emot_symp_16y ~ relevel(stress_group_onesd, ref = 3)*unweighted_LE_sum))
fit_pool2 <- mice::pool(model_onesd)
model_data_onesd <- my_table(fit_pool2) # apply formatting function

# INTERACTION WITH COVARIATES: HAFL SD
# stress group predicting emotional symptoms + covariates
model_covars <- with(full_imp, lm(emot_symp_16y ~ + emot_symp_age_16y + sex + ethnicity + mum_uni + relevel(stress_group_halfsd, ref = 3)*unweighted_LE_sum)) 
fit_pool3 <- mice::pool(model_covars)
model_with_covars <- my_table(fit_pool3) # apply formatting function

# INTERACTION WITH COVARIATES: ONE SD
# stress group predicting emotional symptoms + covariates
model_covars_onesd <- with(full_imp, lm(emot_symp_16y ~ + emot_symp_age_16y + sex + ethnicity + mum_uni + relevel(stress_group_onesd, ref=3)*unweighted_LE_sum))
fit_pool4 <- mice::pool(model_covars_onesd) # stress group not a significant predictor on its own, but for high sensitivity individuals greater nr of LE = higher depression score
model_with_covars_onesd <- my_table(fit_pool4) # apply formatting function

# WITH PRS
# INTERACTION WITH COVARIATES: HAFL SD
# stress group predicting emotional symptoms + covariates
model_prs <- with(full_imp, lm(emot_symp_16y ~ + emot_symp_age_16y + sex + ethnicity + mum_uni + scale(PRS) + relevel(stress_group_halfsd, ref=3)*unweighted_LE_sum)) 
fit_pool5 <- mice::pool(model_prs) 
model_with_prs <- my_table(fit_pool5) # apply formatting function

# INTERACTION WITH COVARIATES: ONE SD
# stress group predicting emotional symptoms + covariates
model_prs_onesd <- with(full_imp, lm(emot_symp_16y ~ emot_symp_age_16y + sex + ethnicity + mum_uni  + scale(PRS) + relevel(stress_group_onesd, ref=3)*unweighted_LE_sum)) 
fit_pool6 <- mice::pool(model_prs_onesd) # stress group not a significant predictor on its own, but for high sensitivity individuals greater nr of LE = higher depression score
model_with_prs_onesd <- my_table(fit_pool6) # apply formatting function

## SAVE ALL INTERACTION MODELS
all_models <- list("no_covars_halfsd" = model_no_covars, "no_covars_onesd" = model_data_onesd, 
                   "covars_halfsd" = model_with_covars, "covars_onesd" = model_with_covars_onesd, 
                   "prs_halfsd" = model_with_prs, "prs_onesd" =model_with_prs_onesd)

openxlsx::write.xlsx(all_models, file = paste0("all-interaction-results-", Sys.Date(),'.xlsx'), rowNames = F, overwrite=T)


####################################
###       PLOT INTERACTION       ### 
####################################

# HALF SD
# Plot the interaction slopes without outlier
pdf("interaction-plot-with-outlier-halfsd-30th-imp.pdf")
p <- ggplot(dataset_30, aes(x = unweighted_LE_sum, 
                               y = emot_symp_16y, 
                               color = stress_group_halfsd)) +
  geom_point() + 
  #geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method= "lm" , fill="lightgrey", se=TRUE) +
  scale_color_manual(values=c("#FFC20A", "darkblue", "lightblue"), name = "Stress group", labels = c('High sensitivity', 'Low sensitivity', 'Typical sensitivity')) + 
  # scale_color_grey() +
  theme_classic()
# the shaded region corresponds to the estimated standard error of the fitted values

p + xlab("Unweighted life events score at 16 years (sum)") + ylab("SDQ emotional symptoms score at 16 years")  
dev.off()

# ONE SD
pdf("interaction-plot-with-outlier-onesd-30th-imp.pdf")
p <- ggplot(dataset_30, aes(x = unweighted_LE_sum, 
                               y = emot_symp_16y, 
                               color = stress_group_onesd)) +
  geom_point() + 
  #geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method= "lm" , fill="lightgrey", se=TRUE) +
  scale_color_manual(values=c("#FFC20A", "darkblue", "lightblue"), name = "Stress group", labels = c('High sensitivity', 'Low sensitivity', 'Typical sensitivity')) +
  # scale_color_grey() +
  theme_classic()
# the shaded region corresponds to the estimated standard error of the fitted values

p + xlab("Unweighted life events score at 16 years (sum)") + ylab("SDQ emotional symptoms score at 16 years")  
dev.off()


####################################
###    REMOVE OUTLIER AND PLOT   ### 
####################################

# remove green point that seems an outlier (using the 30th dataset)
dataset_30[dataset_30$unweighted_LE_mean == 1 ,]$cidB2957 # cidB2957 = 13728

outlier_test <- dataset_30 %>% filter(cidB2957 != 13728)

model <- lm(emot_symp_16y ~ stress_group_halfsd*unweighted_LE_mean, data = outlier_test)
summary(model) # now stress_group-1:unweighted_LE_mean interaction is not significant, only the stress_group1 is

# HALF SD
# Plot the interaction slopes without outlier
pdf("interaction-plot-without-outlier-halfsd.pdf")
p <- ggplot(outlier_test, aes(x = unweighted_LE_sum, 
                              y = emot_symp_16y, 
                              color = stress_group_halfsd)) +
  geom_point() + 
  #geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method= "lm" , fill="lightgrey", se=TRUE) +
  scale_color_manual(values=c("#FFC20A", "darkblue", "lightblue"), name = "Stress group", labels = c('High sensitivity', 'Low sensitivity', 'Typical sensitivity')) + 
  # scale_color_grey() +
  theme_classic()
# the shaded region corresponds to the estimated standard error of the fitted values

p + xlab("Unweighted life events score at 16 years (sum)") + ylab("SDQ emotional symptoms score at 16 years")  
dev.off()

# ONE SD
# Plot the interaction slopes without outlier
pdf("interaction-plot-without-outlier-onesd.pdf")
p <- ggplot(outlier_test, aes(x = unweighted_LE_sum, 
                              y = emot_symp_16y, 
                              color = stress_group_onesd)) +
  geom_point() + 
  #geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method= "lm" , fill="lightgrey", se=TRUE) +
  scale_color_manual(values=c("#FFC20A", "darkblue", "lightblue"), name = "Stress group", labels = c('High sensitivity', 'Low sensitivity', 'Typical sensitivity')) + 
  # scale_color_grey() +
  theme_classic()
# the shaded region corresponds to the estimated standard error of the fitted values

p + xlab("Unweighted life events score at 16 years (mean)") + ylab("SDQ emotional symptoms score at 16 years")  
dev.off()

####################################
###              SAVE            ### 
####################################

# save full_imp mids object, which has all imputed datasets + the stress group variables
saveRDS(full_imp, file.path(data_path,'LE_imputation_list_with_z_prs_stress_groups.rds'))

############## end script #################




