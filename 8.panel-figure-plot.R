# This script combines 4 plots into one 

######################################
###             SET UP             ###
######################################
# load libraries
library(mice)
library(tidyverse)
library(openxlsx)

# define data path
data_path =  "path to data"

# read in imputed data (obtained with '3.imputed-data-regressions.R' script)
imp <- readRDS(file.path(data_path,'LE_imputation_list_with_z_prs.rds'))
full_imp<- readRDS(file.path(data_path,'LE_imputation_list_with_z_prs_stress_groups.rds'))

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

# function
interaction.plot <- function(data, group){
  group2 <-  data[, group]
  p <- ggplot(data, aes(x = unweighted_LE_sum, 
                        y = smfq_16y_sum_imp, 
                        color = group2)) +
    #geom_point() + 
    #geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(aes(linetype = group2), show.legend = FALSE, method= "lm",fill="lightgrey", se=TRUE) +
    #geom_smooth(method= "lm" , fill="lightgrey", se=TRUE) +
    scale_color_manual(values=c("#FFC20A", "darkblue", "lightblue"), name = "Stress reactivity", labels = c('High', 'Low', 'Typical')) + 
    # scale_color_grey() +
    theme_minimal() + geom_jitter(aes(colour = stress_group_halfsd),width = 0.3,height = 0, alpha = 0.3) + theme(legend.position = "top", panel.grid.major = element_line(color = "gray", linetype = "dashed"))
  # the shaded region corresponds to the estimated standard error of the fitted values
  
  p + xlab("Unweighted life events (sum)") + ylab("SMFQ score at 16 years") + theme(legend.position = "bottom")  
}

####################################
###       PLOT INTERACTION       ### 
####################################
## Only plot 30th dataset for manuscript ##
dataset_30 <- complete(full_imp, 30)

# change reference level
dataset_30$stress_group_halfsd <- relevel(dataset_30$stress_group_halfsd, ref = 3)
dataset_30$stress_group_onesd <- relevel(dataset_30$stress_group_onesd, ref = 3)
males <- dataset_30 %>% filter(sex == 1) # subset males for plotting
females <- dataset_30 %>% filter(sex == 2) # subset females for plotting

# Arrange above plots into panels for manuscript
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
p1 = interaction.plot(data = dataset_30, group = "stress_group_halfsd") 
p2 = interaction.plot(data = males, group = "stress_group_halfsd")
p3 = interaction.plot(data = females, group = "stress_group_halfsd")

# remove point that seems an outlier (using the 30th dataset) --> will be used for conceptual figure in the manuscript
dataset_30[dataset_30$unweighted_LE_mean == 1 ,]$cidB2957 # cidB2957 = 13728
dataset_30s <- dataset_30 %>% filter(cidB2957 != 13728)

# plot association between unweighted_LE_sum and weighted_LE_mean (conceptual figure based on empirical data)
p <- ggplot(dataset_30s, aes(x = unweighted_LE_sum, y = weighted_LE_sum)) +
  geom_smooth(method = "lm", se = FALSE, color = "gray", linetype = "solid") +
  geom_segment(aes(xend = unweighted_LE_sum, yend = predicted), alpha = 0.2) +
  #geom_point(aes(color = stress_group_halfsd), size = 2, shape = 20, alpha = 0.3) +
  scale_color_manual(values=c("#FFC20A", "darkblue", "lightblue"), name = "Stress reactivity", labels = c('High', 'Low', 'Typical')) + 
  geom_jitter(aes(colour = stress_group_halfsd),width = 0,height = 0, alpha = 0.3) +
  labs(x = "Unweighted LE Sum", y = "Weighted LE Sum") +
  theme_minimal() +
  theme(legend.position = "top", panel.grid.major = element_line(color = "gray", linetype = "dashed"))
p4 <- p + xlab("Unweighted life events (sum)") + ylab("Impact-weighted life event score") + labs(colour = "Stress group")

# add 'Person A' and 'Person B' to plot
s1 = p4 + annotate(geom="point", x=12, y=19,
                  color="red3", shape = 23, bg="red3", size = 2)
s2 = s1 + annotate(geom="text", x=14, y=19, label="Person B",
                   color="red3", fontface = "bold", size = 3.5) 
s3 = s2 + annotate(geom="point", x=7, y=19,
                   color="red3", shape = 23, bg="red3", size = 2)
s4 = s3 + annotate(geom="text", x=5, y=19, label="Person A",
                   color="red3", fontface = "bold", size = 3.5) 
s5 = s4 + annotate(geom="point", x=7, y=9,
                   color="red3", shape = 23, bg="red3", size = 2)
s6 = s5 + annotate(geom="text", x=9, y=9, label="Person C",
                   color="red3", fontface = "bold", size = 3.5) 

# combine only two of the above figures into one (conceptual + interaction)
figure1 <- ggarrange(s6, p1,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1,
                    common.legend = TRUE, legend = "bottom"
                    
) 
figure1
ggexport(figure1, filename = "Figure_1_two_panels.png", res = 300, width = 2500, height = 1350)

# same as above but no header 
figure1.v2 <- ggarrange(s6, p1,
                     ncol = 2, nrow = 1,
                     common.legend = TRUE, legend = "bottom"
                     
) 

figure1.v2
ggexport(figure1.v2, filename = "Figure_1_no_headers_two_panels.png", res = 300, width = 2500, height = 1350)

# sex-specific 
figure_sex <- ggarrange(p2, p3,
                     #labels = c("A", "B"),
                     ncol = 2, nrow = 1,
                     common.legend = TRUE, legend = "bottom"
                     
)
figure_sex
ggexport(figure_sex, filename = "Figure_sex_panels.png", res = 300, width = 2500, height = 1350)


