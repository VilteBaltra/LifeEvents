######################################
###             SET UP             ###
######################################
# load libraries
library(mice)
library(miceadds)
library(openxlsx)
library(psych)

# define data path
data_path =  "path to data"

# read in imputed data (obtained with '4.life-events-interaction.R' script)
full_imp <- readRDS(file.path(data_path,'LE_imputation_list_with_z_prs_stress_groups.rds'))

######################################
##     DESCRIPTIVES BEFORE IMP      ##
######################################

# Extract the original set (with NAs)
original <- complete(full_imp, 0) 

# unweighted life event items 
le_unweighted <- c("ccs2000", "ccs2010", "ccs2020" ,"ccs2030", "ccs2040", "ccs2050", "ccs2060", "ccs2070" ,"ccs2080" ,"ccs2090", "ccs2100",
                   "ccs2110", "ccs2120", "ccs2130", "ccs2140", "ccs2150", "ccs2160", "ccs2170", "ccs2180", "ccs2190", "ccs2200", "ccs2210",
                   "ccs2220")

# weighted life event items 
le_weighted <- c("ccs2001", "ccs2011", "ccs2021", "ccs2031", "ccs2041","ccs2051", "ccs2061", "ccs2071", "ccs2081", "ccs2091", "ccs2101",
                 "ccs2111", "ccs2121", "ccs2131" ,"ccs2141", "ccs2151", "ccs2161", "ccs2171" ,"ccs2181", "ccs2191", "ccs2201" ,"ccs2211", 
                 "ccs2221")

# smfq item names
smfq_16y_13items <- c('ccs4500','ccs4502', 'ccs4503', 'ccs4504', 'ccs4505', 'ccs4506', 'ccs4508', 'ccs4509', 'ccs4511', 'ccs4512', 'ccs4513', 'ccs4514', 'ccs4515')


# define function to check item missingness
item_missingness <- function(dataset, sample.size) {
  cont_before <- psych::describe(original[, dataset])
  # print descriptives
  print(cont_before)
  # print % missingness per item
  print(cbind(rownames(cont_before),  round(100 - ( (cont_before$n / sample.size)* 100 ),1)))
}

# unweighted LE items
item_missingness(dataset=le_unweighted, sample.size = 4791)
# weighted LE items
item_missingness(dataset=le_weighted, sample.size = 4791)
# smfq items
item_missingness(dataset=smfq_16y_13items, sample.size = 4791)






