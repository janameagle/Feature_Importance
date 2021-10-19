################################################################################
#                                                                              #
# This script enables you to calculate the SHAP values (Shapley Additive       #
# exPlanations) for a feature-sample-matrix of Sentinel-1, Sentinel-2 temporal #
# spectral features in the form of a csv.                                      #
#                                                                              #
# You are able to choose the regarding sensor, bands, features, and time range #
#                                                                              #
# Input: A matrix with temporal spectral features should be in a folder 'data' #
#                                                                              #
# Output: A summary point plot of the shap values per feature, all features    #
#         A summary point plot of the shap values per feature, top features    #
#         A dependence plot for top features                                   #
#         All outputs will be saved in the 'SHAP_output' folder                #
#                                                                              #
#                                                                              #
# Souce: Code and instruction are taken from                                   #
#   https://liuyanguu.github.io/post/2018/10/14/shap-visualization-for-xgboost/#
#   https://github.com/liuyanguu/SHAPforxgboost                                #
#                                                                              #
################################################################################


# load needed libraries
library(data.table)
library(SHAPforxgboost)
library(dplyr)
library(xgboost)
library(ggplot2)
library(stringr)
library(lubridate)
#source("SHAPforRandomForest.R")



getwd()
# setwd('enter your working directory here')

data_raw <- read.csv("data/STR3_Data_Extraction_v11.csv")
file <- "v11" # used to mark output files


################################################################################

# preparing the dataframe for random forest and data filtering

################################################################################

# drop aux columns, as they are not needed
data <- data_raw[ , !grepl("aux_" , names(data_raw))]

# include aux_vector_CODE again as it contains the class information, include as factor for random forest classification
data$aux_vector_CODE = as.factor(data_raw$aux_vector_CODE)  
# add a column with rownames for filtering
data$rownames <- row.names(data)
tail(names(data))



# add two new rows with startmonth and endmonth for temporal filtering

# starting month
# matching S2 name pattern, S1 entries will get NA
data[nrow(data)+1, ] <- str_extract(names(data), "_[[:digit:]]{4}.[[:digit:]]{2}.[[:digit:]]{2}") %>% 
  gsub("_", "", .) %>% 
  as.Date(. , "%Y.%m.%d") %>% 
  month(.)


# matching S1 name pattern
data[nrow(data), is.na(data[nrow(data),])] <- str_extract(names(data[,is.na(data[nrow(data),])]), "_[[:digit:]]{4}[[:digit:]]{2}") %>%  
  gsub("_", "", .) %>% 
  paste0(. , "01") %>% # without day info as.Date won't work
  as.Date(. , "%Y%m%d") %>% 
  month(.)

data[nrow(data), "rownames"] <- "start"


# endmonth
# matching S2 name pattern, S1 entries will get NA
data[nrow(data)+1, ] <- str_extract(names(data), "TO[[:digit:]]{4}.[[:digit:]]{2}.[[:digit:]]{2}") %>% 
  gsub("TO", "", .) %>% 
  as.Date(. , "%Y.%m.%d") %>% 
  month(.)

# matching S1 name pattern
data[nrow(data), is.na(data[nrow(data),])] <- str_extract(names(data[,is.na(data[nrow(data),])]), "_[[:digit:]]{4}[[:digit:]]{2}") %>% 
  gsub("_", "", .) %>% 
  paste0(. , "01") %>% # without day info as.Date won't work
  as.Date(. , "%Y%m%d") %>% 
  month(.)

data[nrow(data), "rownames"] <- "end"

str(data)



################################################################################

# run script from here with new filters (Strg + Alt + E)

################################################################################
# define your values of interest
# if choosing several of one type, add them like x1-x2-x3, without a space between

sensor = "S2"           # S1, S2
band = "B01-B02-B03-B04"       # S1: VV, VH; S2: B01, B02, B03, B04, B05, B06, B07, B08, B11, B12, B8A, NDVI
feature = "min-max-median"            # S1: min, max, mean; S2: min, max, median, avg
startmonth = 1             # 1 - 12
endmonth = 12              # 1 - 12


# filtering data for values of interest
# adjust values for filter
f.sensor = str_replace_all(sensor, "-", "|")
f.band = str_replace_all(band, "-", "|")
f.feature = str_replace_all(feature, "-", "|")
f.startmonth = startmonth
f.endmonth = endmonth



# filter the data to your predefined values
data_filtered <- data[ , grepl(f.sensor , names(data))]
data_filtered <- data_filtered[ , grepl(f.band, names(data_filtered))]
data_filtered <- data_filtered[ , grepl(f.feature, names(data_filtered))]
data_filtered$rownames <- data$rownames # was filtered out before
data_filtered <- data_filtered[ , data_filtered[data_filtered$rownames == 'start',] >= f.startmonth]
data_filtered <- data_filtered[ , data_filtered[data_filtered$rownames == 'end',] <= f.endmonth]

data_filtered$aux_vector_CODE = data$aux_vector_CODE  # include class info again
names(data_filtered)


################################################################################
# remove rows with na values. Na values are -9999. Wasn't done before to avoid removal caused by unnecessary column
data_filtered[data_filtered == -9999] <- NA
# nrow(data_filtered)
data_filtered = na.omit(data_filtered)
# nrow(data_filtered)




################################################################################

# Create random forest element

################################################################################

# reduce dataframe to less rows for testing
#data_filtered = data_filtered[0:1000,]

# set seed for comparison
set.seed(4858)

dtrain = as.matrix(select(data_filtered, -aux_vector_CODE))


params <- list(
  objective = "binary:logistic",  # for classification, not regression
  learning_rate = 1,              # to create a random forest without boosting
  num_parallel_tree = 500,
  subsample = 0.63,
  colsample_bynode = 0.8,
  reg_lambda = 0,
  max_depth = 20,
  min_child_weight = 2
)

mod1 = xgboost::xgboost(
  data = dtrain, 
  label = data_filtered$aux_vector_CODE, 
  gamma = 0, 
  eta = 1,
  lambda = 0, 
  nrounds = 1,                   # to create a random forest without boosting
  verbose = FALSE, 
  params)


# shap.values(model, X_dataset) returns the SHAP
# data matrix and ranked features by mean|SHAP|
shap_values <- shap.values(xgb_model = mod1, X_train = dtrain)
shap_values$mean_shap_score
shap_values_data <- shap_values$shap_score



# shap.prep() returns the long-format SHAP data from either model or
shap_long <- shap.prep(xgb_model = mod1, X_train = dtrain)
# is the same as: using given shap_contrib
shap_long <- shap.prep(shap_contrib = shap_values_data, X_train = dtrain)


# **SHAP summary plot**
# shap.plot.summary(shap_long, scientific = FALSE)
# shap.plot.summary(shap_long, x_bound  = 3, dilute = 10)

# Alternatives options to make the same plot:
# option 1: from the xgboost model
# shap.plot.summary.wrap1(mod1, X = dtrain, top_n = 3)

# option 2: supply a self-made SHAP values dataset
# (e.g. sometimes as output from cross-validation)
# shap.plot.summary.wrap2(shap_score = shap_values_data, X = dtrain, top_n = 10)


# Dependence plot for each feature
# xgboost::xgb.plot.shap(data = dtrain, model = mod1, top_n = 9, n_col = 3)


################################################################################

# export results

################################################################################

# long plot shows all features, might have a bad readability
pdf(paste0("SHAP_output/", file, "_SHAPlong_", sensor, "_", band, "_", feature, "_", startmonth, "-", endmonth, ".pdf"), width = 11.7, height = 8.3)
    shap.plot.summary(shap_long, dilute = 30) # use dilute to reduce nr points plotted
dev.off()


# get the number of features for the long plot
nfeats <- length(names(data_filtered))-1
n_max <- 50 # change, if you want to reduce nr of features plotted

# shows all features, but maximum 50 (to fit on one page), or maximum your defined n_max
pdf(paste0("SHAP_output/", file, "_SHAPtop_", sensor, "_", band, "_", feature, "_", startmonth, "-", endmonth, ".pdf"), width = 11.7, height = 8.3)
    shap.plot.summary.wrap2(shap_score = shap_values_data, X = dtrain, dilute = 30, top_n = min(50, nfeats, n_max)) # use dilute to reduce nr points plotted
dev.off()



# plot dependence plot for top 9 features
pdf(paste0("SHAP_output/", file, "_SHAPdependence_", sensor, "_", band, "_", feature, "_", startmonth, "-", endmonth, ".pdf"), width = 11.7, height = 8.3)
    xgb.plot.shap(data = dtrain, model = mod1, top_n = 9, n_col = 3)
dev.off()
