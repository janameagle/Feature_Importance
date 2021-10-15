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
# Output: The matrix with the impurity measures in form of a xlsx file         #
#         A barplot showing the importance for each selected feature           #
#         A barplot showing the importance for each selected feature in        #
#             ascending order                                                  #
#         All outputs will be saved in the 'output' folder                     #
#                                                                              #
################################################################################

# load needed libraries
devtools::install_github('ModelOriented/treeshap')

library(xgboost)
library(randomForest)
library(stringr)
library(lubridate)
library(treeshap)
source("shap.R")
library(shapper)
install_shap()


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

sensor = "S1"           # S1, S2
band = "VH"       # S1: VV, VH; S2: B01, B02, B03, B04, B05, B06, B07, B08, B11, B12, B8A, NDVI
feature = "mean"            # S1: min, max, mean; S2: min, max, median, avg
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
data_filtered = data_filtered[0:1000,]

# set seed for comparison
set.seed(4858)
data_rf <- randomForest(aux_vector_CODE ~ ., data_filtered)


predict(data_rf)

# shapper
library("DALEX")
exp_rf <- explain(data_rf, data = data_filtered[,1:ncol(data_filtered)-1])

ive_rf <- shap(exp_rf, new_observation = data_filtered[1,1:ncol(data_filtered)-1])
ive_rf
plot(ive_rf, show_predicted = FALSE, bar_width = 4)


# filtered

ive_rf_20 <- ive_rf[ive_rf$`_ylevel_` == 20, ]
ive_rf_30 <- ive_rf[ive_rf$`_ylevel_` == 30, ]
ive_rf_42 <- ive_rf[ive_rf$`_ylevel_` == 42, ]
ive_rf_52 <- ive_rf[ive_rf$`_ylevel_` == 52, ]

p20 <- shapper:::plot.individual_variable_effect(ive_rf_20, bar_width = 4)
p30 <- shapper:::plot.individual_variable_effect(ive_rf_30)
p42 <- shapper:::plot.individual_variable_effect(ive_rf_42)
p52 <- shapper:::plot.individual_variable_effect(ive_rf_52)

library(cowplot)
plot_grid(p20, p30, p42, p52, labels = c('A', 'B'), ncol = 2, label_size = 12)



################################################################################

# export results

################################################################################

i = 23
for (i in levels(data$aux_vector_CODE)){
  ive_rf_i <- ive_rf[ive_rf$`_ylevel_` == i, ]
  plot_i <- shapper:::plot.individual_variable_effect(ive_rf_i, bar_width = 4)
  assign(paste0("p", i), plot_i)
}

# create graph from output and export
pdf(paste0("SHAP_output/", file, "_SHAP_", sensor, "_", band, "_", feature, "_", startmonth, "-", endmonth, ".pdf"), width = 8.3, height = 11.7)
  plot_grid(p10, p20, p21, ncol = 1, label_size = 12)
  plot_grid(p22, p23, p24, ncol = 1, label_size = 12)
  plot_grid(p25, p30, p41, ncol = 1, label_size = 12)
  plot_grid(p42, p51, p52, ncol = 1, label_size = 12)
  plot_grid(p52, p60, p70, ncol = 1, label_size = 12)
  plot_grid(p80, p90, p100, ncol = 1, label_size = 12)
dev.off()


