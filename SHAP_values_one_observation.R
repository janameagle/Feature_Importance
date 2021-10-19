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

#library(xgboost)
library(randomForest)
library(stringr)
library(lubridate)
library(cowplot)
library(shapper)
#library(ggforce)
source("shap.R")
#install_shap()


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
# create an explainer of the random forest element
exp_rf <- explain(data_rf, data = data_filtered[,1:ncol(data_filtered)-1])

# get the individual variable effect
# new_observation is the observation to be analyzed
ive_rf <- shap(exp_rf, new_observation = data_filtered[1,1:ncol(data_filtered)-1])
ive_rf

# uncomment to see an example for class 20
#ive_rf_20 <- ive_rf[ive_rf$`_ylevel_` == 20, ]
#shapper:::plot.individual_variable_effect(ive_rf_20, bar_width = 4)



################################################################################

# export results

################################################################################

for (i in levels(data$aux_vector_CODE)){
  ive_rf_i <- ive_rf[ive_rf$`_ylevel_` == i, ]
  plot_i <- shapper:::plot.individual_variable_effect(ive_rf_i, bar_width = 4)
  assign(paste0("p", i), plot_i)
}

nplots <- length(unique(ive_rf[, '_vname_']))

# create graph from output and export
# depending on nplots a different number of plots fit on one PDF page
# multiple pdfs are created and deleted conditionally due to issues in conditional creation
pdf(paste0("SHAP_output/", file, "_s_SHAP_", sensor, "_", band, "_", feature, "_", startmonth, "-", endmonth, ".pdf"), width = 8.3, height = 11.7)
      plot_grid(p10, p20, p21, ncol = 1, label_size = 12)
      plot_grid(p22, p23, p24, ncol = 1, label_size = 12)
      plot_grid(p25, p30, p41, ncol = 1, label_size = 12)
      plot_grid(p42, p51, p52, ncol = 1, label_size = 12)
      plot_grid(p53, p60, p70, ncol = 1, label_size = 12)
      plot_grid(p80, p90, p100, ncol = 1, label_size = 12)
dev.off()

  
pdf(paste0("SHAP_output/", file, "_m_SHAP_", sensor, "_", band, "_", feature, "_", startmonth, "-", endmonth, ".pdf"), width = 8.3, height = 11.7)
      plot_grid(p10, p20, ncol = 1, label_size = 12)
      plot_grid(p21, p22, ncol = 1, label_size = 12)
      plot_grid(p23, p24, ncol = 1, label_size = 12)
      plot_grid(p25, p30, ncol = 1, label_size = 12)
      plot_grid(p41, p42, ncol = 1, label_size = 12)
      plot_grid(p51, p52, ncol = 1, label_size = 12)
      plot_grid(p53, p60, ncol = 1, label_size = 12)
      plot_grid(p70, p80, ncol = 1, label_size = 12)
      plot_grid(p90, p100, ncol = 1, label_size = 12)
dev.off()
  
pdf(paste0("SHAP_output/", file, "_l_SHAP_", sensor, "_", band, "_", feature, "_", startmonth, "-", endmonth, ".pdf"), width = 8.3, height = 11.7)
      plot_grid(p10, ncol = 1, label_size = 12)
      plot_grid(p20, ncol = 1, label_size = 12)
      plot_grid(p22, ncol = 1, label_size = 12)
      plot_grid(p23, ncol = 1, label_size = 12)
      plot_grid(p24, ncol = 1, label_size = 12)
      plot_grid(p25, ncol = 1, label_size = 12)
      plot_grid(p30, ncol = 1, label_size = 12)
      plot_grid(p41, ncol = 1, label_size = 12)
      plot_grid(p42, ncol = 1, label_size = 12)
      plot_grid(p51, ncol = 1, label_size = 12)
      plot_grid(p52, ncol = 1, label_size = 12)
      plot_grid(p53, ncol = 1, label_size = 12)
      plot_grid(p60, ncol = 1, label_size = 12)
      plot_grid(p70, ncol = 1, label_size = 12)
      plot_grid(p80, ncol = 1, label_size = 12)
      plot_grid(p90, ncol = 1, label_size = 12)
      plot_grid(p100, ncol = 1, label_size = 12)
dev.off()


# depending on nplots, the unaesthetic files are deleted, only one with good readability will remain
if(nplots <= 24) { # define number of plots per page for good readability
  file.remove(paste0("SHAP_output/", file, "_m_SHAP_", sensor, "_", band, "_", feature, "_", startmonth, "-", endmonth, ".pdf"))
  file.remove(paste0("SHAP_output/", file, "_l_SHAP_", sensor, "_", band, "_", feature, "_", startmonth, "-", endmonth, ".pdf"))
} else if(nplots <= 40) {
  file.remove(paste0("SHAP_output/", file, "_s_SHAP_", sensor, "_", band, "_", feature, "_", startmonth, "-", endmonth, ".pdf"))
  file.remove(paste0("SHAP_output/", file, "_l_SHAP_", sensor, "_", band, "_", feature, "_", startmonth, "-", endmonth, ".pdf"))
} else {
  file.remove(paste0("SHAP_output/", file, "_s_SHAP_", sensor, "_", band, "_", feature, "_", startmonth, "-", endmonth, ".pdf"))
  file.remove(paste0("SHAP_output/", file, "_m_SHAP_", sensor, "_", band, "_", feature, "_", startmonth, "-", endmonth, ".pdf"))
}

################################################################################
# point plot
shap_values=predict(data_rf, data_filtered, predcontrib = TRUE, approxcontrib = F)

## Calculate shap values
shap_result = shap.score.rank(xgb_model = data_rf, 
                                   X_train =bike_x,
                                   shap_approx = F
)

# `shap_approx` comes from `approxcontrib` from xgboost documentation. 
# Faster but less accurate if true. Read more: help(xgboost)

## Plot var importance based on SHAP
var_importance(shap_result_bike, top_n=10)

## Prepare data for top N variables
shap_long_bike = shap.prep(shap = shap_result_bike,
                           X_train = bike_x , 
                           top_n = 10
)

plot.shap.summary(data_long = shap_long_bike)



## point plot

ggplot(data = ive_rf) +
  coord_flip() + 
  # sina plot: 
  geom_sina(aes(x = ive_rf[,"_vname_"], y = ive_rf[, '_yhat_mean_'] + pmax(ive_rf[, '_attribution_'], 0), color = ive_rf[,'_sign_'])) +
  # print the mean absolute value: 
  geom_text(data = unique(ive_rf[, c("_vname_", "_yhat_mean_")]),
            aes(x = ive_rf[,"_vname_"], y=-Inf, label = round(ive_rf[,"_yhat_mean_"],3)),
            hjust = -0.2) + # bold
  # # add a "SHAP" bar notation
  # annotate("text", x = -Inf, y = -Inf, vjust = -0.2, hjust = 0, size = 3,
  #          label = expression(group("|", bar(SHAP), "|"))) + 
  #scale_color_discrete(low="#FFCC33", high="#6600CC", 
  #                     breaks=c(0,1), labels=c("Low","High")) 
  theme_bw() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), # remove axis line
        legend.position="bottom") +
  labs(y = "SHAP value (impact on model output)", x = "", color = "Feature value") +
  # reverse the order of features
  scale_x_discrete(limits = rev(levels(ive_rf[, '_yhat_mean_']))) +
  geom_hline(yintercept = 0) +  # the vertical line
  scale_y_continuous(limits = c(-max(abs(ive_rf[, '_yhat_mean_'])), max(abs(ive_rf[, '_yhat_mean_'])))) 
