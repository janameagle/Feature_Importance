library(tidyverse)
library(xgboost)
library(caret)
source("shap.R")

##############################################
# How to calculate and interpret shap values
##############################################

load(url("https://github.com/christophM/interpretable-ml-book/blob/master/data/bike.RData?raw=true"))
#readRDS("bike.RData")

data_2=select(data_filtered, -aux_vector_CODE)

data_dmy = dummyVars(" ~ .", data = data_2, fullRank=T)
data_x = predict(data_dmy, newdata = data_2)

## Create the xgboost model
model_data = xgboost(data = data_x, 
                     nround = 10, 
                     objective="reg:linear",
                     label= data_filtered$aux_vector_CODE)  


cat("Note: The functions `shap.score.rank, `shap_long_hd` and `plot.shap.summary` were
originally published at https://github.com/liuyanguu/Blogdown/blob/master/hugo-xmag/content/post/2018-10-05-shap-visualization-for-xgboost.Rmd
All the credits to the author.")

## Calculate shap values
shap_result = shap.score.rank(xgb_model = model_data, 
                                   X_train =data_x,
                                   shap_approx = F
)

# `shap_approx` comes from `approxcontrib` from xgboost documentation. 
# Faster but less accurate if true. Read more: help(xgboost)

## Plot var importance based on SHAP
var_importance(shap_result, top_n=10)

## Prepare data for top N variables
shap_long = shap.prep(shap = shap_result,
                           X_train = data_x , 
                           top_n = 10
)

## Plot shap overall metrics
plot.shap.summary(data_long = shap_long)


## 
xgb.plot.shap(data = data_x, # input data
              model = model_data, # xgboost model
              features = names(shap_result$mean_shap_score[1:10]), # only top 10 var
              n_col = 3, # layout option
              plot_loess = T # add red line to plot
)


# Do some classical plots
# ggplotgui::ggplot_shiny(bike)
 