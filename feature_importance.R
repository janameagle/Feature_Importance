
################################################################################
#                                                                              #
# This script enables you to calculate the mean decrease in node impurity /    #
# impurity measure for the random forest classifier for a feature-sample-matrix#
# of Sentinel-1, Sentinel-2 temporal spectral features in the form of a csv.   #
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
library("writexl")
library("randomForest")
library("ggplot2")
library("stringr")
library("dplyr")
library("magrittr")
library("tidyverse")
library("lubridate")

getwd()
# setwd('enter your working directory here')

data_raw <- read.csv("data/STR3_Data_Extraction_v12_median.csv")
file <- "v12_median" # used to mark output files

# exploring and checking the data
str(data_raw)
head(names(data_raw))
class(data_raw)


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

sensor = "S1-S2"           # S1, S2
band = "VH-NDVI"       # S1: VV, VH; S2: B01, B02, B03, B04, B05, B06, B07, B08, B11, B12, B8A, NDVI
feature = "min-max"            # S1: min, max, mean; S2: min, max, median, avg
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



################################################################################
# remove rows with na values. Na values are -9999. Wasn't done before to avoid removal caused by unnecessary column
data_filtered[data_filtered == -9999] <- NA
# nrow(data_filtered)
data_filtered = na.omit(data_filtered)
# nrow(data_filtered)




################################################################################

# Create random forest element including importance measure

################################################################################

# reduce dataframe to less rows for testing
data_filtered = data_filtered[0:5000,]

# set seed for comparison
set.seed(4858)
data_rf <- randomForest(aux_vector_CODE ~ ., data = data_filtered, ntree = 200,
                        keep.forest = FALSE, importance = TRUE)

data_importance <- importance(data_rf, type=2)   # type 2 for MeanDecreaseGini, mean decrease in node impurity
data_importance
nrow(data_importance)




################################################################################

# export results

################################################################################

# convert to dataframe for export
data_importance_df <- as.data.frame(data_importance)
# set row names as column so that it will be written in the excel file
data_importance_df <- cbind(feature = rownames(data_importance_df), data_importance_df)


write_xlsx(data_importance_df, path = paste0("output/importance_", sensor, "_", band, "_", feature, "_", startmonth, "-", endmonth, "_gini.xlsx"), col_names = TRUE)


# define number of bars per page for good readability
n <- nrow(data_importance_df) / ceiling(nrow(data_importance_df)/50)

# create graph from output and export
pdf(paste0("output/", file, "_importance_", sensor, "_", band, "_", feature, "_", startmonth, "-", endmonth, "_gini.pdf"))
for (i in seq(1, nrow(data_importance_df), n)){
  p <- ggplot(data=data_importance_df[i:(i+n-1),], aes(y=feature, x=MeanDecreaseGini)) +
    geom_bar(stat="identity", fill = '#123661', width = 0.8) +
    theme_minimal() +
    labs(title=paste0("Feature importance for ", sensor, ",\n", band, ",\n", feature, ",\n", startmonth, "-", endmonth)) +
    theme(plot.title = element_text(size = 10, face = "bold", hjust = 1)) +
    scale_x_continuous(limits = c(0, max(data_importance_df$MeanDecreaseGini))) +
    scale_y_discrete(limits = rev) +
    geom_text(aes(label = round(MeanDecreaseGini), hjust = 1.2), color = "white", size = 3)
  print(p)
}

dev.off()



# also export in sorted order
data_importance_df_sorted <- data_importance_df[order(data_importance_df$MeanDecreaseGini),]

# define number of bars per page for good readability
n <- nrow(data_importance_df_sorted) / ceiling(nrow(data_importance_df_sorted)/50)
  
pdf(paste0("output/", file, "_sorted_importance_", sensor, "_", band, "_", feature, "_", startmonth, "-", endmonth, "_gini.pdf"))
for (i in seq(1, nrow(data_importance_df_sorted), n)){
  p_sorted <- ggplot(data=data_importance_df_sorted[i:(i+n-1),], aes(y=reorder(feature, -MeanDecreaseGini), x=MeanDecreaseGini)) +
    geom_bar(stat="identity", fill = '#123661', width = 0.8) +
    theme_minimal() +
    labs(title=paste0("Feature importance for ", sensor, ",\n", band, ",\n", feature, ",\n", startmonth, "-", endmonth), y="feature") +
    theme(plot.title = element_text(size = 10, face = "bold")) +
    scale_x_continuous(limits = c(0, max(data_importance_df_sorted$MeanDecreaseGini))) +
    geom_text(aes(label = round(MeanDecreaseGini), hjust = 1.2), color = "white", size = 3)
  print(p_sorted)
}

dev.off()

