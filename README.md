# Feature_Importance


################################################################################
                
                                                                            
This repository enables you to calculate two measures for feature importance
for a feature-sample-matrix of Sentinel-1, Sentinel-2 temporal spectral features
in the form of a csv. The two main scripts are
    feature_importance.R - which calculates the mean decrease in node impurity /    
        impurity measure for the random forest classifier
    SHAP_values_xgboost.R - which creates a random forest using the xgboost
        package and SHAP values Shapley Additive exPlanations) for each feature 
        accordingly
                                                                             
You are able to choose the regarding sensor, bands, features, and time range 
                                                                             
Input: A matrix with temporal spectral features should be in a folder 'data' 
                                                                             
Output: xlsx files with the calculated feature importance / SHAP values
        PDF files with plots of the values per feature
        All outputs will be saved in the 'output' or 'SHAP_output' folders                     
                      
                                                                             
################################################################################