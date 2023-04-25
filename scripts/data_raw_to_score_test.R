library(data.table) # melt()
library(dplyr)
library(stringr) #str_sub()
library(ggplot2)

Indctr_lst <- c("Greenhouse_Gas","N_Surplus","Soil_Erosion","Water_Consumption",
                 "P_Surplus","Land_Use_Change","Price_Volatility", "Finance_Access", 
                 "Labor_Productivity", "Government_Support", "Trade_Openness", 
                 "Food_Loss","Under_nourishment","Crop_Diversity","Rural_Poverty",
                 "Gender_Gap","Land_Right","Food_Affordability")
neg_lst <- c("P_Surplus","N_Surplus", "Food_Loss", "Under_nourishment", 
             "Rural_Poverty", "Land_Right") # indicators that require negative transformation
pos_lst <- c("Finance_Access","Crop_Diversity","Food_Affordability",'Gender_Gap') # indicators that do not require transformation before normalization
log_lst <- c("Labor_Productivity", "Government_Support", "Trade_Openness") # indicators that require positive logarithmic transformation
neg_log_lst<- c("Water_Consumption","Land_Use_Change","Greenhouse_Gas","Soil_Erosion") # indicators that require negative logarithmic transformation
inverse_lst <- c("Price_Volatility")

#### Functions
NormalizeScores <- function(x) {
  x$SAM_Score_new <- 33*(x$SAM_Score_new-red_bndry)/(green_bndry-red_bndry)+33
  score_list <- x$SAM_Score_new
  #score_list[score_list > 100] <- 100
  #score_list[score_list < 0] <- 0
  return(score_list)
}

### read in all raw csv files and combine them in long format
setwd('C://SAM//data_reference//data_raw//') # depending on where you unzip file
getwd()
files <- list.files(pattern = '\\.csv$', full.names = FALSE)
Raw_list <- c()
x=1
for (Raw_val in files){
  indicator <- str_sub(Raw_val,5, -5)
  Raw_df <- read.csv(Raw_val)
  Raw_df_new <- melt(setDT(Raw_df), id.vars = c("ISO","Country_Name"), variable.name = "Year")
  Raw_df_new$Indicator <- indicator
  Raw_list[[x]] <- Raw_df_new
  x=x+1
}
Raw_data <- do.call(rbind, Raw_list)
Raw_data$Year <- as.integer(gsub('^.{1}', '', Raw_data$Year))
names(Raw_data)[names(Raw_data) == 'value'] <- 'Raw'
Raw_data <- na.omit(Raw_data)

setwd('C://SAM//data_reference//data_scores') # depending on where you unzip file
files <- list.files(pattern = '\\.csv$', full.names = FALSE)
Score_list <- c()
x=1
for (score in files){
  indicator <- str_sub(score,8, -5)
  score_df <- read.csv(score)
  score_df_new <- melt(setDT(score_df), id.vars = c("ISO","Country_Name"), variable.name = "Year")
  score_df_new$Indicator <- indicator
  Score_list[[x]] <- score_df_new
  x=x+1
}
Score_data <- do.call(rbind, Score_list)
Score_data$Year <- as.integer(gsub('^.{1}', '', Score_data$Year))
names(Score_data)[names(Score_data) == 'value'] <- 'SAM_Score'
Score_data <- na.omit(Score_data)
#score score and raw
SAM_Score_join <- full_join(Raw_data, Score_data, by = c("Country_Name","ISO", "Year", 'Indicator'))
SAM_Score_join <- na.omit(SAM_Score_join)
SAM_Score_join$SAM_Score_new <- SAM_Score_join$Raw
SAM_Score_join$SAM_Score_v2 <- NA

### Transoformations
df_trnsfrm_lst <- c()
x=1
for (Indctr in Indctr_lst) {
  print(Indctr)
  df_trnsfrm <- SAM_Score_join[which(SAM_Score_join$Indicator==Indctr),]
  if (Indctr == "Food_Loss"){ #### Econ Indicators
    # indicator_name <- 'Food Loss (FLP)'
    # y_label <- 'FLP (%)'
    green_bndry <- 2.2
    red_bndry <- 6.6
  }
  else if (Indctr == "Trade_Openness"){
    # indicator_name <- 'Trade Openness (TROP)'
    # y_label <- 'TROP (%)'
    green_bndry <- 71
    red_bndry <- 17
  }
  else if (Indctr == "Government_Support"){
    # indicator_name <- ' Government Support (AEXP)'
    # y_label <- 'AEXP (2011 US$ PPP)'
    green_bndry <- 2405
    red_bndry <- 25
  }
  else if (Indctr == "Price_Volatility"){
    # indicator_name <- 'Price Volatility (PVOL)'
    # y_label <- 'PVOL (unitless)'
    green_bndry <- 0.10
    red_bndry <- 0.23
  }
  else if (Indctr == "Finance_Access"){
    # indicator_name <- 'Finance Access (A2F)'
    # y_label <- 'A2F (score)'
    green_bndry <- 100
    red_bndry <- 25
  }
  else if (Indctr == "Labor_Productivity"){
    # indicator_name <- 'Labor Productivity (AGDP)'
    # y_label <- 'AGDP (2011 US$ PPP)'
    green_bndry <- 7946
    red_bndry <- 460
  }
  else if (Indctr == 'Soil_Erosion'){ #### Env Indicators
    # indicator_name <- 'Soil Erosion (SER)'
    # y_label <- 'SER (ton/ha)'
    green_bndry <- 1
    red_bndry <- 5
  }
  else if (Indctr == 'Greenhouse_Gas'){
    # indicator_name <- 'Greenhouse Gas Emissions (GHG)'
    # y_label <- 'GHG (CO2eq/ha)'
    green_bndry <- 0.86
    red_bndry <- 1.08
  }
  else if (Indctr == 'Land_Use_Change'){
    # indicator_name <- 'Land Cover Change (LCC)'
    # y_label <- 'LCC (ha-deforested/ha-Cropland/yr)'
    green_bndry <- 0.00
    red_bndry <- 0.0053
  }
  else if (Indctr == 'P_Surplus'){
    # indicator_name <- 'Phosphorous Surplus (Psur)'
    # y_label <- 'Psur (kg P/ha/yr)'
    green_bndry <- 3.5
    red_bndry <- 6.9
  }
  else if (Indctr == "N_Surplus"){
    # indicator_name <- 'Nitrogen Surplus (Nsur)'
    # y_label <- 'Nsur (kg N/ha/yr)'
    green_bndry <- 52
    red_bndry <- 69
  }
  else if (Indctr == 'Water_Consumption'){
    # indicator_name <- 'Water Consumption (SUSI)'
    # y_label <- 'SUSI (annual km^3 irrigation/ km^3 sustainable water consum.)'
    green_bndry <- 1
    red_bndry <- 2
  }
  else if (Indctr == "Crop_Diversity"){ #### Soc. Indicators
    # indicator_name <- 'Crop Diversity (Hindex)'
    # y_label <- 'Hindex (count)'
    green_bndry <- 48
    red_bndry <- 22
  }
  else if (Indctr == "Food_Affordability"){
    # indicator_name <- 'Food Affordability (RSE)'
    # y_label <- 'RSE (%)'
    green_bndry <- 100
    red_bndry <- 30
  }
  else if (Indctr == "Under_nourishment"){
    # indicator_name <- 'Under-nourishment (UDN)'
    # y_label <- 'UDN (%)'
    green_bndry <- 0
    red_bndry <- 7.5
  }
  else if (Indctr == "Rural_Poverty"){
    # indicator_name <- 'Rural Poverty (RPV)'
    # y_label <- 'RPV (%)'
    green_bndry <- 2
    red_bndry <- 13
  }
  else if (Indctr == "Gender_Gap"){
    # indicator_name <- 'Gender Gap Score (GGG)'
    # y_label <- 'GGG (score)'
    green_bndry <- 80
    red_bndry <- 70
  }
  else {
    # indicator_name <- 'Land Rights (LRS)'
    # y_label <- 'LRS (score)'
    green_bndry <- 2
    red_bndry <- 3
  }
  
  # nor transform boundary thresholds and indicator values
  if (Indctr %in% neg_log_lst){
    df_trnsfrm$SAM_Score_new <- -log10(df_trnsfrm$SAM_Score_new + min(df_trnsfrm$SAM_Score_new) + 1)
    green_bndry <- -log10(green_bndry + min(green_bndry) + 1)
    red_bndry <- -log10(red_bndry + min(red_bndry) + 1)
    df_trnsfrm$SAM_Score_v2 <- NormalizeScores(df_trnsfrm)
    df_trnsfrm_lst[[x]] <- df_trnsfrm
    x=x+1
  }
  else if (Indctr %in% log_lst){
    df_trnsfrm$SAM_Score_new <- log10(df_trnsfrm$SAM_Score_new + min(df_trnsfrm$SAM_Score_new) + 1)
    green_bndry <- log10(green_bndry + min(green_bndry) + 1)
    red_bndry <- log10(red_bndry + min(red_bndry) + 1)
    df_trnsfrm$SAM_Score_v2 <- NormalizeScores(df_trnsfrm)
    df_trnsfrm_lst[[x]] <- df_trnsfrm
    x=x+1
  }
  else if (Indctr %in% neg_lst){
    df_trnsfrm$SAM_Score_new <- -df_trnsfrm$SAM_Score_new
    green_bndry <- -green_bndry
    red_bndry <- -red_bndry
    df_trnsfrm$SAM_Score_v2 <- NormalizeScores(df_trnsfrm)
    df_trnsfrm_lst[[x]] <- df_trnsfrm
    x=x+1
  }
  else if (Indctr %in% inverse_lst){
    df_trnsfrm$SAM_Score_new <- 1/(df_trnsfrm$SAM_Score_new + min(df_trnsfrm$SAM_Score_new) + 1)
    green_bndry <- 1/(green_bndry + min(green_bndry) + 1)
    red_bndry <- 1/(red_bndry + min(red_bndry) + 1)
    df_trnsfrm$SAM_Score_v2 <- NormalizeScores(df_trnsfrm)
    df_trnsfrm_lst[[x]] <- df_trnsfrm
    x=x+1
  }
  else{
    print(Indctr)
    green_bndry <- green_bndry
    red_bndry <- red_bndry
    df_trnsfrm$SAM_Score_v2 <- NormalizeScores(df_trnsfrm)
    df_trnsfrm_lst[[x]] <- df_trnsfrm
    x=x+1
  }
}
print(x)
Trnsfrm_data = do.call(rbind, df_trnsfrm_lst)

#Trnsfrm_data$diff <- Trnsfrm_data$SAM_Score_v2-Trnsfrm_data$SAM_Score

ISO_lst <- c('AUT', 'BRA', 'TUR', 'USA', 'KEN', "GHA", "MAR", "ZAF")
for (iso in ISO_lst){
  print(iso)
  df <- Trnsfrm_data[which(Trnsfrm_data$ISO==iso),]
  ggplot(df, aes(x= df$Year)) +
    geom_line(aes(y = df$SAM_Score, colour = 'Original'), size = 1) +
    geom_line(aes(y = df$SAM_Score_v2, colour = 'Re-calculated'), size = 1) +
    xlab('Year') +
    ylab('Score (0-100)') +
    ggtitle(unique(df$Country_Name)) +
    scale_color_manual(name = "Scores", values = c("Original" = "black", "Re-calculated" = "red")) +
    theme(legend.position='bottom') +
    facet_wrap(vars(df$Indicator))
  
  ggsave(paste0('C://SAM_tst//',iso,".png"), width = 8, height = 8)
}
