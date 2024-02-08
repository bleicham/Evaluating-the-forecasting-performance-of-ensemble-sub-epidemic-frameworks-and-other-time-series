#------------------------------------------------------------------------------#
#                                                                              #
#             File 2: Forecasting for ARIMA, GAM, SLR, and Prophet             #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# The following code file reads in and cleans the data used during the         #
# forecasting process. It also creates the folders where forecasts will be     #
# stored and prepares the input files for the n-sub-epidemic and spatial-wave  #
# toolboxes.                                                                   #
#------------------------------------------------------------------------------#                                                                              
# Set-up:                                                                      #
#                                                                              #
# Before running the code file, ensure a folder called 'Orignal_Data' is       #
# located within your working directory. The `CDC-08-09-2023.csv` and          #
# `owid-monkeypox-data.csv` files must be located within the 'Orignal_Data'    #
# folder.                                                                      #
#------------------------------------------------------------------------------#
#        Authors: Amanda Bleichrodt, Ruiyan Luo, Alexander Kirpich             #
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
#                           Needed Packages                                    #
#------------------------------------------------------------------------------#
library(tidyverse)
library(lubridate)
library(stats)
library(prophet)
library(mgcv)

#########################################################
# Calibration periods for model fitting and forecasting #
#########################################################
calibration <- c(9, 10, 11)

###################################################################
# Looping through forecast types (e.g., ARIMA, SLR, GAM, Prophet) #
###################################################################
for(f in 1:4){
  
  # Setting correct index for running different forecast types 
  forecasts_type <- (f-1)
  
  #####################################
  # Looping through forecast horizons #
  #####################################
  for(h in 1:4){
    
    # Setting forecast horizon
    forecast_length <- h
    
    #######################################
    # Looping through calibration lengths #
    #######################################
    for(c in 1:3){
      
      # Selecting the calibration period for forecasting
      calibration_forecast_length <- calibration[c]
      
#------------------------------------------------------------------------------#
#                     Preparing the dates for forecasting                      #
#------------------------------------------------------------------------------#

######################################################################
# Adjusting the start and end date to end of week rather than start  #
######################################################################

# Last day of the week for the first forecast period 
start <- base::switch(c,
                      lubridate::ymd("2022-06-30") + 6, # 9-Weeks
                      lubridate::ymd("2022-07-07") + 6, # 10-Weeks
                      lubridate::ymd("2022-07-14") + 6) # 11-Weeks

# Last day of the week for the last forecast period 
end <- (lubridate::ymd("2023-08-03") + 6) - (forecast_length*7)


######################################
# Setting the ground-truth data date #
######################################
Ground_Truth_Data <- "2023-08-03"


###############################################################
# Creating the vector of dates to loop through in forecasting #
###############################################################
forecastingDates <- base::seq(lubridate::ymd(start), lubridate::ymd(end), by = "week")


#####################################
# Looping through forecasting dates #
#####################################
for(d in 1:base::length(forecastingDates)){
  
  # Forecasting week indexed in the loop of dates 
  Week_Index <- base::as.character((forecastingDates[d]))
  
      
      
#------------------------------------------------------------------------------#
#                                                                              #
#                      Reading in the ground-truth data                        #
#                                                                              #
#------------------------------------------------------------------------------#
  
  ground_truth <- read_csv("Inputs/OWID_CDC_Weekly_Counts_Combined.csv") %>%
    dplyr::filter(Week <= Week_Index) # Filtering to include data corresponding to forecast period 
   
     
#------------------------------------------------------------------------------#
#                                                                              #
#                        Setting up Calibration Period                         #
#                                                                              #
#------------------------------------------------------------------------------#
  
  ###################################
  # Creating the calibration period #
  ###################################
  calibrationPeriod <- ground_truth %>% 
    # Filtering the main data to only include the data needed to calibrate the model
    dplyr::filter(Week > lubridate::ymd(max(lubridate::ymd(ground_truth$Week)) 
                                        - (7*calibration_forecast_length))) %>% 
    ungroup() # Un-grouping the data 
  
  
  #############################################
  # Removing the Date column fore forecasting #
  #############################################
  calibrationPeriodFinal <- calibrationPeriod %>%
    dplyr::select(-Week) # Removing the date column 
  

#------------------------------------------------------------------------------#
#                                                                              #
#                     Final preparations for forecasting                       #
#                                                                              #
#------------------------------------------------------------------------------#
  
  #####################################
  # Creating a list of location names #
  #####################################
  locationList <- base::colnames(calibrationPeriodFinal)
     
  ###############################
  # Needed variables for models #
  ###############################
  
  # Number of columns to be run through the forecast
  ncols <- base::ncol(calibrationPeriodFinal)
  
  ####################
  # Forecasted Dates #
  ####################
  
  # Creating an empty vector to fill in with forecast dates 
  forecasted_dates <- NA
  
  # Determining the forecast dates 
  for(z in 1:forecast_length){
    
    forecasted_dates[z] <- lubridate::ymd(max(lubridate::ymd(calibrationPeriod$Week)) + (7*z))
    
  }
  
  # Changing the forecast dates to date-format
  forecasted_dates <- as.Date(forecasted_dates)
      
#------------------------------------------------------------------------------#
#                                                                              #
#                              ARIMA Forecasts                                 #
#                                                                              #
#------------------------------------------------------------------------------#
  
  if(forecasts_type == 0){
    
    ################################
    # Pre-forecasting preparations #
    ################################
    
    # Creates matrix to fill in with the parameters of the best fitting model 
    orders <- base::matrix(0,ncols, 7)
    
    # Creating a matrix to fill in with the forecasting means 
    forecast.mean <- base::matrix(0, forecast_length, ncols)
    
    # Creating a matrix, the same size as the forecast.means matrix for UBs
    forecast.upper <- forecast.mean
    
    # Creating a matrix, the same size as the forecast.means matrix for LBs
    forecast.lower <- forecast.mean
    
    # Creating a vector of prediction levels to run through the model 
    levels <- 100-c(0.02, 0.05, seq(0.1, 0.9, by=0.1))*100
    
    # Empty vector to fill in with p-values of Ljung-Box test
    pvalues <- base::rep(0,ncols)
    
    #################################################
    # Looping through each of the areas of interest #
    #################################################
    for(a in 1:ncols){
      
      # Location name currently indexed
      location_current <- locationList[a]
      
      # Selecting the needed column from the calibration data
      sdata <- calibrationPeriodFinal %>%
        dplyr::select(all_of(location_current)) %>%
        na.omit()
      
      # Transforming the `sdata` vector to a R-time series object for forecasts
      sdata.ts <- stats::ts(sdata, start=1, frequency=1)
      
      ########################################
      # Fitting the Non-Seasonal ARIMA model #
      ########################################
      
      # Fitting the model 
      fit <- forecast::auto.arima(sdata.ts, start.p=0, max.p=10, start.q=0,
                                  max.q=5, max.P=3, max.Q=3,max.d=2, max.D=2, 
                                  trace=0)
      
      # Producing the Ljung-Box test, and Residual output 
      pvalues[a]=forecast::checkresiduals(fit)$p.value
      
      ################################################################
      # Producing forecasts and PIs using the ARIMA model from above #
      ################################################################
      
      # Forecasting from the model 
      fcst <- forecast::forecast(fit, h=forecast_length, level=levels)
            
      # Creating a variable that includes the forecasted values/means 
      means <- fcst$mean
            
      # Changing all neg. forecasted values to 0
      means[means < 0] <- 0
            
      # Creating a variable that includes the lower PI values 
      lower <- fcst$lower
            
      # Changing all neg. lower CI values to 0
      lower[lower < 0] <- 0
            
      # Creating a variable that contains the upper PI values 
      upper <- fcst$upper
            
      # Changing all neg. upper PI values to 0
      upper[upper < 0] <- 0
            
      # Combining the means, and PIs into one data frame 
      fcstval <- base::cbind(means, lower, upper)
      
      # Creating a data frame with the forecasts
      all_forecast <- as.data.frame(fcstval)
      
      ############################################
      # Saving the individual quantile forecasts #
      ############################################
      
      # Date label for file name
      dateLabel <- lubridate::ymd(Week_Index) - 6

      # Saving the forecast for each location/forecast period 
      write.csv(all_forecast, paste0("ARIMA/Forecasts/All-Quantiles/ARIMA-weekly-Mpox-Cases-calibration-", calibration_forecast_length, 
                                "-horizon-", forecast_length, "-", location_current, "-", paste(format(lubridate::ymd(dateLabel), "%m-%d-%Y")), ".csv"),
                row.names = F)
      
      ############################
      # Formatting the forecasts #
      ############################
      
      # Model calibration period data
      calibration_data <- calibrationPeriod %>%
        dplyr::select(Week, location_current) %>% # Selecting date and location
        dplyr::mutate(year = lubridate::year(Week), # Year column
                      month = lubridate::month(Week), # Month column
                      day = lubridate::day(Week), # Day column
                      median = "NaN", # Median column
                      LB = "NaN", # LB column
                      UB = "NaN") %>% # UB column
        dplyr::rename("data" = location_current) %>% # Observed data
        dplyr::mutate(median = data) %>% # Filling in median with data
        dplyr::select(-Week) %>% # Removing full date column
        dplyr::select(year, month, day, data, median, LB, UB) # Reordering 
      
      # Determining the observed data during the forecasted period
      observed_forecast_data <- read_csv("Inputs/OWID_CDC_Weekly_Counts_Combined.csv") %>%
        dplyr::filter(Week %in% c(forecasted_dates)) %>% # Selecting observed data during forecast period
        dplyr::select(location_current, -Week) # Selecting column corresponding to indexed location 
      
      # Renaming the column to L1 
      names(observed_forecast_data) <- "L1"
        
      # Organizing the forecast data
      forecasted_data <- all_forecast %>%
        dplyr::select(means, `lower.95%`, `upper.95%`) %>% # Selecting mean, LB, UB
        dplyr::mutate(Week = forecasted_dates, # Dates of forecast period
                      year = lubridate::year(Week), # Year of forecast period
                      month = lubridate::month(Week), # Month of forecast period
                      day = lubridate::day(Week), # Day of forecast period
                      data = observed_forecast_data$L1, # Observed data during forecast period
                      median = means, # Forecast
                      LB = `lower.95%`, # LB of forecast
                      UB = `upper.95%`) %>% # UB of forecast
        dplyr::select(-Week, -means, -`lower.95%`, -`upper.95%`) # Removing un-needed variables
      
      # Combining the calibration period and forecast period 
      formattedForecast <- base::rbind(calibration_data, forecasted_data) 
      
      # Saving the formatted forecast for each location/forecast period 
      write.csv(formattedForecast , paste0("ARIMA/Forecasts/Formatted/ARIMA-weekly-Mpox-Cases-calibration-", calibration_forecast_length, 
                                     "-horizon-", forecast_length, "-", location_current, "-", paste(format(lubridate::ymd(dateLabel), "%m-%d-%Y")), "-Formatted.csv"),
                row.names = F)
      
    }# End of loop going through locations 
    
    } # End of ARIMA forecasting 
        
#------------------------------------------------------------------------------#
#                                                                              #
#                                SLR Forecasts                                 #
#                                                                              #
#------------------------------------------------------------------------------#
  
  else if(forecasts_type == 1){
    
    ################################
    # Pre-forecasting preparations #
    ################################
    
    # Saving the desired alpha values in which to produce Prediction
    # intervals under the variable name alphas
    alphas <- c(0.02, 0.05, seq(0.1, 0.9, by=0.1))
    
    # Creating the `time` predictor, which is the length of the calibration
    # period 
    time <- seq(1:calibration_forecast_length)
    
    # Emptying the previous forecast data frame holder
    all_forecast <- NA
    
    #################################################
    # Looping through each of the areas of interest #
    #################################################
    
    for(s in 1:ncols){
      
      # Location name currently indexed
      location_current <- locationList[s]
      
      # Selecting the column of data needed for modeling
      data <- calibrationPeriodFinal %>%
        dplyr::select(any_of(location_current)) %>%
        dplyr::rename("A1" = location_current)
      
      # Saving the calibration period for modeling as vector 
      data.cur <- na.omit(as.numeric(data$A1))
      
      #############################################
      # Fitting the SLR model, assuming normality #
      #############################################
      
      # Fitting the SLR model, with time as the only predictor 
      glm.mod <- stats::glm(data.cur~time)
      
      ############################################
      # Producing forecast from fitted SLR model #
      ############################################
      
      # Forecasting from the model fit
      fcst <- stats::predict(glm.mod, data.frame(time=(calibration_forecast_length+1): 
                                                   (calibration_forecast_length+forecast_length)),
                      se.fit=T)
      
      # Creating a variable that includes the forecasted values/means 
      all_forecast <- as.data.frame(as.numeric(fcst$fit))
      
      ##################################
      # Producing Prediction intervals #
      ##################################
      for(iAlpha in 1:length(alphas)){
        
        # Lower bounds of the prediction intervals 
        lower <- fcst$fit+qnorm(alphas[iAlpha]/2)*fcst$se.fit 
        
        # Upper bounds of the prediction intervals 
        upper <- fcst$fit+qnorm(1-alphas[iAlpha]/2)*fcst$se.fit 
        
        # Combining the forecasted means, and upper and lower PIs into one
        all_forecast <- base::cbind(all_forecast,base::cbind(as.numeric(lower),as.numeric(upper)))
        
        } # End of loop producing PI intervals
      
      # Changing all negative values to 0 in the forecast data frame 
      all_forecast[all_forecast < 0] <- 0
      
      ###############################################
      # Labeling the columns of the forecasted data #
      ###############################################

      # Assigning the names to the columns of the forecasted data frame 
      names(all_forecast) <- c("fitted", paste(rep(c("l", "u"), length(alphas)),
                                              rep(alphas,each=2), sep=" "))
      
      
      # Re-ordering and re-naming data columns for producing metrics later
      all_forecast <- all_forecast %>%
        # Re-ordering the columns
        dplyr::select(fitted, `l 0.9`, `l 0.8`, `l 0.7`, `l 0.6`, `l 0.5`, `l 0.4`, `l 0.3`, `l 0.2`,
                      `l 0.1`, `l 0.05`, `l 0.02`, `u 0.9`, `u 0.8`, `u 0.7`, `u 0.6`, `u 0.5`, `u 0.4`,
                          `u 0.3`, `u 0.2`, `u 0.1`, `u 0.05`, `u 0.02`) %>%
        # Re-naming the columns
        dplyr::rename(`means` = fitted,
                      `lower.10%` = `l 0.9`,
                      `lower.20%` = `l 0.8`,
                      `lower.30%` = `l 0.7`,
                      `lower.40%` = `l 0.6`,
                      `lower.50%` = `l 0.5`,
                      `lower.60%` = `l 0.4`,
                      `lower.70%` = `l 0.3`,
                      `lower.80%` = `l 0.2`,
                      `lower.90%` = `l 0.1`,
                      `lower.95%` = `l 0.05`,
                      `lower.98%` = `l 0.02`,
                      `upper.10%` = `u 0.9`,
                      `upper.20%` = `u 0.8`,
                      `upper.30%` = `u 0.7`,
                      `upper.40%` = `u 0.6`,
                      `upper.50%` = `u 0.5`,
                      `upper.60%` = `u 0.4`,
                      `upper.70%` = `u 0.3`,
                      `upper.80%` = `u 0.2`,
                      `upper.90%` = `u 0.1`,
                      `upper.95%` = `u 0.05`,
                      `upper.98%` = `u 0.02`)
      
      ############################################
      # Saving the individual quantile forecasts #
      ############################################
      
      # Date label for file name
      dateLabel <- lubridate::ymd(Week_Index) - 6

      # Saving the forecast for each location/forecast period 
      write.csv(all_forecast, paste0("SLR/Forecasts/All-Quantiles/SLR-weekly-Mpox-Cases-calibration-", calibration_forecast_length, 
                                     "-horizon-", forecast_length, "-", location_current, "-", paste(format(lubridate::ymd(dateLabel), "%m-%d-%Y")), ".csv"),
                row.names = F)
          

      ############################
      # Formatting the forecasts #
      ############################
      
      # Model calibration period data
      calibration_data <- calibrationPeriod %>%
        dplyr::select(Week, location_current) %>% # Selecting date and location
        dplyr::mutate(year = lubridate::year(Week), # Year column
                      month = lubridate::month(Week), # Month column
                      day = lubridate::day(Week), # Day column
                      median = "NaN", # Median column
                      LB = "NaN", # LB column
                      UB = "NaN") %>% # UB column
        dplyr::rename("data" = location_current) %>% # Observed data
        dplyr::mutate(median = data) %>% # Filling in median with data
        dplyr::select(-Week) %>% # Removing full date column
        dplyr::select(year, month, day, data, median, LB, UB) # Reordering 
      
      # Determining the observed data during the forecasted period
      observed_forecast_data <- read_csv("Inputs/OWID_CDC_Weekly_Counts_Combined.csv") %>%
        dplyr::filter(Week %in% c(forecasted_dates)) %>% # Selecting observed data during forecast period
        dplyr::select(location_current, -Week) # Selecting column corresponding to indexed location 
      
      # Renaming the column to L1 
      names(observed_forecast_data) <- "L1"
      
      # Organizing the forecast data
      forecasted_data <- all_forecast %>%
        dplyr::select(means, `lower.95%`, `upper.95%`) %>% # Selecting mean, LB, UB
        dplyr::mutate(Week = forecasted_dates, # Dates of forecast period
                      year = lubridate::year(Week), # Year of forecast period
                      month = lubridate::month(Week), # Month of forecast period
                      day = lubridate::day(Week), # Day of forecast period
                      data = observed_forecast_data$L1, # Observed data during forecast period
                      median = means, # Forecast
                      LB = `lower.95%`, # LB of forecast
                      UB = `upper.95%`) %>% # UB of forecast
        dplyr::select(-Week, -means, -`lower.95%`, -`upper.95%`) # Removing un-needed variables
      
      # Combining the calibration period and forecast period 
      formattedForecast <- base::rbind(calibration_data, forecasted_data) 
      
      # Saving the formatted forecast for each location/forecast period 
      write.csv(formattedForecast , paste0("SLR/Forecasts/Formatted/SLR-weekly-Mpox-Cases-calibration-", calibration_forecast_length, 
                                           "-horizon-", forecast_length, "-", location_current, "-", paste(format(lubridate::ymd(dateLabel), "%m-%d-%Y")), "-Formatted.csv"),
                row.names = F)
      
    } # End of locations loop
    
    } # End of SLR forecasting 


#------------------------------------------------------------------------------#
#                                                                              #
#                                GAM Forecasts                                 #
#                                                                              #
#------------------------------------------------------------------------------#
  
  else if(forecasts_type == 2){
    
    ################################
    # Pre-forecasting preparations #
    ################################
    
    # Saving the desired alpha values in which to produce Prediction
    # intervals under the variable name alphas
    alphas <- c(0.02, 0.05, seq(0.1, 0.9, by=0.1))
    
    # Creating the `time` predictor, which is the length of the calibration
    # period 
    time <- seq(1:calibration_forecast_length)
    
    # Emptying the previous forecast data frame holder
    all_forecast <- NA
    
    #################################################
    # Looping through each of the areas of interest #
    #################################################
    
    for(g in 1:ncols){
      
      # Location name currently indexed
      location_current <- locationList[g]
      
      # Selecting the column of data needed for modeling
      data <- calibrationPeriodFinal %>%
        dplyr::select(any_of(location_current)) %>%
        dplyr::rename("A1" = location_current)
      
      # Saving the calibration period for modeling as vector 
      data.cur <- na.omit(as.numeric(data$A1))
      
      # Determining K - basis dimension choice for smoothing based on data length
      k <- floor(as.numeric(nrow(na.omit(data[,1])))/2) + 5
      
      
      #############################################
      # Fitting the GAM model, assuming normality #
      #############################################
      
      # Fitting the GAM model, with time as the only predictor 
      gam.mod <- mgcv::gam(data.cur~s(time,bs="ps",k=k)) 
      
      ############################################
      # Producing forecast from fitted GAM model #
      ############################################
      
      # Forecasting from the model fit
      fcst <- stats::predict(gam.mod, data.frame(time=(calibration_forecast_length+1): 
                                                   (calibration_forecast_length+forecast_length)),
                             se.fit=T)
      
      # Creating a variable that includes the forecasted values/means 
      all_forecast <- as.data.frame(as.numeric(fcst$fit))
      
      # Checking residuals of GAM model 
      mgcv::gam.check(gam.mod)
      
      ##################################
      # Producing Prediction intervals #
      ##################################
      for(iAlpha in 1:length(alphas)){
        
        # Lower bounds of the prediction intervals 
        lower <- fcst$fit+qnorm(alphas[iAlpha]/2)*fcst$se.fit 
        
        # Upper bounds of the prediction intervals 
        upper <- fcst$fit+qnorm(1-alphas[iAlpha]/2)*fcst$se.fit 
        
        # Combining the forecasted means, and upper and lower PIs into one
        all_forecast <- base::cbind(all_forecast,base::cbind(as.numeric(lower),as.numeric(upper)))
        
      } # End of loop producing PI intervals
      
      # Changing all negative values to 0 in the forecast data frame 
      all_forecast[all_forecast < 0] <- 0
      
      ###############################################
      # Labeling the columns of the forecasted data #
      ###############################################
      
      # Assigning the names to the columns of the forecasted data frame 
      names(all_forecast) <- c("fitted", paste(rep(c("l", "u"), length(alphas)),
                                              rep(alphas,each=2), sep=" "))
      
      
      # Re-ordering and re-naming data columns for producing metrics later
      all_forecast <- all_forecast %>%
        # Re-ordering the columns
        dplyr::select(fitted, `l 0.9`, `l 0.8`, `l 0.7`, `l 0.6`, `l 0.5`, `l 0.4`, `l 0.3`, `l 0.2`,
                      `l 0.1`, `l 0.05`, `l 0.02`, `u 0.9`, `u 0.8`, `u 0.7`, `u 0.6`, `u 0.5`, `u 0.4`,
                      `u 0.3`, `u 0.2`, `u 0.1`, `u 0.05`, `u 0.02`) %>%
        # Re-naming the columns
        dplyr::rename(`means` = fitted,
                      `lower.10%` = `l 0.9`,
                      `lower.20%` = `l 0.8`,
                      `lower.30%` = `l 0.7`,
                      `lower.40%` = `l 0.6`,
                      `lower.50%` = `l 0.5`,
                      `lower.60%` = `l 0.4`,
                      `lower.70%` = `l 0.3`,
                      `lower.80%` = `l 0.2`,
                      `lower.90%` = `l 0.1`,
                      `lower.95%` = `l 0.05`,
                      `lower.98%` = `l 0.02`,
                      `upper.10%` = `u 0.9`,
                      `upper.20%` = `u 0.8`,
                      `upper.30%` = `u 0.7`,
                      `upper.40%` = `u 0.6`,
                      `upper.50%` = `u 0.5`,
                      `upper.60%` = `u 0.4`,
                      `upper.70%` = `u 0.3`,
                      `upper.80%` = `u 0.2`,
                      `upper.90%` = `u 0.1`,
                      `upper.95%` = `u 0.05`,
                      `upper.98%` = `u 0.02`)
      
      ############################################
      # Saving the individual quantile forecasts #
      ############################################
      
      # Date label for file name
      dateLabel <- lubridate::ymd(Week_Index) - 6
      
      # Saving the forecast for each location/forecast period 
      write.csv(all_forecast, paste0("GAM/Forecasts/All-Quantiles/GAM-weekly-Mpox-Cases-calibration-", calibration_forecast_length, 
                                     "-horizon-", forecast_length, "-", location_current, "-", paste(format(lubridate::ymd(dateLabel), "%m-%d-%Y")), ".csv"),
                row.names = F)
      
      
      ############################
      # Formatting the forecasts #
      ############################
      
      # Model calibration period data
      calibration_data <- calibrationPeriod %>%
        dplyr::select(Week, location_current) %>% # Selecting date and location
        dplyr::mutate(year = lubridate::year(Week), # Year column
                      month = lubridate::month(Week), # Month column
                      day = lubridate::day(Week), # Day column
                      median = "NaN", # Median column
                      LB = "NaN", # LB column
                      UB = "NaN") %>% # UB column
        dplyr::rename("data" = location_current) %>% # Observed data
        dplyr::mutate(median = data) %>% # Filling in median with data
        dplyr::select(-Week) %>% # Removing full date column
        dplyr::select(year, month, day, data, median, LB, UB) # Reordering 
      
      # Determining the observed data during the forecasted period
      observed_forecast_data <- read_csv("Inputs/OWID_CDC_Weekly_Counts_Combined.csv") %>%
        dplyr::filter(Week %in% c(forecasted_dates)) %>% # Selecting observed data during forecast period
        dplyr::select(location_current, -Week) # Selecting column corresponding to indexed location 
      
      # Renaming the column to L1 
      names(observed_forecast_data) <- "L1"
      
      # Organizing the forecast data
      forecasted_data <- all_forecast %>%
        dplyr::select(means, `lower.95%`, `upper.95%`) %>% # Selecting mean, LB, UB
        dplyr::mutate(Week = forecasted_dates, # Dates of forecast period
                      year = lubridate::year(Week), # Year of forecast period
                      month = lubridate::month(Week), # Month of forecast period
                      day = lubridate::day(Week), # Day of forecast period
                      data = observed_forecast_data$L1, # Observed data during forecast period
                      median = means, # Forecast
                      LB = `lower.95%`, # LB of forecast
                      UB = `upper.95%`) %>% # UB of forecast
        dplyr::select(-Week, -means, -`lower.95%`, -`upper.95%`) # Removing un-needed variables
      
      # Combining the calibration period and forecast period 
      formattedForecast <- base::rbind(calibration_data, forecasted_data) 
      
      # Saving the formatted forecast for each location/forecast period 
      write.csv(formattedForecast , paste0("GAM/Forecasts/Formatted/GAM-weekly-Mpox-Cases-calibration-", calibration_forecast_length, 
                                           "-horizon-", forecast_length, "-", location_current, "-", paste(format(lubridate::ymd(dateLabel), "%m-%d-%Y")), "-Formatted.csv"),
                row.names = F)
      
      } # End of locations loop
    
    } # End of GAM forecasting 
  
#------------------------------------------------------------------------------#
#                                                                              #
#                            Prophet Forecasts                                 #
#                                                                              #
#------------------------------------------------------------------------------#
  
  else if(forecasts_type == 3){
    
    ################################
    # Pre-forecasting preparations #
    ################################
    
    # Quantiles used in model fitting and forecasting 
    quantiles <- c(0.02, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90)
    
    #################################################
    # Looping through each of the areas of interest #
    #################################################
    
    for(p in 1:ncols){
      
      # Location name currently indexed
      location_current <- locationList[p]
      
      # Calibration period 
      data <- calibrationPeriod %>%
        dplyr::select(Week, any_of(location_current)) %>% # Selecting the location
        dplyr::rename("ds" = Week, # Re-labeling date column
                      "y" = location_current) # Re-labeling observed column 


      #####################################################################
      # Preparing the quantiles for forecasting and model fit uncertainty #
      #####################################################################
      
      # List of quantiles to be forecasted 
      quantiles_names_list <- paste0(100 - 100*quantiles, "%") 
      
      # Creating the empty data frame to fill in with estimates
      all_quantiles <- data.frame(base::matrix(data = 0, nrow = dim(data)[1]+forecast_length, ncol = 3 + 2*length(quantiles)))
      
      # Naming the columns of the `all_quantiles` data frame
      names(all_quantiles) <- c("ds", "y", "prediction", paste0( "lower.", quantiles_names_list), paste0( "upper.", quantiles_names_list))
      
      
      ################################################################
      # Looping through quantiles to produce forecast and model fits #
      ################################################################
      for(quantile_current in quantiles) {
        
        # Loop indexed quantile 
        uncertainty_level_current <- 1 - quantile_current
        
        ##############################
        # Creating a prophet object. #
        ##############################
        
        # Fitting a prophet model that does not assume weekly seasonality
        prophet_object_current <- prophet::prophet(data, interval.width = uncertainty_level_current)
        
        
        #######################
        # Producing Forecasts #
        #######################
        
        # Dates to produce estimates and PIs for
        dates_for_forecast <- base::rbind(subset(data, select = ds), 
                                    data.frame(ds = c(forecasted_dates)))
        
        # Predicting for the specified dates.
        prophet_predictions <- stats::predict(prophet_object_current, dates_for_forecast) %>%
          mutate(ds = ymd(ds)) # Reformatting date
        
        
        ##################################################
        # Combining predicted values and observed values #
        ##################################################
        
        # Determining the observed data during the forecasted period
        observed_forecast_data <- read_csv("Inputs/OWID_CDC_Weekly_Counts_Combined.csv") %>%
          dplyr::filter(Week %in% c(prophet_predictions$ds)) %>% # Selecting observed data during forecast period
          dplyr::select(Week, location_current) %>% # Selecting column corresponding to indexed location 
          dplyr::rename("ds" = Week,
                        "y" = location_current)
        
        # Combining the observed values and predicted values by the date column 
        predictions_observed_data <- base::merge(x = observed_forecast_data, y = prophet_predictions, by = "ds") %>%
          dplyr::select(ds, y, yhat, yhat_lower, yhat_upper)
        
        # Setting the Dates (the same for all confidence levels)
        all_quantiles$ds <- predictions_observed_data$ds
        
        # Setting the Observed Data (the same for all confidence levels)
        all_quantiles$y  <- predictions_observed_data$y
        
        ############################################################
        # Setting labels for the model fit and quantile data frame #
        ############################################################
        
        # Labeling the predicted value
        all_quantiles_yhat_text  <-
          paste0("all_quantiles$prediction <- predictions_observed_data$yhat")
        # Running predicted value (text) code
        eval(parse(text = all_quantiles_yhat_text))
        
        # Labeling the lower bounds 
        all_quantiles_yhat_lower_text  <-
          paste0("all_quantiles$`lower.", 100*uncertainty_level_current, "%` <- predictions_observed_data$yhat_lower")
        # Running LB value (text) code
        eval(parse(text = all_quantiles_yhat_lower_text))
        
        # Labeling the upper bounds
        all_quantiles_yhat_upper_text  <-
          paste0("all_quantiles$`upper.", 100*uncertainty_level_current, "%` <- predictions_observed_data$yhat_upper")
        # Running UB value (text) code
        eval(parse(text = all_quantiles_yhat_upper_text))
      
      }
      
      
      #############################################################
      # Formatting the all-quantiles data frame for later metrics #
      #############################################################
      
      # Changing any negative value in the data frame to zero 
      all_quantiles[all_quantiles<0] <- 0
      
      # Renaming and re-ordering columns
      all_forecast <- all_quantiles %>%
        # Selecting only the forecast period 
        dplyr::filter(ds %in% c(forecasted_dates)) %>%
        # Re-ordering quantiles 
        dplyr::select(prediction,`lower.10%`,`lower.20%`, `lower.30%`, `lower.40%`,
                      `lower.50%`, `lower.60%`, `lower.70%`, `lower.80%`, 
                      `lower.90%`, `lower.95%`, `lower.98%`, `upper.10%`,
                      `upper.20%`, `upper.30%`, `upper.40%`, `upper.50%`,
                      `upper.60%`, `upper.70%`, `upper.80%`, `upper.90%`,
                      `upper.95%`, `upper.98%`) %>%
        dplyr::rename("means" = prediction) # Re-naming point prediction 
      
      ############################################
      # Saving the individual quantile forecasts #
      ############################################
      
      # Date label for file name
      dateLabel <- lubridate::ymd(Week_Index) - 6
      
      # Saving the forecast for each location/forecast period 
      write.csv(all_forecast, paste0("Prophet/Forecasts/All-Quantiles/Prophet-weekly-Mpox-Cases-calibration-", calibration_forecast_length, 
                                     "-horizon-", forecast_length, "-", location_current, "-", paste(format(lubridate::ymd(dateLabel), "%m-%d-%Y")), ".csv"),
                row.names = F)
      
      
      ############################
      # Formatting the forecasts #
      ############################
      
      # Formatted forecast, with 95% PIs 
      formattedForecast <- all_quantiles %>%
        dplyr::select(ds, y, prediction, `lower.95%`, `upper.95%`) %>% # Selecting date and location
        dplyr::mutate(year = lubridate::year(ds), # Year column
                      month = lubridate::month(ds), # Month column
                      day = lubridate::day(ds)) %>% # Day column
        dplyr::rename("data" = y, # Observed data
                      "median" = prediction, # Point prediction
                      "LB" = `lower.95%`, # Lower 95% PI
                      "UB" = `upper.95%`) %>% # Upper 95% PI
        dplyr::select(year, month, day, data, median, LB, UB) # Reordering 
      
      
      # Saving the formatted forecast for each location/forecast period 
      write.csv(formattedForecast , paste0("Prophet/Forecasts/Formatted/Prophet-weekly-Mpox-Cases-calibration-", calibration_forecast_length, 
                                           "-horizon-", forecast_length, "-", location_current, "-", paste(format(lubridate::ymd(dateLabel), "%m-%d-%Y")), "-Formatted.csv"),
                row.names = F)
      
    } # End of locations loop 
    
    } # End of Prophet forecasts 
  
  
#------------------------------------------------------------------------------#
#                                                                              #
#                    Observed Data during Forecast Period                      #
#                                                                              #
#------------------------------------------------------------------------------#
  
  # Observed data for calculating metrics
  observed_data <- read_csv("Inputs/OWID_CDC_Weekly_Counts_Combined.csv") %>%
    dplyr::filter(Week %in% c(forecasted_dates)) %>% # Selecting observed data during forecast period
    dplyr::select(-Week) # Removing the week indicator
  
  # Date label for file name
  dateLabel <- lubridate::ymd(Week_Index) - 6
  
  # Saving the observed data
  write.csv(observed_data, file = paste0("Observed/observed-weekly-mpox-cases-horizon-", forecast_length, "-", paste(format(lubridate::ymd(dateLabel), "%m-%d-%Y")), ".csv"))
  
  
  } # End of Forecasting Dates 
    
  } # End of calibration loop
  
} # End of forecasting horizons
  
} # End of forecasting type loops 
