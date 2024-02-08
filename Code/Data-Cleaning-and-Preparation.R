#------------------------------------------------------------------------------#
#                                                                              #
#            File 1: Data Cleaning and Preparation for Forecasting             #
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
# Author:                  Amanda Bleichrodt                                   #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#                                                                             
#                              Needed Packages                                 #
#------------------------------------------------------------------------------#
library(tidyverse)
library(lubridate)

#------------------------------------------------------------------------------#
#                             Creating Folders                                 #
#------------------------------------------------------------------------------#

{
  # Creating the folder to save the observed data for calculating metrics
  base::dir.create("Observed")
  
  # Creating the folder to save the inputs files for the SW and SE toolboxes
  base::dir.create("Inputs")
  
  # Creating the folder to save the inputs for the SW and SE toolboxes - 9W
  base::dir.create("Inputs/9-Weeks")
  
  # Creating the folder to save the inputs for the SW and SE toolboxes - 10W
  base::dir.create("Inputs/10-Weeks")
  
  # Creating the folder to save the inputs for the SW and SE toolboxes - 11W
  base::dir.create("Inputs/11-Weeks")
  
  # Creating the base folder for the n-sub-epidemic
  base::dir.create("n-sub-epidemic")
  
  # Creating the folder to save the forecast files for the n-sub-epidemic
  base::dir.create("n-sub-epidemic/Forecasts")
  
  # Creating the folder to save the individual forecast graphs for the n-sub-epidemic
  base::dir.create("n-sub-epidemic/Graphs")
  
  # Creating the folder to save the metrics for the n-sub-epidemic
  base::dir.create("n-sub-epidemic/Metrics")
  
  # Creating the base folder for the Spatial-wave
  base::dir.create("Spatial-wave")
  
  # Creating the folder to save the forecast files for the Spatial-wave
  base::dir.create("Spatial-wave/Forecasts")
  
  # Creating the folder to save the individual forecast graphs for the Spatial-wave
  base::dir.create("Spatial-wave/Graphs")
  
  # Creating the folder to save the metrics for the Spatial-wave
  base::dir.create("Spatial-wave/Metrics")
  
  # Creating the base folder for the Prophet
  base::dir.create("Prophet")
  
  # Creating the folder to save the forecast files for the Prophet
  base::dir.create("Prophet/Forecasts")
  
  # Creating the folder to save the crude forecast files for the Prophet
  base::dir.create("Prophet/Forecasts/All-Quantiles")
  
  # Creating the folder to save the formatted forecast files for the Prophet
  base::dir.create("Prophet/Forecasts/Formatted")
  
  # Creating the folder to save the individual forecast graph files for the Prophet
  base::dir.create("Prophet/Graphs")
  
  # Creating the folder to save the metrics for the Prophet
  base::dir.create("Prophet/Metrics")
  
  # Creating the base folder for the ARIMA
  base::dir.create("ARIMA")
  
  # Creating the folder to save the forecast files for the ARIMA
  base::dir.create("ARIMA/Forecasts")
  
  # Creating the folder to save the crude forecast files for the ARIMA
  base::dir.create("ARIMA/Forecasts/All-Quantiles")
  
  # Creating the folder to save the formatted forecast files for the ARIMA
  base::dir.create("ARIMA/Forecasts/Formatted")
  
  # Creating the folder to save the individual forecast graphs for the ARIMA
  base::dir.create("ARIMA/Graphs")
  
  # Creating the folder to save the metrics for the ARIMA
  base::dir.create("ARIMA/Metrics")
  
  # Creating the base folder for the SLR
  base::dir.create("SLR")
  
  # Creating the folder to save the forecast files for the SLR
  base::dir.create("SLR/Forecasts")
  
  # Creating the folder to save the crude forecast files for the SLR
  base::dir.create("SLR/Forecasts/All-Quantiles")
  
  # Creating the folder to save the formatted forecast files for the SLR
  base::dir.create("SLR/Forecasts/Formatted")
  
  # Creating the folder to save the individual forecast graph files for the SLR
  base::dir.create("SLR/Graphs")
  
  # Creating the folder to save the metrics for the SLR
  base::dir.create("SLR/Metrics")
  
  # Creating the base folder for the GAM
  base::dir.create("GAM")
  
  # Creating the folder to save the forecast files for the GAM
  base::dir.create("GAM/Forecasts")
  
  # Creating the folder to save the crude forecast files for the GAM
  base::dir.create("GAM/Forecasts/All-Quantiles")
  
  # Creating the folder to save the formatted forecast files for the GAM
  base::dir.create("GAM/Forecasts/Formatted")
  
  # Creating the folder to save the individual forecast graph files for the GAM
  base::dir.create("GAM/Graphs")
  
  # Creating the folder to save the metrics for the GAM
  base::dir.create("GAM/Metrics")
  
}



#------------------------------------------------------------------------------#
#                    Reading in data from the OWID Data                        #
#------------------------------------------------------------------------------#

owid <- read_csv(paste0("Orignal_Data/owid-monkeypox-data.csv")) %>% # Reading in the data
  dplyr::mutate(date = ymd(date)) %>% # Re-formatting the date column to YYYY-MM-DD
  dplyr::filter(date <= "2023-08-09") # Filtering dates associated with cases

#------------------------------------------------------------------------------#
#               Reading in the CDC data from the count folder                  #
#------------------------------------------------------------------------------#


cdc <- read_csv(paste0("Orignal_Data/CDC-08-09-2023.csv")) %>%
  dplyr::mutate(Epi_date_V3 = mdy(Epi_date_V3))


#------------------------------------------------------------------------------#
#                    Cleaning up the OWID source data                          #
#------------------------------------------------------------------------------#

owid_cases <- owid %>%
  # Selecting study areas of interest 
  dplyr::filter(location %in% c("United States", "Brazil", "Spain", "France", "United Kingdom", "Germany", "Canada", "World")) %>%
  dplyr::select(date, location, new_cases) %>% # Needed variables 
  dplyr::mutate(location = ifelse(location == "United States", "US(OWID)", location)) %>% # Re-labeling US cases 
  dplyr::rename("Day" = date, # Onset date of case per WHO definition 
                "Country" = location, # Country
                "Cases" = new_cases) # Confirmed, incident cases 


#------------------------------------------------------------------------------#
#                         Cleaning up the CDC data                             #
#------------------------------------------------------------------------------#

# Adding Country column, and changing names to match the OWID data
cdc_cases <- cdc %>%
  dplyr::mutate(Country = "US(CDC)") %>% # Adding the location label
  dplyr::rename("Day" = Epi_date_V3) %>% # Renaming the date column
  dplyr::select(Day, Country, Cases) # Selected needed variables 


#------------------------------------------------------------------------------#
#                      Merging the OWID and CDC Sources                        #
#------------------------------------------------------------------------------#

# Merging case data for OWID and CDC
case_data <- base::rbind(owid_cases, cdc_cases)

# Changing the format of the data from long to wide, and changing it from daily to weekly 
cases <- case_data %>%
  dplyr::mutate(Week = lubridate::floor_date(lubridate::ymd(Day), unit="week", week_start = getOption("lubridate.week.start", 4))) %>%
  dplyr::group_by(Week, Country) %>%
  dplyr::mutate(Cases = sum(Cases)) %>%
  dplyr::select(-Day) %>%
  dplyr::distinct(Week, .keep_all = T) %>%
  tidyr::spread(Country, Cases) %>%
  dplyr::filter(Week != "2022-04-28")

# Changing all NAs to zeros
cases[is.na(cases)] <- 0

# Saving the data frame with all labels
write.csv(cases, paste0("Inputs/OWID_CDC_Weekly_Counts_Combined.csv"), row.names = F)


#------------------------------------------------------------------------------#
#               Creating the 'input' data for the Sub-Epidemic Toolboxes       #
#------------------------------------------------------------------------------#

# Ungrouping data and re-naming for later sub-setting 
toolbox_inputs <- cases %>%
  dplyr::ungroup() # Un-grouping data for later analysis 

###########################################################
# Loop to create .txt files for 9-week calibration period #
###########################################################
for(i in 9:nrow(cases)){
  
  # Sub-setting the observed data from the first week through the indexed week
  data <- toolbox_inputs[1:i,]
  
  # Creating the forecast-date label for the file name (format = mm-dd-YYYY)
  forecast_period <- format(max(data$Week), "%m-%d-%Y")
  
  # Removing the week column 
  data_final <- data %>%
    dplyr::select(!Week)
  
  # Saving the input file for the sub-epidemic toolboxes 
  write.table(data_final, paste0("Inputs/9-Weeks/weekly-Mpox-cases-All-", forecast_period, ".txt"), append = FALSE, sep = " ", dec = ".", row.names = F, col.names = F)
  
}

############################################################
# Loop to create .txt files for 10-week calibration period #
############################################################
for(i in 10:nrow(cases)){
  
  # Sub-setting the observed data from the first week through the indexed week
  data <- toolbox_inputs[1:i,]
  
  # Creating the forecast-date label for the file name (format = mm-dd-YYYY)
  forecast_period <- format(max(data$Week), "%m-%d-%Y")
  
  # Removing the week column 
  data_final <- data %>%
    dplyr::select(!Week)
  
  # Saving the input file for the sub-epidemic toolboxes 
  write.table(data_final, paste0("Inputs/10-Weeks/weekly-Mpox-cases-All-", forecast_period, ".txt"), append = FALSE, sep = " ", dec = ".", row.names = F, col.names = F)
  
}


############################################################
# Loop to create .txt files for 11-week calibration period #
############################################################
for(i in 11:nrow(cases)){
  
  # Sub-setting the observed data from the first week through the indexed week
  data <- toolbox_inputs[1:i,]
  
  # Creating the forecast-date label for the file name (format = mm-dd-YYYY)
  forecast_period <- format(max(data$Week), "%m-%d-%Y")
  
  # Removing the week column 
  data_final <- data %>%
    dplyr::select(!Week)
  
  # Saving the input file for the sub-epidemic toolboxes 
  write.table(data_final, paste0("Inputs/11-Weeks/weekly-Mpox-cases-All-", forecast_period, ".txt"), append = FALSE, sep = " ", dec = ".", row.names = F, col.names = F)
  
}