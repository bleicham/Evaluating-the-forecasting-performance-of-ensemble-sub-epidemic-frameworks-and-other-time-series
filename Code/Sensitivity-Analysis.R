#------------------------------------------------------------------------------#
#                                                                              #
#                            Sensitivity Analysis                              #
#                                                                              #
#------------------------------------------------------------------------------#
# Author: Amanda Bleichrodt                                                    #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#                            Needed Packages                                   #
#------------------------------------------------------------------------------#
library(tidyverse)

#------------------------------------------------------------------------------#
#                           Reading in the data                                #
#------------------------------------------------------------------------------#

# Reading in the average metrics data 
average_metrics <- readr::read_csv("Forecast-Metrics/average-metrics-overall.csv")

#------------------------------------------------------------------------------#
#                       Determining Winning Calibration - MSE                  #
#------------------------------------------------------------------------------#

#####################################################
# Determining the MSE winners, and frequency of win #
#####################################################
winnersMSE <- average_metrics %>%
  dplyr::select(Location, Model, Calibration, Horizon, AvgMSE) %>% # Selecting the needed variables 
  dplyr::group_by(Location, Model, Horizon) %>% # Grouping by location, calibration, and horizon 
  dplyr::mutate(minMSE = base::min(AvgMSE), # Determining the minimum avg. MSE
                winner = base::ifelse(AvgMSE == minMSE, Calibration, 0), # Determining the "winning" model
                winner9 = base::ifelse(winner == 9, 1, 0), # Dummy variable for 9-week winner 
                winner10 = base::ifelse(winner == 10, 1, 0), # Dummy variable for 10-week winner
                winner11 = base::ifelse(winner == 11, 1, 0)) %>% # Dummy variable for 11-week winner
  dplyr::ungroup() %>% # Un-grouping by all variables 
  dplyr::group_by(Location) %>% # Grouping by location to find overall counts of "MSE" winners
  dplyr::mutate(totalWinner9 = base::sum(winner9), # Aggregate count of wins for 9-weeks 
                totalWinner10 = base::sum(winner10), # Aggregate count of wins for 10-weeks 
                totalWinner11 = base::sum(winner11)) %>% # Aggregate count of wins for 11-weeks 
  dplyr::select(Location, totalWinner9, totalWinner10, totalWinner11) %>% # Needed variables for going from wide-to-long
  tidyr::pivot_longer(-Location, names_to = "Calibration", values_to = "Wins") %>% # wide to long data
  dplyr::distinct(Location, Calibration, .keep_all = T) %>% # Removing repeat rows
  dplyr::ungroup() %>% # Ungrouping
  dplyr::mutate(PercentWins = (Wins/48)*100) # Percent wins 

########################
# Formatting for graph #
########################
MSEgraph <- winnersMSE %>%
  dplyr::mutate(Calibration = ifelse(Calibration == "totalWinner9", 9 , # Renaming the 9-week rows 
                                     ifelse(Calibration == "totalWinner10", 10, 11))) %>% # Renaming the 10- and 11-week rows 
  dplyr::group_by(Location) %>% # Grouping by location 
  dplyr::mutate(maxWins = base::max(Wins), # Determining which overall calibration period "won"
                colorWin = ifelse(Wins == maxWins, "red", "grey")) # Setting colors based on overall winner 
           

#########################################
# Plotting the overall avg. MSE winners #
#########################################
MSEWinners <- ggplot(data = MSEgraph, aes(x = Calibration, y = Wins, fill = colorWin)) +
  geom_bar(stat = "identity", position = "dodge") + # Bar graph 
  scale_fill_manual(values = c('grey50', '#C21807')) + # Setting colors of bars 
  labs(y = "Number of Wins", # Y-axis label 
       x = "Calibration Period") + # X-axis label 
  theme_bw() + # Base theme 
  theme(panel.grid.minor = element_blank(), # Removing minor background lines 
        panel.grid.major.x = element_blank(), # Removing major x-background lines 
        legend.position = "none", # Removing legend 
        axis.title.x = element_text(size = 10, family = "sans"), # Setting size and text font
        axis.title.y = element_text(size = 10, family = "sans"), # Setting size and text font
        strip.text.x = element_text(size = 10, family = "sans")) + # Setting size and text font in headers  
  facet_wrap(.~Location) # Faceting by calibration period 


###########################################################
# Saving the figures as a .tiff with journal requirements #
###########################################################
ggsave(MSEWinners, file = "Sensitivity-Analysis/MSEWinners_Supp.tiff", 
       compression = "lzw", width = 6, height = 6, units = "in", dpi = 900)


#------------------------------------------------------------------------------#
#                       Determining Winning Calibration - MAE                  #
#------------------------------------------------------------------------------#

#####################################################
# Determining the MAE winners, and frequency of win #
#####################################################
winnersMAE <- average_metrics %>%
  dplyr::select(Location, Model, Calibration, Horizon, AvgMAE) %>% # Selecting the needed variables 
  dplyr::group_by(Location, Model, Horizon) %>% # Grouping by location, calibration, and horizon 
  dplyr::mutate(minMAE = base::min(AvgMAE), # Determining the minimum avg. MAE
                winner = base::ifelse(AvgMAE == minMAE, Calibration, 0), # Determining the "winning" model
                winner9 = base::ifelse(winner == 9, 1, 0), # Dummy variable for 9-week winner 
                winner10 = base::ifelse(winner == 10, 1, 0), # Dummy variable for 10-week winner
                winner11 = base::ifelse(winner == 11, 1, 0)) %>% # Dummy variable for 11-week winner
  dplyr::ungroup() %>% # Un-grouping by all variables 
  dplyr::group_by(Location) %>% # Grouping by location to find overall counts of "MAE" winners
  dplyr::mutate(totalWinner9 = base::sum(winner9), # Aggregate count of wins for 9-weeks 
                totalWinner10 = base::sum(winner10), # Aggregate count of wins for 10-weeks 
                totalWinner11 = base::sum(winner11)) %>% # Aggregate count of wins for 11-weeks 
  dplyr::select(Location, totalWinner9, totalWinner10, totalWinner11) %>% # Needed variables for going from wide-to-long
  tidyr::pivot_longer(-Location, names_to = "Calibration", values_to = "Wins") %>% # wide to long data
  dplyr::distinct(Location, Calibration, .keep_all = T) %>% # Removing repeat rows 
  dplyr::ungroup() %>% # Ungrouping
  dplyr::mutate(PercentWins = (Wins/48)*100) # Percent wins


########################
# Formatting for graph #
########################
MAEgraph <- winnersMAE %>%
  dplyr::mutate(Calibration = ifelse(Calibration == "totalWinner9", 9 , # Renaming the 9-week rows 
                                     ifelse(Calibration == "totalWinner10", 10, 11))) %>% # Renaming the 10- and 11-week rows 
  dplyr::group_by(Location) %>% # Grouping by location 
  dplyr::mutate(maxWins = base::max(Wins), # Determining which overall calibration period "won"
                colorWin = ifelse(Wins == maxWins, "red", "grey")) # Setting colors based on overall winner 


#########################################
# Plotting the overall avg. MAE winners #
#########################################
MAEWinners <- ggplot(data = MAEgraph, aes(x = Calibration, y = Wins, fill = colorWin)) +
  geom_bar(stat = "identity", position = "dodge") + # Bar graph 
  scale_fill_manual(values = c('grey50', '#C21807')) + # Setting colors of bars 
  labs(y = "Number of Wins", # Y-axis label 
       x = "Calibration Period") + # X-axis label 
  theme_bw() + # Base theme 
  theme(panel.grid.minor = element_blank(), # Removing minor background lines 
        panel.grid.major.x = element_blank(), # Removing major x-background lines 
        legend.position = "none", # Removing legend 
        axis.title.x = element_text(size = 10, family = "sans"), # Setting size and text font
        axis.title.y = element_text(size = 10, family = "sans"), # Setting size and text font
        strip.text.x = element_text(size = 10, family = "sans")) + # Setting size and text font in headers  
  facet_wrap(.~Location) # Faceting by calibration period 


###########################################################
# Saving the figures as a .tiff with journal requirements #
###########################################################
ggsave(MAEWinners, file = "Sensitivity-Analysis/MAEWinners_Supp.tiff", 
       compression = "lzw", width = 6, height = 6, units = "in", dpi = 900)


#------------------------------------------------------------------------------#
#                       Determining Winning Calibration - WIS                  #
#------------------------------------------------------------------------------#

#####################################################
# Determining the MSE winners, and frequency of win #
#####################################################
winnersWIS <- average_metrics %>%
  dplyr::select(Location, Model, Calibration, Horizon, AvgWIS) %>% # Selecting the needed variables 
  dplyr::group_by(Location, Model, Horizon) %>% # Grouping by location, calibration, and horizon 
  dplyr::mutate(minWIS = base::min(AvgWIS), # Determining the minimum Avg. WIS
                winner = base::ifelse(AvgWIS == minWIS, Calibration, 0), # Determining the "winning" model
                winner9 = base::ifelse(winner == 9, 1, 0), # Dummy variable for 9-week winner 
                winner10 = base::ifelse(winner == 10, 1, 0), # Dummy variable for 10-week winner
                winner11 = base::ifelse(winner == 11, 1, 0)) %>% # Dummy variable for 11-week winner
  dplyr::ungroup() %>% # Un-grouping by all variables 
  dplyr::group_by(Location) %>% # Grouping by location to find overall counts of "WIS" winners
  dplyr::mutate(totalWinner9 = base::sum(winner9), # Aggregate count of wins for 9-weeks 
                totalWinner10 = base::sum(winner10), # Aggregate count of wins for 10-weeks 
                totalWinner11 = base::sum(winner11)) %>% # Aggregate count of wins for 11-weeks 
  dplyr::select(Location, totalWinner9, totalWinner10, totalWinner11) %>% # Needed variables for going from wide-to-long
  tidyr::pivot_longer(-Location, names_to = "Calibration", values_to = "Wins") %>% # wide to long data
  dplyr::distinct(Location, Calibration, .keep_all = T) %>% # Removing repeat rows 
  dplyr::ungroup() %>% # Un-grouping 
  dplyr::mutate(PercentWins = (Wins/48)*100) # Percent of wins

########################
# Formatting for graph #
########################
WISgraph <- winnersWIS %>%
  dplyr::mutate(Calibration = ifelse(Calibration == "totalWinner9", 9 , # Renaming the 9-week rows 
                                     ifelse(Calibration == "totalWinner10", 10, 11))) %>% # Renaming the 10- and 11-week rows 
  dplyr::group_by(Location) %>% # Grouping by location 
  dplyr::mutate(maxWins = base::max(Wins), # Determining which overall calibration period "won"
                colorWin = ifelse(Wins == maxWins, "red", "grey")) # Setting colors based on overall winner 


#########################################
# Plotting the overall avg. WIS winners #
#########################################
WISWinners <- ggplot(data = WISgraph, aes(x = Calibration, y = Wins, fill = colorWin)) +
  geom_bar(stat = "identity", position = "dodge") + # Bar graph 
  scale_fill_manual(values = c('grey50', '#C21807')) + # Setting colors of bars 
  labs(y = "Number of Wins", # Y-axis label 
       x = "Calibration Period") + # X-axis label 
  theme_bw() + # Base theme 
  theme(panel.grid.minor = element_blank(), # Removing minor background lines 
        panel.grid.major.x = element_blank(), # Removing major x-background lines 
        legend.position = "none", # Removing legend 
        axis.title.x = element_text(size = 10, family = "sans"), # Setting size and text font
        axis.title.y = element_text(size = 10, family = "sans"), # Setting size and text font
        strip.text.x = element_text(size = 10, family = "sans")) + # Setting size and text font in headers  
  facet_wrap(.~Location) # Faceting by calibration period 


###########################################################
# Saving the figures as a .tiff with journal requirements #
###########################################################
ggsave(WISWinners, file = "Sensitivity-Analysis/WISWinners_Supp.tiff", 
       compression = "lzw", width = 6, height = 6, units = "in", dpi = 900)



#------------------------------------------------------------------------------#
#                    Determining Winning Calibration - 95% PI                  #
#------------------------------------------------------------------------------#

#######################################################
# Determining the 95%PI winners, and frequency of win #
#######################################################
winners95PI <- average_metrics %>%
  dplyr::select(Location, Model, Calibration, Horizon, AvgPI) %>% # Selecting the needed variables 
  dplyr::mutate(diff95 = abs(95 - AvgPI)) %>%
  dplyr::group_by(Location, Model, Horizon) %>% # Grouping by location, calibration, and horizon 
  dplyr::mutate(minDiff = base::min(diff95), # Determining the smallest difference from 95 Avg. 95PI
                winner = base::ifelse(diff95 == minDiff, Calibration, 0), # Determining the "winning" model
                winner9 = base::ifelse(winner == 9, 1, 0), # Dummy variable for 9-week winner 
                winner10 = base::ifelse(winner == 10, 1, 0), # Dummy variable for 10-week winner
                winner11 = base::ifelse(winner == 11, 1, 0)) %>% # Dummy variable for 11-week winner
  dplyr::ungroup() %>% # Un-grouping by all variables 
  dplyr::group_by(Location) %>% # Grouping by location to find overall counts of "95%PI" winners
  dplyr::mutate(totalWinner9 = base::sum(winner9), # Aggregate count of wins for 9-weeks 
                totalWinner10 = base::sum(winner10), # Aggregate count of wins for 10-weeks 
                totalWinner11 = base::sum(winner11)) %>% # Aggregate count of wins for 11-weeks 
  dplyr::select(Location, totalWinner9, totalWinner10, totalWinner11) %>% # Needed variables for going from wide-to-long
  tidyr::pivot_longer(-Location, names_to = "Calibration", values_to = "Wins") %>% # wide to long data
  dplyr::distinct(Location, Calibration, .keep_all = T) %>% # Removing repeat rows 
  dplyr::ungroup()

########################
# Formatting for graph #
########################
PIgraph <- winners95PI %>%
  dplyr::mutate(Calibration = ifelse(Calibration == "totalWinner9", 9 , # Renaming the 9-week rows 
                                     ifelse(Calibration == "totalWinner10", 10, 11))) %>% # Renaming the 10- and 11-week rows 
  dplyr::group_by(Location) %>% # Grouping by location 
  dplyr::mutate(maxWins = base::max(Wins), # Determining which overall calibration period "won"
                colorWin = ifelse(Wins == maxWins, "red", "grey")) # Setting colors based on overall winner 


#########################################
# Plotting the overall avg. 95%PI winners #
#########################################
PIWinners <- ggplot(data = PIgraph, aes(x = Calibration, y = Wins, fill = colorWin)) +
  geom_bar(stat = "identity", position = "dodge") + # Bar graph 
  scale_fill_manual(values = c('grey50', '#C21807')) + # Setting colors of bars 
  labs(y = "Number of Wins", # Y-axis label 
       x = "Calibration Period") + # X-axis label 
  theme_bw() + # Base theme 
  theme(panel.grid.minor = element_blank(), # Removing minor background lines 
        panel.grid.major.x = element_blank(), # Removing major x-background lines 
        legend.position = "none", # Removing legend 
        axis.title.x = element_text(size = 10, family = "sans"), # Setting size and text font
        axis.title.y = element_text(size = 10, family = "sans"), # Setting size and text font
        strip.text.x = element_text(size = 10, family = "sans")) + # Setting size and text font in headers  
  facet_wrap(.~Location) # Faceting by calibration period 


###########################################################
# Saving the figures as a .tiff with journal requirements #
###########################################################
ggsave(PIWinners, file = "Sensitivity-Analysis/95%PIWinners_Supp.tiff", 
       compression = "lzw", width = 6, height = 6, units = "in", dpi = 900)