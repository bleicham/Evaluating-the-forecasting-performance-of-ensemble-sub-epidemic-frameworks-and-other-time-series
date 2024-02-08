 
function [getperformance, deletetempfiles, forecastingperiod, weight_type1]=options_forecast

% <==============================================================================>
% <========================== Forecasting parameters ===================================>
% <==============================================================================>

getperformance=1; % flag or indicator variable (1/0) to indicate whether we want to compute the forecasting performance metrics

deletetempfiles=0; % flag or indicator variable (1/0) to indicate whether we want to delete Forecast..mat files from the output folder after use

forecastingperiod=30; % forecast horizon (number of time units ahead)

% <==============================================================================>
% <====================== weighting scheme for ensemble model ============================>
% <==============================================================================>

weight_type1=-1; % -1= equally weighted ensemble from the top models, 0=ensemble weighted based on AICc, 1= ensemble weighted based on relative likelihood (Akaike weights), 
% 2= ensemble weighted based on WISC during calibration, 3 = ensemble weighted based on WISF during forecasting performance at previous time period (week)

