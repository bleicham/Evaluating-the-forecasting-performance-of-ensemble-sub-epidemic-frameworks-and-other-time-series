% <============================================================================>
% < Author: Gerardo Chowell  ==================================================>
% <============================================================================>

function [cumulative1, outbreakx, caddate1, cadregion, caddisease,datatype, DT, datevecfirst1, datevecend1, numstartpoints,topmodelsx, B,flag1,typedecline2]=options

% parameter values for the Spatial wave sub-epidemic framework.

% <============================================================================>
% <=================== Declare global variables =======================================>
% <============================================================================>

global method1 %LSQ=0, MLE Poisson=1, Pearson chi-squared=2, MLE (Neg Binomial)=3,MLE (Neg Binomial)=4, MLE (Neg Binomial)=5
global npatches_fixed
global onset_fixed
global dist1
global smoothfactor1
global calibrationperiod1

% <============================================================================>
% <================================ Datasets properties =======================>
% <============================================================================>

% This section specifies the characteristics of the time series data, which will be used for inference purposes. 
% The data file is a text file with extension *.txt, located in the input folder.
% 
% The data file can contain one or more incidence curves (one per
% column in the file). Each column corresponds to the number of new cases over time for each epidemic corresponding to a different area/group. 
% For instance, each column could correspond to different states in the U.S or countries in the world. 
% In the options.m file, a specific data column in the file can be accessed using the parameter <outbreakx> (see below).
% 
% if the time series file contains cumulative incidence count data, the name of the time series data file starts with "cumulative" with the following format:
% 
% 'cumulative-<cadtemporal>-<caddisease>-<datatype>-<cadregion>-<caddate1>.txt');
% For example: 'cumulative-daily-coronavirus-deaths-USA-05-11-2020.txt'
% 
% Otherwise, if the time series file contains incidence data, the name of the data file follows the format:
% 
% <cadtemporal>-<caddisease>-<datatype>-<cadregion>-<caddate1>.txt');
% For example: 'daily-coronavirus-deaths-USA-05-11-2020.txt'
% 
% The following variables are specified in this section:


cumulative1=1; % This is a Boolean variable used to indicate if the data file contains cumulative incidence counts (cumulative1=1) or not (cumulative1=0).

outbreakx=52;  % This is an identifier for the spatial area of interest.

caddate1='05-11-2020';  % This is a string variable with the data file time stamp in format: mm-dd-yyyy

cadregion='USA'; % This is a string variable indicating the geographic region of the time series contained in the file (Georgia, USA, World, Asia, Africa, etc.)

caddisease='coronavirus'; % This is a string variable indicating the name of the disease related to the time series data.

datatype='cases'; % This is a string variable indicating the nature of the data (e.g., cases, deaths, hospitalizations).

DT=1; % This variable indicates the temporal resolution in days (e.g., 1=daily data, 7=weekly data, 365=yearly data).

if DT==1
    cadtemporal='daily';
elseif DT==7
    cadtemporal='weekly';
elseif DT==365
    cadtemporal='yearly';
end


datevecfirst1=[2020 02 27]; % This variable contains the date corresponding to the first data point in time series data in format [year_number month_number day_number].

datevecend1=[2021 05 31]; % This is the date of the most recent data file in format [year_number month_number day_number]. This data file is accessed to assess forecast performance.

% <============================================================================>
% <============================Adjustments to data =================================>
% <============================================================================>

% This section specifies variables relating to smoothing and the calibration period. 
% 
% The following variables are specified in this section:

smoothfactor1=7; % This variable indicates the span of the moving average smoothing of the case series (smoothfactor1=1 indicates no smoothing)

calibrationperiod1=90; % This variable indicates the number of most recent data points that will be used to calibrate the model. 
% If this value exceeds the length of the time series data, it will use the maximum length of the data.

% <=============================================================================>
% <=========================== Parameter estimation and bootstrapping=====================>
% <=============================================================================>

% This section specifies the parameter estimation method and the associated assumptions relating to the error structure in the data.   

% The following variables are specified in this section:

method1=0; % This integer variable indicates that parameter estimation method employed to estimate the parameters from data. The following estimation methods are available:

% method1=0; Nonlinear least squares (LSQ),
% method1=1; MLE Poisson=1,
% method1=3; MLE (Neg Binomial)=3, with VAR=mean+alpha*mean,
% method1=4; MLE (Neg Binomial)=4, with VAR=mean+alpha*mean^2,
% method1=5; MLE (Neg Binomial)=5, with VAR=mean+alpha*mean^d.


dist1=0; % This integer variable indicates the error structure assumptions. The following error structure assumptions are available:

% dist1=0; Normal distribution to model error structure (method1=0)
% dist1=1; Poisson error structure (method1=0 OR method1=1)
% dist1=2; Neg. binomial error structure where var = factor1*mean where
%            factor1 is empirically estimated from the time series data (method1=0)
% dist1=3; MLE (Neg Binomial) with VAR=mean+alpha*mean  (method1=3)
% dist1=4; MLE (Neg Binomial) with VAR=mean+alpha*mean^2 (method1=4)
% dist1=5; MLE (Neg Binomial)with VAR=mean+alpha*mean^d (method1=5)


numstartpoints=10; % This variable defines the number of different initial guesses for the optimization procedure using Multistart in its search for the globally optimal set of parameters.

B=300; % Number of bootstrap realizations utilized to characterize parameter uncertainty.

% <==============================================================================>
% <========================= Spatial wave sub-epidemic model ============================>
% <==============================================================================>

npatches_fixed=3; % This variable indicates the maximum number of sub-epidemics considered in the epidemic wave model fit.

topmodelsx=4; % This variable specifies the number of best fitting models (based on AICc) that will be generated to derive ensemble models. If npatches_fixed=1 then there is only one model.

if npatches_fixed==1  % if one sub-epidemic is employed, then there is only one model
    topmodelsx=1;
end

flag1=1; % This integer variable specifies the type of growth model used to model a subepidemic.

% 0 = GGM
% 1 = GLM
% 2 = GRM
% 3 = LM
% 4 = Richards

onset_fixed=0; % This variable indicates if the onset timing of subepidemics is fixed at time 0 (onset_fixed=1) or not (onset_fixed=0).

typedecline2=[2]; % This variable specifies the type of functional declines that will be considered for the sequential sub-epidemic sizes where typedecline2=1 for exponential decline in subepidemic size and typedecline2=2 for power-law decline in sub-epidemic size.
