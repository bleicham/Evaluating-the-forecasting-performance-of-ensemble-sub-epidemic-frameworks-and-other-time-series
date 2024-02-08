%-------------------------------------------------------------------------%
%                                                                         %
%       Function to Run Performance Metrics for the ARIMA, GAM, SLR,      %
%                          and Prophet Models                             %
%                                                                         %
%-------------------------------------------------------------------------%
%-------------------------------------------------------------------------%
%              Authors: Gerardo Chowell, Amanda Bleichrodt                %
%-------------------------------------------------------------------------%
%-------------------------------------------------------------------------%
% About: This code loops through all 9-locations, horizons, forecast      %
% periods, and calibration periods for which forecasts were produced for  %
% in the R-code shared within this GITHUB. The code itself provides the   %
% performance metrics for the forecasts, included the mean squared error  %
% (MSE), mean absolute error (MAE), weighted interval scores (WIS), and   %
% and 95% Prediction Intervals (95% PI). This code should be run only     %
% after the `Data-Cleaning-and-Preparing.R` and `Forecasting-and-Observed-%
% Data.R` files have been run. The performance metrics will be outputted  %
% to the 'Metrics' folder produced by the R-code under each resepective   %
% model type.                                                             %
%-------------------------------------------------------------------------%

%-------------------------------%
% Looping through each location %
%-------------------------------%

% Locations included in forecasting for mpox
locationVector = ["Brazil" "Canada" "France" "Germany" "Spain" "United Kingdom" "US(CDC)" "US(OWID)" "World"];

% Looping through the locations 
for l = 1:9

    % Determining the location to pull forecast for
    location = locationVector{l};

%-----------------------------%
% Looping through model types %
%-----------------------------%

% Models from which mpox forecasts are produced  
models = ["GAM" "SLR" "ARIMA" "Prophet"];

% Looping through the models 
for m = 1:4

    % Determining the model to pull forecast for
    model = models{m}; 

    %-----------------------------------%
    % Looping through forecast horizons %
    %-----------------------------------%
    
    % Loop for forecasting horizon s
    for h = 1:4

    % Setting the forecast horizon
    forecastingperiod1 = h;
    
    %-------------------------------------%
    % Looping through calibration periods %
    %-------------------------------------%

    % Calibration periods that we have forecasts for
    calibration = {9 10 11};

    % Looping through the calibration periods 
    for c = 1:3
        
        % Setting the calibration period
        calibrationPeriod = calibration{c};

        %----------------------------------------------------------%
        % Setting the correct start and end date based on horizon  %
        % and calibration period.                                  %
        %----------------------------------------------------------%
        
        % Selecting the start date based on the calibration period 
        switch c

            case 1 % 9-Week calibration period 
                % Takes the string inserted above and transforms it to a date
                first_date_formatted = datetime('06-30-2022', "InputFormat", "MM-dd-yyyy");
            case 2 % 10-Week calibration period 
                % Takes the string inserted above and transforms it to a date
                first_date_formatted = datetime('07-07-2022', "InputFormat", "MM-dd-yyyy");
            case 3 % 11-Week calibration period 
                % Takes the string inserted above and transforms it to a date
                first_date_formatted = datetime('07-14-2022', "InputFormat", "MM-dd-yyyy");

        end 

        % Selecting the last date based on the forecasting horizon 
        switch h

            case 1 % 1-Week forecasting horizon 
                % Takes the string inserted above and transforms it to a date
                last_date_formatted = datetime('07-27-2023', "InputFormat", "MM-dd-yyyy");
            case 2 % 2-Week forecasting horizon 
                % Takes the string inserted above and transforms it to a date
                last_date_formatted = datetime('07-20-2023', "InputFormat", "MM-dd-yyyy"); 
            case 3 % 3-Week forecasting horizon 
                % Takes the string inserted above and transforms it to a date
                last_date_formatted = datetime('07-13-2023', "InputFormat", "MM-dd-yyyy"); 
            case 4 % 4-Week forecasting horizon 
                % Takes the string inserted above and transforms it to a date
                last_date_formatted = datetime('07-06-2023', "InputFormat", "MM-dd-yyyy"); 

        end 
        
        %-----------------------%
        % Looping through dates %
        %-----------------------%

        for date = first_date_formatted:7:last_date_formatted % Looping through dates of interest 

        % Setting the date variable
        date = string(datestr(datenum(date), "mm-dd-yyyy"));
        
        
        %--------------------------------%
        %  Reading in the forecast data  %
        %--------------------------------%

        % The produced warning is due to the presence of headers in the
        % data file. It can be ignored, as the function automatically
        % ignores headers, thus producing the warning. A further
        % description of the function can be found at: 
        % https://www.mathworks.com/help/matlab/ref/readtable.html.

        forecasts=readtable(strcat('./', model, '/Forecasts/All-Quantiles/', model, '-weekly-Mpox-Cases-calibration-', num2str(calibrationPeriod), '-horizon-', num2str(forecastingperiod1), '-', location, '-', date, '.csv')); % Reads in file from input folder on path 
        
        % Changes the tables to arrays to be worked with to produce metrics - solves header issue/warning
        forecasts = table2array(forecasts); 
        
        %------------------------------%
        % Reading in the observed data %
        %------------------------------%
        
        % Observed data is the files that contain the true counts of what 
        % happen during the forecast period of interest. 
        
        % Reading in the observed data: The produced warning is due to the
        % presence of headers in the data file. It can be ignored, as the 
        % function automatically ignores headers, thus producing the
        % warning. A further description of the function can be found at: 
        % https://www.mathworks.com/help/matlab/ref/readtable.html.
        
        datasa=readtable(strcat('./Observed/observed-weekly-mpox-cases-horizon-', num2str(forecastingperiod1), "-", date, ".csv"));
        
        % Changes the tables to arrays to be worked with - solves header issue/warning 
        datasa=table2array(datasa);
        
        % Subsetting the data based on the location of intrest: There is a
        % one added to the location or column index, as there is an initial
        % column of row numbers that need to be ignored.
        
        datas = datasa(:,l+1); 
        
        %-------------------------------------%
        % Setting up for running metrics code %
        %-------------------------------------%
        
        % Empty matrixes to fill in with metrics - RMSEFS, MSEFS, MAEFS, 
        % PIFS, and MISFS
        RMSEFS=[]; MSEFS=[]; MAEFS=[]; PIFS=[]; MISFS=[];
        
        % Empty matrix to fill in with metric - WISFS
        WISFS=[];
        
        % Used later in code when filling in matrixes 
        col1=1;
        
        %--------------------------------------------------------------------%
        % Loop running through observed and forecast data to produce metrics %
        %--------------------------------------------------------------------%
        
        % Vector i with elements spaced in an increment of the size of the forecast
        % period 
        for i=1:forecastingperiod1:forecastingperiod1*1 
            
            % Pulling the mean from the forecast data 
            mean1=forecasts(i:1:i+forecastingperiod1-1,1);
            
            % Pulling the 95% LB from the forecast data 
            lb1=forecasts(i:1:i+forecastingperiod1-1,11);
            
            % Pulling the 95% UB from the forecast data 
            ub1=forecasts(i:1:i+forecastingperiod1-1,end-1); 
            
            % Closing all open plot windows 
            close all
            
            %--------------------------------%
            % Plotting the forecasted values %
            %--------------------------------%
            
            % Creating an empty plot 
            figure(99)
            
            % Plotting the forecasted values 
            plot(mean1)
            
            % Holding axis steady for the plot 
            hold on
            
            % Variable that contains a range of numbers 1:Length of forecasts 
            dataFperiod=1:forecastingperiod1;
            
            % Creating a variable that contains the forecasted values 
            yfmean=mean1(dataFperiod,1);
            
            %----------------------------------%
            % Organizing the ground truth data %
            %----------------------------------%
            
            % Renaming the ground truth data 
            dataf=datas(dataFperiod,col1);
            
            %--------------------------------%
            % Plotting the ground truth data %
            %--------------------------------%

            % The ground truth data is plotted as red points 
            plot(dataf, 'ro');
            
            % Splitting date into repspective parts 
            date_split = strsplit(date,"-");  
            month = str2num(date_split(:,1)); % Month 
            day = str2num(date_split(:,2)); % Day 
            year = str2num(date_split(:,3)); % Year

            % Date for title of plot 
            caddate1=[year month day];
            datenum1=datenum(caddate1); % Transforming date to number 
            date_id = datetime(caddate1) + (col1-1)*7; % Transforming number to date 

            % Assigning title to plot 
            title(datestr(date_id))
            
            % Max forecasted value 
            max1 = max(dataf)
            
            %-------------------------------------%
            % Functions for RMSEF, MSEf, and MAEf %
            %-------------------------------------%
    
            RMSEf=sqrt(mean((dataf-yfmean).^2)); % RMSEf
            MSEf=mean((dataf-yfmean).^2); % MSEf
            MAEf=mean(abs(dataf-yfmean)); % MAEf 
            
            %----------------------------------%
            % 95% prediction coverage (95% PI) %
            %----------------------------------%
            
            % Lower prediction interval bound 
            Lt=lb1(dataFperiod,1);
            
            % Upper prediction interval bound 
            Ut=ub1(dataFperiod,1);
    
            % Finding data that falls between the lower and upper prediction bounds
            coverage1=find(dataf>=Lt & dataf<=Ut);
            
            % Function for calculating the 95% PI
            PIf=100*(length(coverage1)./length(dataf));
            
            % Calulating PI coverage 
            PIFS=[PIFS;[ forecastingperiod1 PIf]];
            
            %---------------------------%
            % Mean Interval Score (MIS) %
            %---------------------------%
    
            % Function for calculating MIS 
            MISf=mean([(Ut-Lt)+(2/0.05).*(Lt-dataf).*(dataf<Lt)+(2/0.05)*(dataf-Ut).*(dataf>Ut)]);
            
            % Calculating MIS
            MISFS=[MISFS;[  forecastingperiod1 MISf]];
            
            % Calculating RMSE
            RMSEFS=[RMSEFS;[  forecastingperiod1 RMSEf]];
            
            % Calculating MSE
            MSEFS=[MSEFS;[  forecastingperiod1 MSEf]];
            
            % Calculating MAE 
            MAEFS=[MAEFS;[  forecastingperiod1 MAEf]];
            
            %-----------------------------------------%
            % Calculating the Weighted Interval Score %
            %-----------------------------------------%

            % Alphas needed for calculating WIS
            alphas=[0.02 0.05 0.1:0.1:0.9]; %0.02, 0.05, 0.1,0.2,0.3,0.4,0.5,0.6, 0.7,0.8,0.9 
            
            % w0
            w0=1/2;
            
            % Starting value for sum1
            sum1=0;
            
            % Starting value for K
            K=length(alphas);
            
            % Empty matrix to fill in with WISF values 
            WISF=zeros(forecastingperiod1,1);

            % Determining inputs for WIS function 
            for j=1:1:forecastingperiod1
                
                colx=12;
        
                sum1=0;
                
                y=dataf(j);
                
                IS=zeros(length(alphas),1);
                
                % Determining K 
                for k=1:K
                                
                    alpha=alphas(k);
                    
                    [alpha/2  1-alpha/2]
                    
                    w_k=alpha/2;
                    
                    % (1-lpha)x100 % prediction coverage
                    Lt1=forecasts(i:1:i+forecastingperiod1-1,colx);
                    Ut1=forecasts(i:1:i+forecastingperiod1-1,colx+11);
          
                    Lt=Lt1(j);
                    Ut=Ut1(j);
                    
                    IS(k)=[(Ut-Lt)+(2/alpha).*(Lt-y).*(y<Lt)+(2/alpha)*(y-Ut).*(y>Ut)];
                    
                    sum1=sum1+w_k*IS(k);
                    
                    colx=colx-1;
        
                end % End of loop determing K
                
                %median prediction, m
                %m=plims(curvesforecasts2(dataFperiod,:)',0.5)';
                m=mean1(j);
                
                % WISF function with inputed variables 
                WISF(j)=(1/(K+1/2))*(w0*abs(y-m) + sum1);
                
            end % End of loop that produces WIS function 
            
            %---------------%
            % Calculate WIS %
            %---------------%
            WISFS=[WISFS;[forecastingperiod1 mean(WISF)]];
            
            % Col1 increase 
            col1=col1+1;
        
        end % End of loop that produces individual metrics 
        
        %---------------------------------------%
        % Compiling the metrics into one matrix %
        %---------------------------------------%
        metrics=[mean(MSEFS(:,2)) mean(MAEFS(:,2)) mean(PIFS(:,2)) mean(WISFS(:,2))]
        
        %--------------------%
        % Saving the metrics %
        %--------------------%
        
        % Saving overall results of the code and write performance metrics to a
        % .csv file 
        
        writematrix(metrics, strcat('./', model, '/Metrics/', model, '-weekly-mpox-cases-calibration-', num2str(calibrationPeriod), '-horizon-', num2str(forecastingperiod1), '-', location, '-', date, '.csv'));
        
        end % End of date loop
    
      end % End of loop going through calibration periods 
    
    end % End of loop going through forecast horizons

  end % End of loop going through models

end % End of loop going through locations


