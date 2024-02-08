% <============================================================================>
% < Author: Gerardo Chowell  ==================================================>
% <============================================================================>

function [AICcs,RelLik]=plotRankings_SW_subepidemicFramework(outbreakx_pass,caddate1_pass)

% Plot model fits for the best fitting models

% <============================================================================>
% <=================== Declare global variables ===============================>
% <============================================================================>

global invasions
global timeinvasions
global Cinvasions
global npatches_fixed
global onset_fixed

global method1 dist1 factor1

global smoothfactor1 calibrationperiod1


% <============================================================================>
% <=================== Load parameter values supplied by user =================>
% <============================================================================>

% options.m
[cumulative1_INP, outbreakx_INP, caddate1_INP, cadregion_INP, caddisease_INP,datatype_INP, DT_INP, datevecfirst1_INP, datevecend1_INP, numstartpoints_INP,topmodelsx_INP, M_INP,flag1_INP,typedecline2_INP]=options


% <============================================================================>
% <================================ Dataset ====================================>
% <============================================================================>

if exist('outbreakx_pass','var')==1

    outbreakx=outbreakx_pass;
else
    outbreakx=outbreakx_INP;

end

if exist('caddate1_pass','var')==1

    caddate1=caddate1_pass;
else
    caddate1=caddate1_INP;

end


cadregion=cadregion_INP;

caddisease=caddisease_INP;

datatype=datatype_INP;

DT=DT_INP; % temporal resolution in days (1=daily data, 7=weekly data).

if DT==1
    cadtemporal='daily';
elseif DT==7
    cadtemporal='weekly';
elseif DT==365
    cadtemporal='yearly';
end


cadfilename1=strcat(cadtemporal,'-',caddisease,'-',datatype,'-',cadregion,'-state-',num2str(outbreakx),'-',caddate1);


% <============================================================================>
% <============================Adjustments to data ============================>
% <============================================================================>

%smoothfactor1=3; % <smoothfactor1>-day smoothing

%calibrationperiod1=60; % calibrates model using the most recent <calibrationperiod1> days  where calibrationperiod1<length(data1)

% <=============================================================================>
% <=========================== Statistical method ==============================>
% <=============================================================================>

%method1=0;

%dist1=2;

% <==============================================================================>
% <========================= Mathematical model =================================>
% <==============================================================================>

%npatches_fixed=3;

flag1=flag1_INP;

typedecline2=typedecline2_INP; % 1=exponential decline in subepidemic size; 2=power-law decline in subepidemic size

% <==============================================================================>
% <======== Number of best fitting models used to generate ensemble model =======>
% <==============================================================================>

topmodels1=1:topmodelsx_INP;


AICc_bests=[];

for run_id=-1
    
    cc1=1;
    
    close all
    
    run_id
    
    if run_id==-1

        
        cadfilename2=strcat(cadtemporal,'-',caddisease,'-',datatype,'-',cadregion,'-state-',num2str(outbreakx),'-',caddate1);
        
    end
    
    
    load (strcat('./output/ABC-original-npatchesfixed-',num2str(npatches_fixed),'-onsetfixed-',num2str(onset_fixed),'-typedecline-',num2str(sum(typedecline2)),'-smoothing-',num2str(smoothfactor1),'-',cadfilename2,'-flag1-',num2str(flag1(1)),'-method-',num2str(method1),'-dist-',num2str(dist1),'-calibrationperiod-',num2str(calibrationperiod1),'.mat'))
    
    % remove repeated rows
    [RMSES,index1]=unique(RMSES,'rows','stable');
    PS=PS(index1,:);
    
    AICc_bests=[AICc_bests full(RMSES(topmodels1,4))];
    
    AICcs=RMSES(topmodels1,4);
    RelLik=relativelik_i(topmodels1);


    if 1  %plot relative likelihoods of the models
        
        figure(99)
        
        subplot(1,3,1)
        line1=plot(RMSES(topmodels1,4),'ko-')
        
        set(line1,'LineWidth',2)
        
        xlabel('i_{th} Ranked Model')
        ylabel('AICc')
        set(gca,'FontSize', 24);
        set(gcf,'color','white')
        
        
        subplot(1,3,2)
        
        line1=plot(relativelik_i(topmodels1),'ko-')
        
        set(line1,'LineWidth',2)
        
        xlabel('i_{th} Ranked Model')
        ylabel('Relative likelihood')
        set(gca,'FontSize', 24);
        set(gcf,'color','white')
        
        
        subplot(1,3,3)
        
        %deltas=AICc_bests-AICc_bests(1);
        
        line1=plot(1./relativelik_i(topmodels1),'ko-')
        
        set(line1,'LineWidth',2)
        
        xlabel('i_{th} Ranked Model')
        ylabel('Evidence ratio')
        set(gca,'FontSize', 24);
        set(gcf,'color','white')
        
        
    end



    figure(100)

    color1=['r-';'b-';'g-';'m-';'c-';'k-';'y-';'r-';'b-';'g-';'m-';'c-';'k-';'y-';'r-';'b-';'g-';'m-';'c-';'k-';'y-';'r-';'b-';'g-';'m-';'c-';'k-';'y-';'r-';'b-';'g-';'m-';'c-';'k-';'y-';];


    factors=factor(length(topmodels1));


    if length(factors)==1
        rows=factors;
        cols=1;

    elseif length(factors)==3
        rows=factors(1)*factors(2);
        cols=factors(3);
    else
        rows=factors(1);
        cols=factors(2);
    end



    for index1=topmodels1


        subplot(rows,cols,index1)

        npatches=RMSES(index1,1);

        onset_thr=RMSES(index1,2);

        typedecline1=RMSES(index1,3);


        AICc_best=RMSES(index1,4);
        
        
        % <=============================================================================================>
        % <============================ Get the best fit results =======================================>
        % <=============================================================================================>
        
        
        P=PS(index1,:);
        
        r_hat=PS(index1,1)
        p_hat=PS(index1,2)
        a_hat=PS(index1,3)
        K_hat=PS(index1,4)
        q_hat=PS(index1,5)
        
        alpha_hat=PS(index1,end-1);
        d_hat=PS(index1,end);
        
        if method1==3
            
            dist1=3; % VAR=mean+alpha*mean;
            
            factor1=alpha_hat;
            
        elseif method1==4
            
            dist1=4; % VAR=mean+alpha*mean^2;
            
            factor1=alpha_hat;
            
        elseif method1==5
            
            dist1=5; % VAR=mean+alpha*mean^2;
            
            factor1=alpha_hat;
            
        end
        
        
        IC=zeros(npatches,1);
        
        IC(1,1)=I0;
        IC(2:end,1)=1;
        
        
        invasions=zeros(npatches,1);
        timeinvasions=zeros(npatches,1);
        Cinvasions=zeros(npatches,1);
        
        invasions(1)=1;
        timeinvasions(1)=0;
        Cinvasions(1)=0;
        
        
        timevect
        
        npatches
        
        onset_thr
        
        flag1
        
        typedecline1
        
        
        [~,x]=ode15s(@modifiedLogisticGrowthPatch,timevect,IC,[],r_hat,p_hat,a_hat,K_hat,npatches,onset_thr,q_hat,flag1,typedecline1);
        
       
  
        for j=1:npatches
            
            incidence1=[x(1,j);diff(x(:,j))];
            
            line1=plot(timevect,incidence1,color1(j,:))
            
            set(line1,'Linewidth',2)
            hold on
            
        end
        
        y=sum(x,2);
        
        totinc=[y(1,1);diff(y(:,1))];
        
        if onset_thr>0
            totinc(1)=totinc(1)-(npatches-1);
        end
        
        bestfit=totinc;
        
        
        hold on
        line1=plot(timevect,data,'bo')
        set(line1,'Linewidth',2,'markerSize',8)
        
        
        line1=plot(timevect,totinc,'k')
        set(line1,'Linewidth',3)
        
        
        ylabel('Cases')
        
        set(gca,'FontSize',24)
        set(gcf,'color','white')
        
        
        title(strcat(num2ordinal(index1),{' '},' Ranked Model; AICc=',num2str(AICc_best,6)))
        
        legend(strcat('Sub-epidemics=',num2str(npatches),'; C_{thr}=',num2str(onset_thr)))
        
        %legend(strcat('Num. Sub-epidemics=',num2str(npatches)))
        
        
        [npatches onset_thr]
        
        
        if (method1==0 & dist1==2)  % calculate the overdispersion factor
            
            %     [coef,ns]=getMeanVarLinear(data,totinc,6);
            %
            %     if coef>0
            %         factor1=coef;
            %     else
            
            
            % estimate dispersion in data
            binsize1=7;
            
            %[ratios,~]=getMeanVarianceRatio(smooth(data,smoothfactor1),binsize1,2);
            [ratios,~]=getMeanVarianceRatio(data,binsize1,2);
            
            index1=find(ratios(:,1)>0);
            
            factor1=mean(ratios(index1,1));
            
            factor1
            
        end
        
        
        'ABC estimates:'
        '[npatches onset_thr q]'
        
        [npatches onset_thr]
        
        factor1
        
        
        
    end
end


for j=1:cols

    subplot(rows,cols,topmodels1(end-j+1))
    xlabel('Time');

end


% <=====================================================================================================>
% <============================== Save file with top-ranked models' AICc metrics =======================>
% <=====================================================================================================>

performanceTop=[topmodels1' RMSES(topmodels1,4) relativelik_i(topmodels1)];

T = array2table(performanceTop);
T.Properties.VariableNames(1:3) = {'i_th-ranked model','AICc','RelativeLikelihood'};
writetable(T,strcat('./output/AICc-topRanked-onsetfixed-',num2str(onset_fixed),'-typedecline-',num2str(sum(typedecline2)),'-flag1-',num2str(flag1(1)),'-method-',num2str(method1),'-dist-',num2str(dist1),'-',cadtemporal,'-',caddisease,'-',datatype,'-',cadregion,'-area-',num2str(outbreakx),'-',caddate1,'.csv'))


