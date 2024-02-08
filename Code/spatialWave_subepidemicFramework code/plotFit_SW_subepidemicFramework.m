% <============================================================================>
% < Author: Gerardo Chowell  ==================================================>
% <============================================================================>

function performance=plotFit_SW_subepidemicFramework(outbreakx_pass,caddate1_pass)

% Plot model fits and derive performance metrics during the calibration period for the best fitting models

% <============================================================================>
% <=================== Declare global variables =======================================>
% <============================================================================>

global invasions
global timeinvasions
global Cinvasions
global npatches_fixed
global onset_fixed

global method1 dist1 factor1 smoothfactor1 calibrationperiod1

% <============================================================================>
% <=================== Load parameter values supplied by user =================>
% <============================================================================>

% options.m
[cumulative1_INP, outbreakx_INP, caddate1_INP, cadregion_INP, caddisease_INP,datatype_INP, DT_INP, datevecfirst1_INP, datevecend1_INP, numstartpoints_INP,topmodelsx_INP, M_INP,flag1_INP,typedecline2_INP]=options

% <============================================================================>
% <================================ Dataset ======================================>
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


cadregion=cadregion_INP; % string indicating the region of the time series (USA, Chile, Mexico, Nepal, etc)

caddisease=caddisease_INP;

datatype=datatype_INP;

DT=DT_INP; % temporal resolution in days (1=daily data, 7=weekly data, 365=yearly data).

M=M_INP;

if DT==1
    cadtemporal='daily';
elseif DT==7
    cadtemporal='weekly';
elseif DT==365
    cadtemporal='yearly';
end


cadfilename2=strcat(cadtemporal,'-',caddisease,'-',datatype,'-',cadregion,'-state-',num2str(outbreakx),'-',caddate1);

datevecfirst1=datevecfirst1_INP; % date corresponding to the first data point in time series data

datevecend1=datevecend1_INP; % date of the most recent data file in format [year_number month_number day_number]. This data file is accessed to assess forecast performance


% <============================================================================>
% <============================Adjustments to data =================================>
% <============================================================================>

%smoothfactor1=1; % <smoothfactor1>-day rolling average of the case series

%calibrationperiod1=90; % calibrates model using the most recent <calibrationperiod1> days  where calibrationperiod1<length(data1)

% <=============================================================================>
% <=========================== Statistical method ====================================>
% <=============================================================================>

%method1=0; %Type of estimation method: 0 = nonlinear least squares

%dist1=0; % Normnal distribution to model error structure


% <==============================================================================>
% <========================= Growth model ==========================================>
% <==============================================================================>

npatchess2=npatches_fixed;  % maximum number of subepidemics considered in epidemic trajectory fit

%GGM=0;  % 0 = GGM
%GLM=1;  % 1 = GLM
%GRM=2;  % 2 = GRM
%LM=3;   % 3 = LM
%RICH=4; % 4 = Richards

flagss2=flag1_INP; % Growth model considered in epidemic trajectory

typedecline2=typedecline2_INP; % 1=exponential decline in subepidemic size; 2=power-law decline in subepidemic size

% <==============================================================================>
% <======== Number of best fitting models used to generate ensemble model ========================>
% <==============================================================================>

topmodels1=1:topmodelsx_INP;

% <=======================================================================================>
% <========== Initialize variables to store results across top-ranked models ===========================>
% <=======================================================================================>

RMSECSS=[];
MSECSS=[];
MAECSS=[];
PICSS=[];
MISCSS=[];
RMSEFSS=[];
MSEFSS=[];
MAEFSS=[];
PIFSS=[];
MISFSS=[];

WISCSS=[];
WISFSS=[];

quantilescs=[];
quantilesfs=[];

MCSES=[];

param_rs=[];
param_ps=[];
param_as=[];
param_K0s=[];
param_qs=[];
param_alphas=[];
param_ds=[];

numsubepidemicss=[];
totepisizess=[];

cc2=1;

AICc_rank1=[];
relativelik_rank1=[];

for rank1=topmodels1

    cc2=1;

    npatches_fixed=npatchess2


    flag1=flagss2;

    npatchess2

    % <========================================================================================>
    % <================================ Load model results ====================================>
    % <========================================================================================>

    load (strcat('./output/modifiedLogisticPatch-original-npatchesfixed-',num2str(npatches_fixed),'-onsetfixed-',num2str(onset_fixed),'-typedecline-',num2str(sum(typedecline2)),'-smoothing-',num2str(smoothfactor1),'-',cadfilename2,'-flag1-',num2str(flag1(1)),'-method-',num2str(method1),'-dist-',num2str(dist1),'-calibrationperiod-',num2str(calibrationperiod1),'-rank-',num2str(rank1),'.mat'))

    rank1
    AICc_rank1=[AICc_rank1;[rank1 AICc_best]];
    relativelik_rank1=[relativelik_rank1;[rank1 relativelik_i(rank1)]];


    timevect=(data1(:,1));

    % <========================================================================================>
    % <================================ Parameter estimates =========================================>
    % <========================================================================================>


    % Parameter values
    param_r=[mean(Phatss(:,1)) quantile(Phatss(:,1),0.025) quantile(Phatss(:,1),0.975)];

    param_p=[mean(Phatss(:,2)) quantile(Phatss(:,2),0.025) quantile(Phatss(:,2),0.975)];

    param_a=[mean(Phatss(:,3)) quantile(Phatss(:,3),0.025) quantile(Phatss(:,3),0.975)];

    param_K0=[mean(Phatss(:,4)) quantile(Phatss(:,4),0.025) quantile(Phatss(:,4),0.975)];

    param_q=[mean(Phatss(:,5)) quantile(Phatss(:,5),0.025) quantile(Phatss(:,5),0.975)];

    param_alpha=[mean(Phatss(:,6)) quantile(Phatss(:,6),0.025) quantile(Phatss(:,6),0.975)];

    param_d=[mean(Phatss(:,7)) quantile(Phatss(:,7),0.025) quantile(Phatss(:,7),0.975)];

    MCSE=[std(Phatss(:,1))/sqrt(M) std(Phatss(:,2))/sqrt(M) std(Phatss(:,3))/sqrt(M) std(Phatss(:,4))/sqrt(M) std(Phatss(:,5))/sqrt(M) std(Phatss(:,6))/sqrt(M) std(Phatss(:,7))/sqrt(M)];

    % store the parameters across top-ranked models
    param_rs=[param_rs;[rank1 param_r]];
    param_ps=[param_ps; [rank1 param_p]];
    param_as=[param_as; [rank1 param_a]];
    param_K0s=[param_K0s; [rank1 param_K0]];
    param_qs=[param_qs; [rank1 param_q]];
    param_alphas=[param_alphas;[rank1 param_alpha]];
    param_ds=[param_ds;[rank1 param_d]];

    MCSES=[MCSES;[rank1 MCSE]];

    cad1=strcat('r=',num2str(param_r(end,1),2),'(95%CI:',num2str(param_r(end,2),2),',',num2str(param_r(end,3),2),')');
    cad2=strcat('p=',num2str(param_p(end,1),2),'(95%CI:',num2str(param_p(end,2),2),',',num2str(param_p(end,3),2),')')
    cad3=strcat('a=',num2str(param_a(end,1),2),'(95%CI:',num2str(param_a(end,2),2),',',num2str(param_a(end,3),2),')')
    cad4=strcat('K=',num2str(param_K0(end,1),3),'(95%CI:',num2str(param_K0(end,2),3),',',num2str(param_K0(end,3),3),')')
    cad5=strcat('q=',num2str(param_q(end,1),3),'(95%CI:',num2str(param_q(end,2),3),',',num2str(param_q(end,3),3),')')

    cad6=strcat('alpha=',num2str(param_alpha(end,1),3),'(95%CI:',num2str(param_alpha(end,2),3),',',num2str(param_alpha(end,3),3),')')

    cad7=strcat('d=',num2str(param_d(end,1),3),'(95%CI:',num2str(param_d(end,2),3),',',num2str(param_d(end,3),3),')')


    figure(100+rank1)
    timevect=data1(:,1);

    subplot(2,4,1)
    hist(Phatss(:,1))
    xlabel('r')
    ylabel('Frequency')
    title(cad1)

    hold on

    line2=[param_r(1,2) 10;param_r(1,3) 10];

    line1=plot(line2(:,1),line2(:,2),'r--')
    set(line1,'LineWidth',2)

    axis([param_r(1,2)-0.05 param_r(1,3)+0.05 0 120])

    set(gca,'FontSize', 16);
    set(gcf,'color','white')


    subplot(2,4,2)
    hist(Phatss(:,2))
    xlabel('p')
    hold on
    title(cad2)

    line2=[param_p(1,2) 10;param_p(1,3) 10];

    line1=plot(line2(:,1),line2(:,2),'r--')
    set(line1,'LineWidth',2)

    axis([param_p(1,2)-0.025 param_p(1,3)+0.025 0 120])

    set(gca,'FontSize', 16);
    set(gcf,'color','white')


    subplot(2,4,3)
    hist(Phatss(:,4))
    xlabel('K')
    hold on
    title(cad4)

    line2=[param_K0(1,2) 10;param_K0(1,3) 10];

    line1=plot(line2(:,1),line2(:,2),'r--')
    set(line1,'LineWidth',2)

    axis([param_K0(1,2)-50 param_K0(1,3)+50 0 120])

    set(gca,'FontSize', 16);
    set(gcf,'color','white')

    subplot(2,4,4)
    hist(Phatss(:,5))
    xlabel('q')
    hold on
    title(cad5)

    line2=[param_q(1,2) 10;param_q(1,3) 10];

    line1=plot(line2(:,1),line2(:,2),'r--')
    set(line1,'LineWidth',2)

    axis([max(param_q(1,2)-0.1,0) param_q(1,3)+0.1 0 200])

    set(gca,'FontSize', 16);
    set(gcf,'color','white')

    % <=========================================================================================>
    % <===================== Get performance metrics during calibration period================================>
    % <=========================================================================================>

    [RMSECS_model1 MSECS_model1 MAECS_model1  PICS_model1 MISCS_model1]=computeforecastperformance(data1,data1,curves,curves,0);

    [WISC,WISFS]=computeWIS(data1,data1,curves,0);

    % store metrics for calibration
    RMSECSS=[RMSECSS;[rank1 outbreakx datenum(caddate1) RMSECS_model1(end,end)]];
    MSECSS=[MSECSS;[rank1 outbreakx datenum(caddate1) MSECS_model1(end,end)]];
    MAECSS=[MAECSS;[rank1 outbreakx datenum(caddate1) MAECS_model1(end,end)]];
    PICSS=[PICSS;[rank1 outbreakx datenum(caddate1) PICS_model1(end,end)]];
    MISCSS=[MISCSS;[rank1 outbreakx datenum(caddate1) MISCS_model1(end,end)]];

    WISCSS=[WISCSS;[rank1 outbreakx datenum(caddate1) WISC(end,end)]];


    % <=========================================================================================>
    % <===================== Plot model fit and 95% prediction intervals=====================================>
    % <=========================================================================================>

    subplot(2,4,5)
    plot(timevect,curves,'c-')
    hold on

    LB1=quantile(curves',0.025);
    LB1=(LB1>=0).*LB1;

    UB1=quantile(curves',0.975);
    UB1=(UB1>=0).*UB1;

    %h=area(timevect',[LB1' UB1'-LB1'])
    %hold on

    h(1).FaceColor = [1 1 1];
    h(2).FaceColor = [0 1 1]; %cyan   [0.8 0.8 0.8] --> light gray

    line1=plot(timevect,median(curves,2),'r--')

    set(line1,'LineWidth',2)

    line1=plot(timevect,LB1,'r--')
    set(line1,'LineWidth',2)

    line1=plot(timevect,UB1,'r--')
    set(line1,'LineWidth',2)

    line1=plot(timevect,data1(:,2),'ko')
    set(line1,'LineWidth',2)

    axis([0 length(timevect)-1 0 max(UB1)*1.2])


    ylabel(strcat(caddisease,{' '},datatype))

    xlabel('Time')

    set(gca,'FontSize', 16);
    set(gcf,'color','white')

    hold on

    % residuals

    subplot(2,4,[7 8])

    resid1=bestfit-data1(:,2);

    stem(timevect,resid1,'b')
    hold on

    %anscomberesid=(3/2)*(data1(:,2).^(2/3)-bestfit.^(2/3))./(bestfit.^(1/6))
    %stem(timevect,anscomberesid,'r')
    %hold on

    %plot(timevect,zeros(length(timevect),1)+1.96,'k--')
    %plot(timevect,zeros(length(timevect),1)-1.96,'k--')

    xlabel('Time')
    ylabel('Residuals')

    axis([timevect(1) timevect(end)+1 min(resid1)-1 max(resid1)+1])

    set(gca,'FontSize', 16);
    set(gcf,'color','white')

    % <============================================================================>
    % <========================plot sub-epidemic profile ==========================>
    % <============================================================================>

    figure(100+rank1)

    color1=['r-';'b-';'g-';'m-';'c-';'k-';'y-';'r-';'b-';'g-';'m-';'c-';'k-';'y-';'r-';'b-';'g-';'m-';'c-';'k-';'y-';'r-';'b-';'g-';'m-';'c-';'k-';'y-';'r-';'b-';'g-';'m-';'c-';'k-';'y-';];

    fittedcurves=zeros(length(timevect),M);

    % generate forecast curves from each bootstrap realization
    for realization=1:M


        r_hat=Phatss(realization,1);
        p_hat=Phatss(realization,2);
        a_hat=Phatss(realization,3);
        K_hat=Phatss(realization,4);
        q_hat=Phatss(realization,5);


        alpha_hat=Phatss(realization,6);
        d_hat=Phatss(realization,7);


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


        %npatches=Phatss(realization,8);

        IC=zeros(npatches,1);

        IC(1,1)=data1(1,2);
        IC(2:end,1)=1;

        invasions=zeros(npatches,1);
        timeinvasions=zeros(npatches,1);
        Cinvasions=zeros(npatches,1);

        invasions(1)=1;
        timeinvasions(1)=0;
        Cinvasions(1)=0;

        [~,x]=ode15s(@modifiedLogisticGrowthPatch,timevect,IC,[],r_hat,p_hat,a_hat,K_hat,npatches,onset_thr,q_hat,flag1,typedecline1);

        subplot(2,4,6)

        for j=1:npatches

            incidence1=[x(1,j);diff(x(:,j))];

            plot(timevect,incidence1,color1(j,:))

            hold on

        end

        y=sum(x,2);

        totinc=[y(1,1);diff(y(:,1))];

        totinc(1)=totinc(1)-(npatches-1);

        bestfit=totinc;

        fittedcurves(:,realization)=totinc;

        gray1=gray(10);

        plot(timevect,totinc,'color',gray1(7,:))
        %hold on
        plot(timevect,data1(:,2),'ko')

    end

    xlabel('\fontsize{16}Time');
    ylabel(strcat(caddisease,{' '},datatype))

    line1=plot(data(:,1),data(:,2),'ko')
    set(line1,'LineWidth',2)

    axis([timevect(1) timevect(end)+1 0 max(data(:,2))*1.3])

    set(gca,'FontSize',16)
    set(gcf,'color','white')

    title(strcat(num2ordinal(rank1),' Ranked Model'))

    % <========================================================================================>
    % <================================ Store model fit quantiles ======================================>
    % <========================================================================================>

    [quantilesc,quantilesf]=computeQuantiles(data1,curves,0);
    quantilescs=[quantilescs;quantilesc];


       % compute doubling times

    forecastingperiod=0;
    
    meandoublingtime=zeros(M,1);

    doublingtimess=zeros(30,M)+NaN;

    maxd=1;

    for j=1:M

        [tds,C0data,curve,doublingtimes]=getDoublingTimeCurve(max(fittedcurves(:,j),0),DT,0);

        doublingtimess(1:length(doublingtimes),j)=doublingtimes;

        if maxd<length(doublingtimes)
            maxd=length(doublingtimes);
        end

        meandoublingtime=[meandoublingtime;mean(doublingtimes)];
    end

    doublingtimess=doublingtimess(1:maxd,1:M);

    seq_doublingtimes=[];

    for j=1:maxd

        index1=find(~isnan(doublingtimess(j,:)));

        seq_doublingtimes=[seq_doublingtimes;[j mean(doublingtimess(j,index1)) quantile(doublingtimess(j,index1),0.025) quantile(doublingtimess(j,index1),0.975) length(index1)./M]];

    end

    seq_doublingtimes % [ith doubling, mean, 95%CI LB, 95%CI UB, prob. i_th doubling]

    % Mean doubling times
    dmean=mean(meandoublingtime);
    dLB=quantile(meandoublingtime,0.025);
    dUB=quantile(meandoublingtime,0.975);

    param_doubling=[dmean dLB dUB]

    % <=============================================================================================>
    % <============================== Save file with doubling time estimates =======================>
    % <=============================================================================================>

    T = array2table(seq_doublingtimes);
    T.Properties.VariableNames(1:5) = {'i_th doubling','db mean','db 95%CI LB','db 95% CI UB','prob. i_th doubling'};
    
    writetable(T,strcat('./output/doublingTimes-ranked(', num2str(rank1),')-onsetfixed-',num2str(onset_fixed),'-typedecline-',num2str(sum(typedecline2)),'-flag1-',num2str(flag1(1)),'-method-',num2str(method1),'-dist-',num2str(dist1),'-',cadtemporal,'-',caddisease,'-',datatype,'-',cadregion,'-area-',num2str(outbreakx),'-',caddate1,'.csv'))



    % histogram of the predicted distribution of the number of subepidemics and epidemic
    % wave size

    numsubepidemics1=[];

    totepisize1=[];

    for i=1:M

        [Ks,Ktot,n]=getsubepidemicsizesFinite(Phatss(i,4),onset_thr,Phatss(i,5),typedecline1);

        numsubepidemics1=[numsubepidemics1;n];

        totepisize1=[totepisize1;[sum(Ks)]];


    end

    'num subepidemics'
    param1=[median(numsubepidemics1) quantile(numsubepidemics1,0.025) quantile(numsubepidemics1,0.975)];

    'epidemic wave size'
    param2=[median(totepisize1(:,1)) quantile(totepisize1(:,1),0.025) quantile(totepisize1(:,1),0.975)];

    numsubepidemicss=[numsubepidemicss;[rank1 param1]];
    totepisizess=[totepisizess;[rank1 param2]];



    figure(200+rank1)

    subplot(1,2,1)

    hist( numsubepidemics1)
    hold on

    line2=[param1(1,2) 20;param1(1,3) 20];

    line1=plot(line2(:,1),line2(:,2),'r--')
    set(line1,'LineWidth',2)

    line3=[param1(1,1) 0;param1(1,1) 250];

    line1=plot(line3(:,1),line3(:,2),'r--')
    set(line1,'LineWidth',2)

    xlabel('Number of sub-epidemics')
    ylabel('Frequency')

    title(strcat(num2ordinal(rank1),' Ranked Model'))

    set(gca,'FontSize', 16);
    set(gcf,'color','white')


    subplot(1,2,2)

    hist(totepisize1(:,1))

    hold on

    line2=[param2(1,2) 20;param2(1,3) 20];

    line1=plot(line2(:,1),line2(:,2),'r--')
    set(line1,'LineWidth',2)

    line3=[param2(1,1) 0;param2(1,1) 250];

    line1=plot(line3(:,1),line3(:,2),'r--')
    set(line1,'LineWidth',2)

    %lineEpiSize=[sum(data(:,2)) 0;sum(data(:,2)) 250];

    %line1=plot(lineEpiSize(:,1),lineEpiSize(:,2),'r-+')
    %set(line1,'LineWidth',2)

    cad1=strcat('Epidemic wave size=',num2str(param2(end,1),2),'(95%CI:',num2str(param2(end,2),2),',',num2str(param2(end,3),2),')');

    title(cad1)

    xlabel('Total epidemic wave size')
    ylabel('Frequency')

    set(gca,'FontSize', 16);
    set(gcf,'color','white')

end

% <===============================================================================================>
% <=================plot calibration performance metrics for the top-ranked models ===============>
% <===============================================================================================>

figure(300)
subplot(2,2,1)
line1=plot(MAECSS(:,1),MAECSS(:,4),'k-o')
set(line1,'linewidth',2)
xlabel('i_{th}Ranked Model')
ylabel('MAE')

set(gca,'FontSize', 16);
set(gcf,'color','white')

subplot(2,2,2)
line1=plot(MSECSS(:,1),MSECSS(:,4),'k-o')
set(line1,'linewidth',2)
xlabel('i_{th}Ranked Model')
ylabel('MSE')

set(gca,'FontSize', 16);
set(gcf,'color','white')

subplot(2,2,3)
line1=plot(PICSS(:,1),PICSS(:,4),'k-o')
set(line1,'linewidth',2)
xlabel('i_{th}Ranked Model')
ylabel('Coverage of the 95% PI')

set(gca,'FontSize', 16);
set(gcf,'color','white')

subplot(2,2,4)
line1=plot(WISCSS(:,1),WISCSS(:,4),'k-o')
set(line1,'linewidth',2)
xlabel('i_{th}Ranked Model')
ylabel('WIS')

set(gca,'FontSize', 16);
set(gcf,'color','white')

% <=============================================================================================>
% <================= Save file with top-ranked models' performance metrics (calibration)============================>
% <=============================================================================================>

performance=[topmodels1' MAECSS(:,4) MSECSS(:,4) PICSS(:,4) WISCSS(:,4) AICc_rank1(:,2) relativelik_rank1(:,2)];

T = array2table(performance);
T.Properties.VariableNames(1:7) = {'i_th-ranked model','MAE','MSE','Coverage 95%PI','WIS','AICc','RelativeLikelihood'};
writetable(T,strcat('./output/performance-calibration-topRanked-onsetfixed-',num2str(onset_fixed),'-typedecline-',num2str(sum(typedecline2)),'-flag1-',num2str(flag1(1)),'-method-',num2str(method1),'-dist-',num2str(dist1),'-',cadtemporal,'-',caddisease,'-',datatype,'-',cadregion,'-area-',num2str(outbreakx),'-',caddate1,'.csv'))

% <============================================================================>
% <=================plot model parmeters for the top-ranked models =========================>
% <============================================================================>

figure(400)
subplot(3,2,1)
line1=plot(param_rs(:,1),param_rs(:,2:end),'-o')
set(line1,'linewidth',2)
xlabel('i_{th}Ranked Model')
ylabel('r')

set(gca,'FontSize', 16);
set(gcf,'color','white')

subplot(3,2,2)
line1=plot(param_ps(:,1),param_ps(:,2:end),'-o')
set(line1,'linewidth',2)
xlabel('i_{th}Ranked Model')
ylabel('p')

set(gca,'FontSize', 16);
set(gcf,'color','white')

subplot(3,2,3)
line1=plot(param_as(:,1),param_as(:,2:end),'-o')
set(line1,'linewidth',2)
xlabel('i_{th}Ranked Model')
ylabel('a')

set(gca,'FontSize', 16);
set(gcf,'color','white')

subplot(3,2,4)
line1=plot(param_K0s(:,1),param_K0s(:,2:end),'-o')
set(line1,'linewidth',2)
xlabel('i_{th}Ranked Model')
ylabel('K_0')

set(gca,'FontSize', 16);
set(gcf,'color','white')

subplot(3,2,5)
line1=plot(param_qs(:,1),param_qs(:,2:end),'-o')
set(line1,'linewidth',2)
xlabel('i_{th}Ranked Model')
ylabel('q')

set(gca,'FontSize', 16);
set(gcf,'color','white')


% <=============================================================================================>
% <================= Save file with top-ranked models' parameter estimates =====================================>
% <=============================================================================================>

if method1==3 | method1==4  %save parameter alpha. VAR=mean+alpha*mean; VAR=mean+alpha*mean^2;
    rollparams=[param_rs(:,1:end) param_ps(:,2:end) param_as(:,2:end) param_K0s(:,2:end) param_qs(:,2:end) param_alphas(:,2:end)];
    T = array2table(rollparams);
    T.Properties.VariableNames(1:19) = {'i_th-ranked model','r mean','r LB','r UB','p mean','p LB','p UB','a mean','a LB','a UB','K0 mean','K0 LB','K0 UB','q mean','q LB','q UB','alpha mean','alpha LB','alpha UB'};

    rollparams=[MCSES(:,1:7)];
    T2 = array2table(rollparams);
    T2.Properties.VariableNames(1:7) = {'i_th-ranked model','r MCSE','p MCSE','a MCSE','K0 MCSE','q MCSE','alpha MCSE'};

elseif method1==5
    rollparams=[param_rs(:,1:end) param_ps(:,2:end) param_as(:,2:end) param_K0s(:,2:end) param_qs(:,2:end) param_alphas(:,2:end) param_ds(:,2:end)];
    T = array2table(rollparams);
    T.Properties.VariableNames(1:22) = {'i_th-ranked model','r mean','r LB','r UB','p mean','p LB','p UB','a mean','a LB','a UB','K0 mean','K0 LB','K0 UB','q mean','q LB','q UB','alpha mean','alpha LB','alpha UB','d mean','d LB','d UB'};

    rollparams=[MCSES(:,1:8)];
    T2 = array2table(rollparams);
    T2.Properties.VariableNames(1:8) = {'i_th-ranked model','r MCSE','p MCSE','a MCSE','K0 MCSE','q MCSE','alpha MCSE','d MCSE'};

else
    rollparams=[param_rs(:,1:end) param_ps(:,2:end) param_as(:,2:end) param_K0s(:,2:end) param_qs(:,2:end)];
    T = array2table(rollparams);
    T.Properties.VariableNames(1:16) = {'i_th-ranked model','r mean','r LB','r UB','p mean','p LB','p UB','a mean','a LB','a UB','K0 mean','K0 LB','K0 UB','q mean','q LB','q UB'};

    rollparams=[MCSES(:,1:6)];
    T2 = array2table(rollparams);
    T2.Properties.VariableNames(1:6) = {'i_th-ranked model','r MCSE','p MCSE','a MCSE','K0 MCSE','q MCSE'};

end

writetable(T,strcat('./output/parameters-topRanked-onsetfixed-',num2str(onset_fixed),'-typedecline-',num2str(sum(typedecline2)),'-flag1-',num2str(flag1(1)),'-method-',num2str(method1),'-dist-',num2str(dist1),'-',cadtemporal,'-',caddisease,'-',datatype,'-',cadregion,'-area-',num2str(outbreakx),'-',caddate1,'.csv'))

writetable(T2,strcat('./output/MCSES-topRanked-onsetfixed-',num2str(onset_fixed),'-typedecline-',num2str(sum(typedecline2)),'-flag1-',num2str(flag1(1)),'-method-',num2str(method1),'-dist-',num2str(dist1),'-',cadtemporal,'-',caddisease,'-',datatype,'-',cadregion,'-area-',num2str(outbreakx),'-',caddate1,'.csv'))

% <========================================================================================>
% <============ Plot estimated number of sub-epidemics and total epidemic size across top-ranked models============>
% <========================================================================================>

figure(500)
subplot(1,2,1)
line1=plot(numsubepidemicss(:,1),numsubepidemicss(:,2:end),'-o')
set(line1,'linewidth',2)
xlabel('i_{th}Ranked Model')
ylabel('Number of sub-epidemics')

set(gca,'FontSize', 24);
set(gcf,'color','white')

subplot(1,2,2)
line1=plot(totepisizess(:,1),totepisizess(:,2:end),'-o')
set(line1,'linewidth',2)
xlabel('i_{th}Ranked Model')
ylabel('Total epidemic wave size')

set(gca,'FontSize', 24);
set(gcf,'color','white')
