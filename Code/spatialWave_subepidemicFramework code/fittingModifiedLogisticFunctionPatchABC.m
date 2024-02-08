% <============================================================================>
% < Author: Gerardo Chowell  ==================================================>
% <============================================================================>

function [RMSES,PS,npatches,onset_thr,typedecline1,P]=fittingModifiedLogisticFunctionPatchABC(datafilename1,data1,DT,epidemic_period,M,flagX,typedecline2,numstartpoints)

global flag1 method1 timevect ydata yfit

global I0 npatches onset_thr typedecline1

flag1=flagX;

close all

global invasions
global timeinvasions
global Cinvasions

global npatches_fixed

global onset_fixed

global dist1
global factor1

global smoothfactor1
global calibrationperiod1

global LBe UBe

% <=============================================================================================>
% <========= Set bounds for parameters associated with error structure (alpha and d) ===========>
% <=============================================================================================>

switch method1

    case 0
        LBe=[0 0];
        UBe=[0 0];
    case 1
        LBe=[0 0];
        UBe=[0 0];
    case 2
        LBe=[0 0];
        UBe=[0 0];
    case 3
        LBe=[10^-8 0];
        UBe=[10^5 0];
    case 4
        LBe=[10^-8 0];
        UBe=[10^5 0];
    case 5
        LBe=[10^-8 0.2];
        UBe=[10^5 10^2];
end

% <==============================================================================>
% <============ Load data and proceed to parameter estimation ===================>
% <==============================================================================>

%data1=load(datafilename1);

%data1=data1(:,2);

data1=data1(epidemic_period,:);

data1(:,2)=data1(:,2);

I0=data1(1,2); % initial condition

if I0==0
    data1=data1(2:end,:);
end

data=data1(:,2);


% <==============================================================================>
% <============ Set time vector (timevect) and initial condition (I0) ===========>
% <==============================================================================>

timevect=(data1(:,1));

I0=data(1); % initial condition

% <==============================================================================>
% <===================== Set initial parameter guesses ==========================>
% <==============================================================================>

r=0.2;

if flag1==3 %logistic model (p=1)
    p=1;
else
    p=0.9;
end

a=1;
K=sum(data1(:,2));

q=0.3;

if flag1==5

    r=1-I0/K;  % r=1-C0/K

    a=r/log(K/I0); % r/log(K/C0)
end

P0=[r p a K q 1 1];

% <==============================================================================>
% <================= Set range of C_thr values (onset_thrs) =====================>
% <==============================================================================>

% cum1=sum(data1(:,2));
% cum1=round(cum1);
% onset_thrs=unique(cumsum(data1(:,2)));
% index2=find(onset_thrs<=0.96*cum1);
% onset_thrs=onset_thrs(index2)';

% equal-witdh discretization of C_thr
cumcurve1=cumsum(smooth(data1(:,2),smoothfactor1));

onset_thrs=linspace(cumcurve1(1),cumcurve1(end),length(data1(:,2)));

onset_thrs=[0 onset_thrs(1:end-1)];

% <==============================================================================>
% <===== Set range of the possible number of subepidemics (1:npatches_fixed)=====>
% <==============================================================================>

RMSES=[];
PS=[];

if onset_fixed==1
    npatchess=1:1:npatches_fixed;
elseif onset_fixed==0
    npatchess=[1 npatches_fixed];
end


if (onset_fixed==1 | npatchess==1)

    onset_thrs=0;

end

onset_thrs2=onset_thrs;

entro1=0;

RMSES=sparse(1000,4);

PS=sparse(1000,7);

count1=1;

% <================================================================================================>
% <==== Evaluate AICc across models with different number of subepidemics and C_thr values ========>
% <================================================================================================>

ydata=smooth(data,smoothfactor1);


for npatches2=[npatchess]


    npatches=npatches2;


    if (onset_fixed==1 | npatches==1)

        onset_thrs=0;

    else
        onset_thrs=onset_thrs2;

    end

    % <================================================================================================>
    % <=========================== Set initial parameter guesses and bounds ===========================>
    % <================================================================================================>


    r=P0(1);
    p=P0(2);
    a=P0(3);
    K=P0(4);
    q=P0(5);
    alpha=P0(6);
    d=P0(7);


    z(1)=r;
    z(2)=p;
    z(3)=a;
    z(4)=K;
    z(5)=q;
    z(6)=alpha;
    z(7)=d;

    %z
    %[npatches onset_thr]

    rlb=mean(abs(data(1:2,1)))/200;
    rub=max(abs(data(1:2,1)))*5;

    switch flag1

        case 0 %GGM
            LB=[rlb  0 1  1 0 LBe]; % lower bound for r p a K q
            UB=[rub  1 1 1 0 UBe]; % % upper bound for r p a K q

        case 1 %GLM
            LB=[rlb  0 1 20 0 LBe]; % lower bound for r p a K q
            UB=[rub  1 1 100000000 5 UBe]; % % upper bound for r p a K q
            
        case 2 %GRM
            LB=[rlb  0 0 20 0 LBe]; % lower bound for r p a K q
            UB=[rub  1 10 100000000  5 UBe]; % % upper bound for r p a K q

        case 3 %Logistic
            LB=[rlb  1 1 20 0 LBe]; % lower bound for r p a K q
            UB=[rub 1 1 100000000 5 UBe]; % % upper bound for r p a K q

        case 4 %Richards
            LB=[rlb  1 0 20 0 LBe]; % lower bound for r p a K q
            UB=[rub  1 10 100000000 5 UBe]; % % upper bound for r p a K q

        case 5 %Gompertz
            LB=[0.0001  1 0 20 0 LBe]; % lower bound for r p a K q
            UB=[1000  1 10 100000000 5 UBe]; % % upper bound for r p a K q

    end



    for onset_thr=onset_thrs

        %options=[];

        for j=1:length(typedecline2)

            typedecline1=typedecline2(j);


            % ******** MLE estimation method  *********


            %method1=3; %LSQ=0, MLE Poisson=1, Pearson chi-squared=2, MLE (Neg Binomial)=3

            ydata=smooth(data,smoothfactor1);

            %'UseParallel','always'
            options=optimoptions('fmincon','Algorithm','sqp','StepTolerance',1.0000e-9,'MaxFunEvals',20000,'MaxIter',20000);

            %options=optimoptions('fmincon','Algorithm','sqp','tolfun',10^-6,'TolX',10^-6,'MaxFunEvals',20000,'MaxIter',20000);

            f=@plotModifiedLogisticGrowthPatchMethods1;

            problem = createOptimProblem('fmincon','objective',f,'x0',z,'lb',LB,'ub',UB,'options',options);

            %ms = MultiStart('PlotFcns',@gsplotbestf);
            %ms = MultiStart('Display','final');
            ms = MultiStart('Display','off');

            %ms=MultiStart;

            rpoints = RandomStartPointSet('NumStartPoints',numstartpoints); % start with a few random starting sets in addition to the guess supplied by the user (z)

            pts = z;
            tpoints = CustomStartPointSet(z);
            allpts = {tpoints,rpoints};
            %allpts = {tpoints};

            %z
            %list(tpoints)

            %ms = MultiStart(ms,'StartPointsToRun','bounds')
            %[xmin,fmin,flag,outpt,allmins] = run(ms,problem,allpts);

            [P,fval,flagg,outpt,allmins] = run(ms,problem,allpts);


            % --> numerical solver to get the best fit in order to check the actual number of
            % subepidemics involved in the best fit


            r_hat=P(1,1);
            p_hat=P(1,2);
            a_hat=P(1,3);
            K_hat=P(1,4);
            q_hat=P(1,5);

            alpha_hat=P(1,end-1);
            d_hat=P(1,end);

            IC=zeros(npatches,1);

            IC(1,1)=I0;
            IC(2:end,1)=1;


            invasions=zeros(npatches,1);
            timeinvasions=zeros(npatches,1);
            Cinvasions=zeros(npatches,1);

            invasions(1)=1;
            timeinvasions(1)=0;
            Cinvasions(1)=0;

            [~,x]=ode15s(@modifiedLogisticGrowthPatch,timevect,IC,[],r_hat,p_hat,a_hat,K_hat,npatches,onset_thr,q_hat,flag1,typedecline1);
            %x=ode5(@modifiedLogisticGrowthPatch,timevect,IC,r_hat,p_hat,a_hat,K_hat,npatches,onset_thr,q_hat,flag1);

            if sum(invasions)<npatches

                npatches=sum(invasions);

            end

            %

            AICc=getAICc(method1,dist1,npatches,flag1(1),1,fval,length(ydata),onset_fixed);

            RMSES(count1,:)=[npatches onset_thr typedecline1 AICc];

            PS(count1,:)=P;

            count1=count1+1;

        end %typedecline1

    end
    
end

% <=============================================================================================>
% <======================== Sort the results by AICc (lowest to highest) =======================>
% <=============================================================================================>

RMSES=RMSES(1:count1-1,:);

PS=PS(1:count1-1,:);

RMSES=full(RMSES);
PS=full(PS);

[RMSES,index1]=sortrows(RMSES,[4 1]);

PS=PS(index1,:);

% plot best fit

[RMSE1, index1]=min(RMSES(:,4));

npatches=RMSES(index1,1);

onset_thr=RMSES(index1,2);

typedecline1=RMSES(index1,3);

AICmin=RMSES(index1,4);

relativelik_i=exp((AICmin-RMSES(:,4))/2);

AICc_best=RMSES(index1,4);

%-->If we have a series of AICc  (or wSSE) values from N different models sorted from lowest (best model)
%to highest (worst model), I am wondering if we could define a proper threshold criterion to drop models with associated AICc (or wSSE) value greater than some threshold criteria.

% -->Let AICmin denote the minimun AIC from several models.
% The quantity exp((AICmin − AICi)/2) is interpreted as the relative likelihood of model i.
% We can set an alpha (e.g., 0.05), drop the models with exp((AICmin − AICi)/2) smaller than alpha,
% and combine other models with weighted average,
% where the weight is proportional to exp((AICmin − AICi)/2). This is another way to assign weights,
% compared to 1/SSE.

AICmin=RMSES(1,4);

relativelik_i=exp((AICmin-RMSES(:,4))/2);


% <=============================================================================================>
% <============================ Get the best fit resultls ======================================>
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
%x=ode5(@modifiedLogisticGrowthPatch,timevect,IC,r_hat,p_hat,a_hat,K_hat,npatches,onset_thr,q_hat,flag1);


% <=============================================================================================>
% <================================= Plost best model fit ======================================>
% <=============================================================================================>


figure(100)

for j=1:npatches

    incidence1=[x(1,j);diff(x(:,j))];

    plot(timevect,incidence1)
    hold on

end

y=sum(x,2);

totinc=[y(1,1);diff(y(:,1))];

if onset_thr>0
    totinc(1)=totinc(1)-(npatches-1);
end

bestfit=totinc;

plot(timevect,totinc,'r')

hold on
plot(timevect,data,'bo')
xlabel('Time (days)');
ylabel('Incidence')

title('best fit')
[npatches onset_thr]


if (method1==0 & dist1==2)  % calculate the overdispersion factor

    %     [coef,ns]=getMeanVarLinear(data,totinc,6);
    %
    %     if coef>0
    %         factor1=coef;
    %     else


    % estimate dispersion in data
    binsize1=7; %4

    [ratios,~]=getMeanVarianceRatio(data,binsize1,2);  % **

    %[ratios,~]=getMeanVarianceRatio(data,binsize1,1);

    index1=find(ratios(:,1)>0);

    factor1=mean(ratios(index1,1));

    factor1

end

'ABC estimates:'
'[npatches onset_thr typedecline1]'

[npatches onset_thr typedecline1]


% <=============================================================================================>
% <===================================  Save the results  ======================================>
% <=============================================================================================>

save(strcat('./output/ABC-original-npatchesfixed-',num2str(npatches_fixed),'-onsetfixed-',num2str(onset_fixed),'-typedecline-',num2str(sum(typedecline2)),'-smoothing-',num2str(smoothfactor1),'-',datafilename1(1:end-4),'-flag1-',num2str(flag1(1)),'-method-',num2str(method1),'-dist-',num2str(dist1),'-calibrationperiod-',num2str(calibrationperiod1),'.mat'),'-mat')



