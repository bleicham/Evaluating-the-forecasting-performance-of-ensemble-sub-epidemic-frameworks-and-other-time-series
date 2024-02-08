
% <============================================================================>
% < Author: Gerardo Chowell  ==================================================>
% <============================================================================>

function [Phatss,npatches,onset_thr,typedecline1,curves,bestfit,data1,P0,AICc_best,RelLik_best,factor1,d]=fittingModifiedLogisticFunction(RMSES,relativelik_i,PS,data1,DT,epidemic_period,M,flagX,numstartpoints,rank1)


global flag1 timevect ydata yfit

global I0 npatches onset_thr typefit1

flag1=flagX;

global invasions
global timeinvasions
global Cinvasions

global npatches_fixed

global onset_fixed

global subepicurves

global method1
global dist1
global factor1

global smoothfactor1

global LBe UBe


close all

% <=============================================================================================>
% <========== Load previously inferred models according to AICc (best to worst fits) ===========>
% <=============================================================================================>

%load(strcat('./output/ABC-ensem-npatchesfixed-',num2str(npatches_fixed),'-onsetfixed-',num2str(onset_fixed),'-smoothing-',num2str(smoothfactor1),'-',datafilename1(1:end-4),'-flag1-',num2str(flag1(1)),'-flag1-',num2str(flag1(2)),'-method-',num2str(method1),'-dist-',num2str(dist1),'-calibrationperiod-',num2str(calibrationperiod1),'.mat'),'-mat')

% remove repeated rows
[RMSES,index1]=unique(RMSES,'rows','stable');
PS=PS(index1,:);

index1=rank1;

%[npatches,onset_thr,P0]

npatches=RMSES(index1,1);

onset_thr=RMSES(index1,2);

typedecline1=RMSES(index1,3);

AICc_best=RMSES(index1,4);
RelLik_best=relativelik_i(index1);

P0=PS(index1,:);

numparams=get_nparams(method1,dist1,npatches,flag1,1,onset_fixed);

close all

% fitting

Phats=[];

goodnessfit=[];

modelfits=[];

SSstats1=[];

SSstats2=[];

%data1=load(datafilename1)

data1=data1(epidemic_period,:);

data1(:,2)=data1(:,2);

I0=data1(1,2); % initial condition

if I0==0
    
    data1=data1(2:end,:);
    
end

data=data1(:,2);

[max1,index1]=max(data);

timevect=data1(:,1);

r=P0(1);

p=P0(2);

a=P0(3);

K=P0(4);

q=P0(5);

alpha=P0(end-1);

d=P0(end);


z(1)=r;% initial guesses
z(2)=p;
z(3)=a;
z(4)=K;
z(5)=q;
z(6)=alpha;
z(7)=d;

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
        LB=[0  1 0 20 0 LBe]; % lower bound for r p a K q
        UB=[1000  1 10 100000000 5 UBe]; % % upper bound for r p a K q
        
end



I0=data(1); % initial condition

hold on

%options=optimset('MaxFunEvals',3000,'MaxIter',3000,'Algorithm','trust-region-reflective','TolFun',1e-6,'TolX',1e-6);

%options=[];

%options = optimoptions('lsqcurvefit','UseParallel',true,...
%    'TolX',10^(-5),'TolFun',10^(-5));

%[P,resnorm,residual,exitflag,output,lambda,J]=lsqcurvefit(@plotModifiedLogisticGrowthPatch1,z,timevect,smooth(data,smoothfactor1),LB,UB,options,I0,npatches,onset_thr,flag1,typefit1);

% resnorm is the SSE which is given by sum(residual.^2)
% P is the vector with the estimated parameters


%method1=3; %LSQ=0, MLE Poisson=1, Pearson chi-squared=2, MLE (Neg Binomial)=3


ydata=smooth(data,smoothfactor1);

options=optimoptions('fmincon','Algorithm','sqp','StepTolerance',1.0000e-9,'MaxFunEvals',20000,'MaxIter',20000);

%options=optimoptions('fmincon','Algorithm','sqp','tolfun',10^-6,'TolX',10^-6,'MaxFunEvals',3200,'MaxIter',3200);

f=@plotModifiedLogisticGrowthPatchMethods1;

problem = createOptimProblem('fmincon','objective',f,'x0',z,'lb',LB,'ub',UB,'options',options);

%ms = MultiStart('PlotFcns',@gsplotbestf);
ms = MultiStart('Display','final');
%ms=MultiStart;

%rpoints = RandomStartPointSet('NumStartPoints',numstartpoints); % start with a few random starting sets in addition to the guess supplied by the user (z)

pts = z;
tpoints = CustomStartPointSet(z);
%allpts = {tpoints,rpoints};
allpts = {tpoints};

[P,fval,flagg,outpt,allmins] = run(ms,problem,allpts);


r_hat=P(1);
p_hat=P(2);
a_hat=P(3);
K_hat=P(4);
q_hat=P(5);

alpha_hat=P(end-1);
d_hat=P(end);

IC=zeros(npatches,1);

if onset_fixed==0
    IC(1,1)=I0;
    IC(2:end,1)=1;
    
    invasions=zeros(npatches,1);
    timeinvasions=zeros(npatches,1);
    Cinvasions=zeros(npatches,1);
    
    invasions(1)=1;
    timeinvasions(1)=0;
    Cinvasions(1)=0;
else
    IC(1:end,1)=I0./length(IC(1:end,1));
    
    invasions=zeros(npatches,1);
    timeinvasions=zeros(npatches,1);
    Cinvasions=zeros(npatches,1);
    
    invasions(1:end)=1;
    timeinvasions(1:end)=0;
    Cinvasions(1:end)=0;
end

[~,x]=ode15s(@modifiedLogisticGrowthPatch,timevect,IC,[],r_hat,p_hat,a_hat,K_hat,npatches,onset_thr,q_hat,flag1,typedecline1);

%[~,x]=ode15s(@modifiedLogisticGrowthPatch,timevect,IC,[],rs_hat,ps_hat,as_hat,Ks_hat,npatches,onset_thr,flag1,typedecline1);
%x=ode5(@modifiedLogisticGrowthPatch,timevect,IC,r_hat,p_hat,a_hat,K_hat,npatches,onset_thr,q_hat,flag1);


figure(10)

for j=1:npatches
    
    incidence1=[x(1,j);diff(x(:,j))];
    
    plot(timevect,incidence1)
    hold on
    
end

y=sum(x,2);

totinc=[y(1,1);diff(y(:,1))];

if onset_fixed==0
    totinc(1)=totinc(1)-(npatches-1);
end

bestfit=totinc;

% normal distribution of the error structure
if method1==0 & dist1==0

    var1=sum((bestfit-data).^2)./(length(bestfit)-numparams); % last revised: 16 Oct 2022
    factor1=sqrt(var1);

end


plot(timevect,totinc,'r')
hold on
plot(timevect,data,'bo')
xlabel('Time (days)');
ylabel('Incidence')


% <=============================================================================================>
% <======================== Vector with parameter estimates ====================================>
% <=============================================================================================>

Ptrue=[r_hat p_hat a_hat K_hat q_hat alpha_hat d_hat]

'generate simulation study to derive parameter uncertainty..'


% <=============================================================================================>
% <========== Generate parameter uncertainty via parametric bootstrapping ======================>
% <=============================================================================================>

yi=cumsum(totinc);

z=Ptrue; %r p a Ks alpha(MLE-Neg binomial)

Phatss=zeros(M,7);
SSEs=zeros(M,1);
curves=zeros(length(yi),M);

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

z(1)=r;% initial guesses
z(2)=p;
z(3)=a;
z(4)=K;
z(5)=q;
z(6)=alpha;
z(7)=d;


for real=1:M
    
    yirData=AddPoissonError(yi,1,dist1,factor1,d_hat);
    
    %
    %     yirData=zeros(length(yi),1);
    %
    %     yirData(1)=yi(1);
    %
    %     for t=2:length(yi)
    %         lambda=abs(yi(t)-yi(t-1));
    %         yirData(t,1)=poissrnd(lambda,1,1);
    %     end
    %
    curves(:,real)=yirData;
    
    I0=data(1); % initial condition
    
    hold on
    
    %options=optimset('MaxFunEvals',3000,'MaxIter',3000,'Algorithm','trust-region-reflective','TolFun',1e-5,'TolX',1e-5);
    
    %options=[];
    
    %options = optimoptions('lsqcurvefit','UseParallel',true,...
    %    'TolX',10^(-5),'TolFun',10^(-5));
    
    
    ydata=yirData;
    
    options=optimoptions('fmincon','Algorithm','sqp','StepTolerance',1.0000e-9,'MaxFunEvals',20000,'MaxIter',20000);
    
    f=@plotModifiedLogisticGrowthPatchMethods1;
    
    problem = createOptimProblem('fmincon','objective',f,'x0',z,'lb',LB,'ub',UB,'options',options);
    
    %ms = MultiStart('PlotFcns',@gsplotbestf);
    %ms = MultiStart('Display','final');
    ms = MultiStart('Display','off');

    %ms=MultiStart;
    
    rpoints = RandomStartPointSet('NumStartPoints',3); % start with a few random starting sets in addition to the guess supplied by the user (z)
    
    pts = z;
    tpoints = CustomStartPointSet(z);
    allpts = {tpoints,rpoints};
    %allpts = {tpoints};
    
    [P,fval,flagg,outpt,allmins] = run(ms,problem,allpts);
                
    Phatss(real,:)=P;
    
end
