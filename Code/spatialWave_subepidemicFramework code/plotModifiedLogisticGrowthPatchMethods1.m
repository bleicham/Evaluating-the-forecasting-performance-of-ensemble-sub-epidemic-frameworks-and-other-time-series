function objfunction=plotsir(z)

global flag1 method1 timevect ydata I0 npatches onset_thr typedecline1 yfit


global invasions
global timeinvasions
global Cinvasions

global npatches_fixed

global onset_fixed


r=z(1);
p=z(2);
a=z(3);
K=z(4);
q=z(5);

alpha=z(end-1);
d=z(end);

%[r p a K q]

%if onset_thr>K
    
%    npatches=1;

%else


if npatches_fixed<0
    
    
    npatches2=numberSubepidemics(K,onset_thr,q,typedecline1);
    
    npatches2=max(npatches2,1);
    
    npatchesX=min(npatches,npatches2);
    
else
    
    npatchesX=npatches;
    
end

%end


if onset_thr>K
    
   npatchesX=1;
   
end

invasions=zeros(npatchesX,1);
timeinvasions=zeros(npatchesX,1);
Cinvasions=zeros(npatchesX,1);

invasions(1)=1;
timeinvasions(1)=0;
Cinvasions(1)=0;



IC=zeros(npatchesX,1);

IC(1,1)=I0;

if npatchesX>1
    IC(2:end,1)=1;
end

[t,x]=ode15s(@modifiedLogisticGrowthPatch,timevect,IC,[],r,p,a,K,npatchesX,onset_thr,q,flag1,typedecline1);


y=sum(x,2);


totinc=[y(1,1);diff(y(:,1))];

if onset_fixed==0
    totinc(1)=totinc(1)-(npatchesX-1);
end

yfit=totinc;


eps=0.001;

%%MLE expression
%This is the negative log likelihood, name is legacy from least squares code
%Note that a term that is not a function of the params has been excluded so to get the actual
%negative log-likliehood value you would add: sum(log(factorial(sum(casedata,2))))

if sum(yfit)==0
    objfunction=10^10;%inf;
else
    %    z
    yfit(yfit==0)=eps; %set zeros to eps to allow calculation below.  Shouldn't affect solution, just keep algorithm going.
    
    
    switch method1
        case 0
            
            %Least squares
            objfunction=sum((ydata-yfit).^2);
            
        case 1
            %MLE Poisson (negative log-likelihood)
            objfunction=-sum(ydata.*log(yfit)-yfit);
            
        case 2
            %Pearson chi squred
            objfunction=sum(((ydata-yfit).^2)./yfit);
            
        case 3
            % MLE Negative binomial (negative log-likelihood) where sigma^2=mean+alpha*mean;
            
            sum1=0;
            
            for i=1:length(ydata)
                for j=0:(ydata(i)-1)
                    
                    sum1=sum1+log(j+(1/alpha)*yfit(i));
                    
                end
                sum1=sum1+ydata(i)*log(alpha)-(ydata(i)+(1/alpha)*yfit(i))*log(1+alpha);
                
            end
            
            objfunction=-sum1;
            
        case 4
            % MLE Negative binomial (negative log-likelihood) where sigma^2=mean+alpha*mean^2;
            
            sum1=0;
            
            for i=1:length(ydata)
                for j=0:(ydata(i)-1)
                    
                    sum1=sum1+log(j+(1/alpha));
                    
                end
                
                sum1=sum1+ydata(i)*log(alpha*yfit(i))-(ydata(i)+(1/alpha))*log(1+alpha*yfit(i));
                
            end
            
            objfunction=-sum1;
           
            
        case 5
            % MLE Negative binomial (negative log-likelihood) where sigma^2=mean+alpha*mean^d;
            
            sum1=0;
            
            for i=1:length(ydata)
                for j=0:(ydata(i)-1)
                    
                    sum1=sum1+log(j+(1/alpha)*yfit(i).^(2-d));
                    
                end
                
                sum1=sum1+ydata(i)*log(alpha*(yfit(i).^(d-2)).*yfit(i))-(ydata(i)+(1/alpha)*yfit(i).^(2-d))*log(1+alpha*(yfit(i).^(d-2)).*yfit(i));
                
            end
            
            objfunction=-sum1;
               
    end
    
    %     if ~isreal(objfunction)
    %         dbstop
    %     end
end


