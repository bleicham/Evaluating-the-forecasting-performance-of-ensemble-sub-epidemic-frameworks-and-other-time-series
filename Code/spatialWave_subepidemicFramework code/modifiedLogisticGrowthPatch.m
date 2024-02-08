% <============================================================================>
% < Author: Gerardo Chowell  ==================================================>
% <============================================================================>

function dx=modifiedLogisticGrowthPatch(t,x,r,p,a,K,npatches,onset_thr,q,flag1,typedecline1)

global invasions
global timeinvasions
global Cinvasions


dx=zeros(npatches,1);

switch flag1
    
    
    case 0 %GGM
        
        for j=1:npatches
            
            
            if invasions(j)==0
                
                invasions(j)=(x(j-1,1)>=onset_thr);
                timeinvasions(j)=invasions(j).*t;
                Cinvasions(j)=invasions(j).*x(j-1,1);
                
                if (invasions(j)==0)
                    break
                end
                
            end
            
            %             switch typedecline1
            %                 case 1
            %
            %                     K1=K*exp(-q*(j-1)); % exponential decline
            %
            %                 case 2
            %
            %                     K1=K*(1/j).^q; % inverse distance/gravity decline
            %             end
            
            dx(j,1)=invasions(j).*(r*(x(j,1).^p));
            
            
        end
        
        
    case 1 %GLM
        
        for j=1:npatches
            
            %if j<2
            
            %    onset_thr1=onset_thr;
            
            %else
            %    onset_thr1=onset_thr*exp(q*(j-2));
            
            %onset_thr1=K*(onset_thr./(onset_thr+(K-onset_thr)*exp(-q*(j-2))));
            
            %end
            
            
            if invasions(j)==0
                
                invasions(j)=(x(j-1,1)>=onset_thr);
                timeinvasions(j)=invasions(j).*t;
                Cinvasions(j)=invasions(j).*x(j-1,1);
                
                if (invasions(j)==0)
                    break
                end
                
            end
            
            switch typedecline1
                case 1
                    
                    K1=K*exp(-q*(j-1)); % exponential decline
                    
                case 2
                    
                    K1=K*(1/j).^q; % inverse distance/gravity decline
            end
            
            dx(j,1)=invasions(j).*(r*(x(j,1).^p)*(1-(x(j,1)/K1)));
            
            
        end
        
    case 2
        
        for j=1:npatches
            
            if invasions(j)==0
                
                
                invasions(j)=(x(j-1,1)>=onset_thr);
                timeinvasions(j)=invasions(j).*t;
                Cinvasions(j)=invasions(j).*x(j-1,1);
                
            end
            
            
            switch typedecline1
                case 1
                    
                    K1=K*exp(-q*(j-1)); % exponential decline
                    
                case 2
                    
                    K1=K*(1/j).^q; % inverse distance/gravity decline
            end
            
            
            dx(j,1)=invasions(j).*(r*(x(j,1).^p)*(1-(x(j,1)/K1)).^a);
            
            
        end
        
        
    case 3
        
        for j=1:npatches
            
            if invasions(j)==0
                
                invasions(j)=(x(j-1,1)>=onset_thr);
                timeinvasions(j)=invasions(j).*t;
                Cinvasions(j)=invasions(j).*x(j-1,1);
                
            end
            
            switch typedecline1
                case 1
                    
                    K1=K*exp(-q*(j-1)); % exponential decline
                    
                case 2
                    
                    K1=K*(1/j).^q; % inverse distance/gravity decline
            end
            
            dx(j,1)=invasions(j).*(r*(x(j,1))*(1-(x(j,1)/K1)));
            
            
        end
        
    case 4 %Richards
        
        for j=1:npatches
            
            if invasions(j)==0
                
                invasions(j)=(x(j-1,1)>=onset_thr);
                timeinvasions(j)=invasions(j).*t;
                Cinvasions(j)=invasions(j).*x(j-1,1);
                
            end
            
            switch typedecline1
                case 1
                    
                    K1=K*exp(-q*(j-1)); % exponential decline
                    
                case 2
                    
                    K1=K*(1/j).^q; % inverse distance/gravity decline
            end
            
            dx(j,1)=invasions(j).*(r*(x(j,1))*(1-(x(j,1)/K1).^a));
            
            
        end
        
     
         case 5 %Gompertz
        
        for j=1:npatches
            
            if invasions(j)==0
                
                invasions(j)=(x(j-1,1)>=onset_thr);
                timeinvasions(j)=invasions(j).*t;
                Cinvasions(j)=invasions(j).*x(j-1,1);
                
            end
            
            switch typedecline1
                case 1
                    
                    K1=K*exp(-q*(j-1)); % exponential decline
                    
                case 2
                    
                    K1=K*(1/j).^q; % inverse distance/gravity decline
            end
            
            dx(j,1)=invasions(j).*(r*(x(j,1))*exp(-a*t));
            
            
        end
        
        
end
