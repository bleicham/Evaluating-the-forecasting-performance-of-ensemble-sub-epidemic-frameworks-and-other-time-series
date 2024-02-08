function [Ks,Ktot,n]=getsubepidemicsizes(K0,C_thr,q,typedecline1)

if C_thr==inf
    Ks=K0;
    Ktot=K0;
    n=1;
elseif q==0
    n=18;
    %Ks=zeros(n,1)+K0;
    Ks=inf;
    Ktot=inf;

else


    %     if 0
    %         K=K0;
    %
    %         n=1;
    %         while C_thr<K
    %
    %             K=K0*exp(-q*(n));
    %
    %             if C_thr<K
    %                 n=n+1;
    %                 %Ks=[Ks;K];
    %             end
    %         end
    %
    %     end
    %

    switch typedecline1

        case 1
            n=floor(-(1/q)*log(C_thr/K0)+1);  %exponential decline

            Ktot=(K0*(1-exp(-q*n)))/(1-exp(-q));

            Ks=[];

            nfinite=100

            K=K0;
            for j=1:nfinite
                K=K0*exp(-q*(j-1));
                Ks=[Ks;K];
            end

        case 2

            n=floor((C_thr/K0).^(-1/q));     %inverse decline

            Ks=[];

            nfinite=100;

            K=K0;
            for j=1:nfinite
                K=K0*(1/j).^q;
                Ks=[Ks;K];
            end
    end

    Ktot=sum(Ks);

end


end
