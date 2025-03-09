function [negLL,p,p_choose_ref] = lik_effort_only_dm(free_params,features,choices,modelname)

% Bowen Xiao 2023

%likelihood function for simple effort DM
%choice between ref and non-ref
%where the difference between ref and non-ref can be...
%...an intrinsically motivating variable

% have different models with switch case

% structure inspired by https://github.com/ninarouhani/2021_RouhaniNiv/tree/main/models_RL_matlabCode


%% unpack parameters 

%task
T = size(features,1); %T: max trials; features, row is trial!
Nparms = length(free_params);

%fixed parameters (IMPORTANT: must set according to task)
extra_eff = 0.3; %in additive version, shifted by 30%

%free parameters
switch modelname
    case 'additive_lapse'
        eqpoint = free_params(1); %additive bias as preference for reference
        beta = free_params(2); %this should be the effort sensitivity
        lapse  = free_params(3); 
    case 'additive_nolps'
        eqpoint = free_params(1); %additive bias as preference for reference
        beta = free_params(2); %this should be the effort sensitivity
        lapse  = 0;
    case 'effmin_lapse'
        %effmin ~ always choose the offer that appears to have least effort
        eqpoint = 0;
        beta = free_params(1); %this should be the effort sensitivity
        lapse  = free_params(2); 
    case 'effmin_nolps'
        eqpoint = 0;
        beta = free_params(1); %this should be the effort sensitivity
        lapse  = 0; 
    case 'ideal_add_lapse'   
        %assume participants know about the +0.3 shift
        eqpoint = 0+extra_eff; %shift by extra
        beta = free_params(1); %this should be the effort sensitivity
        lapse  = free_params(2); 
    case 'ideal_add_nolps'   
        eqpoint = 0;
        beta = free_params(1); %this should be the effort sensitivity
        lapse  = 0; 
    case 'choice_kernel'
        beta_stick = free_params(1); % stickiness-related
        alpha_stick = free_params(2); % update the choice kernel
end

%important: calculating the decision variable!
switch modelname
    case {'additive_lapse','effmin_lapse','ideal_add_lapse','additive_nolps','effmin_nolps','ideal_add_nolps'}
        decision_var = features(:,1)-features(:,2);
        %only considers the difference in the offer (depth) presented!
    case 'choice_kernel'
        decision_var = NaN; %not applicable
        %maybe we should add choice_side_kernel later...
end

%% loop through trials; build up log likelihood
p = nan(T,1); % for the chosen option
    %one column (p of choosing the reference option, on that trial)
p_choose_ref = nan(T,1);
    
for t=1:T    
    
    switch modelname
        case 'choice_kernel'
            if t == 1
                p1 = 0.5; %equal chance of choosing either
                CK = [0,0]; %define the choice kernel
            else
                %alright I am gonna use the "update before p" here...
                actions_taken = [choices(t-1),1-choices(t-1)]; %look back to see what was done
                CK = CK + alpha_stick.*(actions_taken-CK); %update choice kernel
                p1 = exp(beta_stick*CK(1))/(exp(beta_stick*CK(1))+exp(beta_stick*CK(2))); %decide based on CK
            end
            
        otherwise
            
        %one-line decision-model is here:
        p1 = (1-lapse)./(1+exp(-beta*(decision_var(t)-eqpoint)))+lapse/2;
        
    end
    
    p_choose_ref(t) = p1; %record this 
    p2 = 1-p1; % probability of not choosing the ref option
    
    % get probability of the chosen action
    if choices(t) == 1
        p(t) = p1;
    elseif choices(t) == 0
        p(t) = p2;
    end   

end

% sum of log-probability(data | parameters)
loglik = sum(log(p+eps));
negLL = -loglik;

end