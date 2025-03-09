function [fitted_params, loglik, BIC, fitInfo] = fit_lik_effort_only_dm(features,choices,modelname)
    
% Bowen Xiao 2023
% structure inspired by https://github.com/ninarouhani/2021_RouhaniNiv/tree/main/models_RL_matlabCode


fitInfo.modelname = modelname;
fitInfo.n_ini = 10;

%switch logic in two places:
switch modelname
    case 'additive_lapse'
        fitInfo.X0 = '[rand exprnd(1)*(-1) rand]';
            %initial point for beta: exprnd(1) from the Collins's code,
            %but make it in the negative range
        fitInfo.LB = [-1 -inf 0];
        fitInfo.UB = [1 inf 1];
    case 'additive_nolps'
        fitInfo.X0 = '[rand exprnd(1)*(-1)]';
        fitInfo.LB = [-1 -inf];
        fitInfo.UB = [1 inf];
    case 'effmin_lapse'
        %only beta and lapse
        fitInfo.X0 = '[exprnd(1)*(-1) rand]';
        fitInfo.LB = [-inf 0];
        fitInfo.UB = [inf 1];
    case 'effmin_nolps'
        fitInfo.X0 = '[exprnd(1)*(-1)]';
        fitInfo.LB = -inf;
        fitInfo.UB = inf;
    case 'ideal_add_lapse'   
        %only beta and lapse
        fitInfo.X0 = '[exprnd(1)*(-1) rand]';
        fitInfo.LB = [-inf 0];
        fitInfo.UB = [inf 1];
    case 'ideal_add_nolps'   
        fitInfo.X0 = '[exprnd(1)*(-1)]';
        fitInfo.LB = -inf;
        fitInfo.UB = inf;
    case 'choice_kernel'
        %stickiness & alpha_stick
        fitInfo.X0 = '[exprnd(1)*(-1) rand]';
        fitInfo.LB = [-inf 0];
        fitInfo.UB = [inf 1];
        %stickiness can also be counter-sticki
end
n_params = length(fitInfo.LB);
fitInfo.n_params=n_params;

obFunc = @(params) lik_effort_only_dm(params,features,choices,modelname);%has three parameters

fittedCandidates = nan(fitInfo.n_ini,n_params);
allNegLL = nan(fitInfo.n_ini,1);
fitInfo.fminconsettings = optimoptions('fmincon', 'Display', 'off'); %just to turn off the displays

for round = 1:fitInfo.n_ini

    X0 = eval(fitInfo.X0); %use eval for the string, to get the random numbers
    UB = fitInfo.UB;
    LB = fitInfo.LB;
    [fittedCandidates(round,:), allNegLL(round,1)] = fmincon(obFunc, X0, [], [], [], [], LB, UB,[],fitInfo.fminconsettings);

end

[negLLmin,minNegLLindex] = min(allNegLL); %find attempt that most minimised negLL
fitted_params = fittedCandidates(minNegLLindex,:); % get parameters from that best fit

%% output BIC & stuff
loglik = -negLLmin;
BIC = -2*loglik + n_params*log(length(choices));
    
end