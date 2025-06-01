function [fitted,BIC] = analysis_fit_DMprogress_exp1

%overall settings:
modelnames = {'additive_lapse','effmin_lapse',...
    'additive_nolps','effmin_nolps',...
    'choice_kernel'};
fitSaveDir = 'data_exp1/exp1_params.mat';
BICsaveDir = 'data_exp1/exp1_BICtable.csv';

keepIDs = readtable('data_exp1/keep_effort_not_failed.csv'); %just the IDs
ptpIDs = keepIDs.x;

%% loop through ptp & model

nptp = length(ptpIDs);
nmodels = length(modelnames);

loglik = nan(nptp,nmodels);
BIC = nan(nptp,nmodels);

%store free parameters... length differs for each model
fitted = struct();% Initialize an empty struct
for i = 1:numel(modelnames)
    fieldName = modelnames{i};% Loop over the field names and assign NaN matrices to the struct fields
    fitted.(fieldName) = [];
end

for ptp = 1:nptp
    %% wrangling: get each csv and find the model-relevant variables
    df = readtable(sprintf('data_exp1/Exp1DecisionMaking/DM%i.csv',ptpIDs(ptp))); %DM for all decision-making
    fprintf('fitting %s \n',ptpIDs(ptp))
    options = [df.record_refOffer, df.record_altOffer]; %always, always put record_refOffer first?
    choices = df.ChoseRef;
    EffortDiffOffers = options(:,1)-options(:,2);
    df.dEffort = round(EffortDiffOffers,2); %round to avoid diff error

    %% fit each model
    for j = 1:nmodels
    
        % ====== the actual fitting code ======
        [free_params,loglik(ptp,j),BIC(ptp,j)] = fit_lik_effort_only_dm(options,choices,modelnames{j}); %get the vector of free parameters
        fitted.(modelnames{j})(ptp,:) = free_params;
        fitted.prolific_id{ptp} = ptpIDs(ptp); %IMPORTANT: store ptp sequence... please line up... please no errors
            %maybe just store ptp age or something else as a double check?
        
    end
    
end

BICtable = array2table(BIC,'VariableNames',modelnames);
BICtable.prolific_id = ptpIDs; %IMPORTANT: store ptp names
save(fitSaveDir,'fitted')
writetable(BICtable,BICsaveDir)


end