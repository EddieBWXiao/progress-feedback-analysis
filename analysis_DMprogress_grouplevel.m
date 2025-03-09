%function t = analysis_DMprogress_grouplevel

%% load relevant stuff
fitSaveDir = 'data/exp2_params.mat';
BICsaveDir = 'data/exp2_BICtable.csv';
fitted = load(fitSaveDir);
fitted = fitted.fitted;
BICs = readtable(BICsaveDir);

dptp = readtable('data/Exp2_PtpVarTable_postExclusion.csv');%participant variables
    %this would be having all the mental health data as well

paramsT = array2table(fitted.additive_nolps,'VariableNames',{'eqpoint','beta'});
paramsT.prolific_id = fitted.prolific_id';
% join the tables!
paramsT.prolific_id = cell2mat(paramsT.prolific_id);
t = join(paramsT,dptp,'Keys', 'prolific_id');
fprintf('IMPORTANT CHECK: n = %i \n',height(t))
disp(t)

writetable(t,'data/Exp2_PtpVarTable_full.csv');

