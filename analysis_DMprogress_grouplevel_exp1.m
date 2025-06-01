function analysis_DMprogress_grouplevel_exp1

%group level analysis:

%% load relevant stuff
fitSaveDir = 'data_exp1/exp1_params.mat';
BICsaveDir = 'data_exp1/exp1_BICtable.csv';
fitted = load(fitSaveDir);
fitted = fitted.fitted;
BICs = readtable(BICsaveDir);

dptp = readtable('data_exp1/Exp1_PtpVarTable_postExclusion.csv');%participant variables
    %this would be having all the mental health data as well

paramsT = array2table(fitted.additive_nolps,'VariableNames',{'eqpoint','beta'});
paramsT.prolific_id = fitted.prolific_id';
% join the tables!
paramsT.prolific_id = cell2mat(paramsT.prolific_id);
t = join(paramsT,dptp,'Keys', 'prolific_id');
fprintf('IMPORTANT CHECK: n = %i \n',height(t))
disp(t)


writetable(t,'data_exp1/Exp1_PtpVarTable_full.csv');

end