function analysis_DMprogress_BICcomparison(do_further_exclu)

% calculate mean BIC to select the model & check evidence for variable PSEs
% also, exploratorily, use VBA to do random-effects model comparison

if nargin<1
    do_further_exclu = false;
end

BIC = readtable('data/exp2_BICtable.csv');
modelnames = {'additive_nolps','effmin_nolps',...
    'additive_lapse','effmin_lapse',...
    'choice_kernel'};
Nmodels = length(modelnames);

if do_further_exclu
    %exclude the weird PSEs
    excluPSEs = readtable('data/exclu_PSEs.csv').excluPSEs;
    BIC = BIC(~ismember(BIC.prolific_id,excluPSEs),:);
end

fprintf('N = %i \n',height(BIC));

% collapse across ptp
% Calculate mean differences between columns
differences = zeros(numel(modelnames), numel(modelnames));
for i = 1:numel(modelnames)
    for j = 1:numel(modelnames)
        if i ~= j
            differences(i, j) = round(mean(BIC.(modelnames{i}) - BIC.(modelnames{j})),2);
        end
    end
end

% Create a table to display the results
diffBICt = array2table(differences, 'VariableNames', modelnames, 'RowNames', modelnames);
disp('Difference in BIC (mean across participants)')
disp(diffBICt)

%do the means
meanBIC = nan(Nmodels,1);
seBIC = nan(Nmodels,1);
for i = 1:Nmodels
    allBICs(i,:) = BIC{:,modelnames{i}}';
    meanBIC(i,:) = mean(allBICs(i,:));
    seBIC(i,:) = std(allBICs(i,:))/sqrt(length(allBICs(i,:)));
end

%visual
figure;
bar(meanBIC)
hold on
errorbar(1:length(modelnames), meanBIC, seBIC, '.', 'LineWidth', 1)
hold off
legend('mean BIC across ptp','standard error','Location','best')
xticks(1:length(modelnames))
xticklabels(modelnames)
set(gca,'TickLabelInterpreter','none')
xlabel('Models')
ylabel('Mean BIC')
title('Comparison based on mean BIC across participants')
set(gcf,'Position',[440 438 891 360])

%bms:
lme = -allBICs/2;
[~,out] = VBA_groupBMC(lme);
pxp = out.pxp;
T = table(modelnames',pxp', 'VariableNames', {'model','PXP'});
disp(T)

figure;
bar(pxp)
hold on
yline(1,'k--')
hold off
xticklabels(modelnames)
set(gca,'TickLabelInterpreter','none')
ylabel('protected exceedance probability')
set(gcf,'Position',[81 496 1020 302])

end