function analysis_DMprogress_BICcomparison

BIC = readtable('data_exp1/exp1_BICtable.csv');
modelnames = {'additive_lapse','effmin_lapse',...
    'additive_nolps','effmin_nolps',...
    'choice_kernel'};
Nmodels = length(modelnames);

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

end