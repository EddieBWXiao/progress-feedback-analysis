function out_stat = get_bootstrapped_ci(x,n_samples,what_out)

% Bowen Xiao 2023

%only applicable for bootstrapping for a single estimator on
%a vector of numbers
%what_out: string for which statistic to use

if ~isvector(x)
    error('main input is not a vector')
else
    %check if x is a row vector
    if ~isrow(x)
        x = x'; %rotate any column vector
    end
end

n_total = numel(x);
bootstrap_samples = nan(n_samples, n_total);
for i = 1:n_samples
    % Generate a bootstrap sample by sampling with replacement
    bootstrap_indices = randi(n_total, [1, n_total]); %indices between 1~n_total, for n_total times (to sample with replacement)
    bootstrap_samples(i, :) = x(bootstrap_indices); %extract those samples and store in big mat
end

%for each of the rows (a bootstrapped sample)... calculate the statistic (e.g., mean)
boot_ests = mean(bootstrap_samples,2); %mean(,2) to mean over dim2
    %maybe generalise this so use a function handle, to loop over n_samples

%remember 95%CI is 2.5~97.5, else you are 90%CI
switch what_out
    case 'CIup'
        out_stat = prctile(boot_ests,97.5);
    case 'CIlow'
        out_stat = prctile(boot_ests,2.5);
end

end