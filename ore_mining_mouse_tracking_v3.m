% code to analyse mouse tracking data from ore mining task
% written by Noham Wolpe
% edited by Bowen Xiao 2025 for portability & uploading to GitHub with OSF link

%% to load progress feedback experiment
files2read = dir('./data/Exp2MouseTrackingFinalPostExclusion/mouse_*.csv');

%% big loop around participants
subID = cell(length(files2read),1);
z_thresh = 3; % Threshold for z-score
table_all = [];
for iSub = 1:length(files2read)
    
    clearvars data progress_shown
    % read data
    data = readtable(fullfile(files2read(iSub).folder, files2read(iSub).name));
    
    parts = strsplit(files2read(iSub).name, '_');   % Split at the underscore
    subID{iSub} = strrep(parts{2}, '.csv', '');
    
    
    % create Routes cell
    [path, path_id] = findgroups(data.trialn);
    
    [~, ind] = unique(data.trialn, 'first');
    progress_shown = strncmp(data.progress_shown(ind), 'TRUE',4);
    
    Routes = {};
    velocity_x = {};
    velocity_y = {};
    
    smoothing_size = 4;
    max_velocity_x = nan(max(data.trialn),1);
    max_velocity_y = nan(max(data.trialn),1);
    velocity_slope = nan(max(data.trialn),1);
    var_x = nan(max(data.trialn),1);
    var_y = nan(max(data.trialn),1);
    var_velocity_x= nan(max(data.trialn),1);
    var_velocity_y= nan(max(data.trialn),1);
    effort_level = nan(max(data.trialn),1);
    sample_rate = nan(max(data.trialn),1);
    total_path_length = nan(max(data.trialn),1);
    clearvars table_sub
    
%     clf
%     hold on
    for iPath = 1:length(path_id)
        
        ind2copy = find(data.trialn==path_id(iPath));
        
        %         % Find unique values and their first occurrence indices
        %         [~, ~, idx] = unique(data.time);
        %
        %         % Count the occurrences of each unique value
        %         counts = accumarray(idx, 1);
        %
        %         % Identify indices of duplicates
        %         duplicateIndices = find(counts > 10);
        
        
        x = data.x(ind2copy);
        y = data.y(ind2copy);
        time = data.time(ind2copy);
        
        % averages across time duplicates
        G = findgroups(time);
        time = splitapply(@(x) x(1), time, G); %  take first timestamp
        x = splitapply(@mean, x, G); % average x
        y = splitapply(@mean, y, G); % average y
        
        
        %         Routes{iPath} = [data.time(ind2copy), data.x(ind2copy), data.y(ind2copy)];
        Routes{iPath} = [time, x, y];
        
        % down-sample to 1 in 5 samples
        %         Routes{iPath} = [Routes{iPath}(1:5:end,1), Routes{iPath}(1:5:end,2), Routes{iPath}(1:5:end,3)];
        
        % calculate sampling rate
        sample_rate(iPath) = length(x) / (time(end)/1000 - time(1)/1000);
        
        foo = diff(x) ./diff(time);
        %         foo = diff(Routes{iPath}(:,2)) ./ diff(Routes{iPath}(:,1));
        
        velocity_x{iPath} = smoothdata(foo, 'movmean', 6);
        velocity_x{iPath} = [0; velocity_x{iPath}];
        
        %         foo = diff(Routes{iPath}(:,3)) ./ diff(Routes{iPath}(:,1));
        foo = diff(y) ./diff(time);
        
        velocity_y{iPath} = smoothdata(foo, 'movmean', 6);
        velocity_y{iPath} = [0; velocity_y{iPath}];
        
        % Outlier detection and removal (e.g., using z-score)
        z_velocity_y = zscore(velocity_y{iPath});
        velocity_y{iPath}(z_velocity_y > z_thresh | z_velocity_y < -z_thresh) = NaN;
        
        % Interpolate to handle NaN values
        velocity_y{iPath} = fillmissing(velocity_y{iPath}, 'linear');
        
        % Calculate total path length for x and y
        path_length_x = sum(abs(diff(x)));
        path_length_y = sum(abs(diff(y)));
        total_path_length(iPath) = sum(sqrt(diff(x).^2 + diff(y).^2));
        
        % Store path lengths
        %         path_lengths_x(iPath) = path_length_x;
        %         path_lengths_y(iPath) = path_length_y;
        
        
        %         plot(velocity_y{iPath});
        
        max_velocity_x(iPath) = max(abs(velocity_x{iPath}));
        max_velocity_y(iPath) = max(abs(velocity_y{iPath}));
        var_velocity_x(iPath) = var(abs(velocity_x{iPath}));
        var_velocity_y(iPath) = var(abs(velocity_y{iPath}));
        var_x(iPath) = var(data.x(ind2copy) / data.window_width_effort(1));
        var_y(iPath) = var(data.y(ind2copy) / data.window_height_effort(1));
        effort_level(iPath) = mean(data.effort_level(ind2copy));
        
        %within trial velocity changes
        
        % Define the trial sections
        n_samples = length(x);
        first_30 = 1:round(0.3 * n_samples);
        middle_40 = round(0.3 * n_samples):round(0.7 * n_samples);
        last_30 = round(0.7 * n_samples):n_samples;
        
        % Compute average and max velocity in each section
        avg_velocity_first = mean(abs(velocity_y{iPath}(first_30)), 'omitnan');
        avg_velocity_last = mean(abs(velocity_y{iPath}(last_30)), 'omitnan');
%         avg_velocity_last(iPath) = mean(velocity_y{iPath}(last_30));
        
        % Compute velocity slope (linear regression)
        time_relative = [0, 1];
        avg_velocity = [avg_velocity_first, avg_velocity_last];
        b = polyfit(time_relative, avg_velocity, 1);
        velocity_slope(iPath) = b(1); % Slope of velocity over time
        
        
    end
    
    %     pause
    
    sub_rep = repmat(subID(iSub), length(effort_level),1);
    trial_number = transpose(1:length(effort_level));
    table_sub = table(sub_rep, max_velocity_x, max_velocity_y, var_x, var_y, effort_level, ...
        var_velocity_x, var_velocity_y, progress_shown, sample_rate,total_path_length, velocity_slope,trial_number);
    table_all = [table_all; table_sub];
    
    
    
end

%%
mdl = fitlme(table_all, 'velocity_slope ~ trial_number + effort_level + (effort_level + trial_number | sub_rep)')



%% run lmer

% Logit Function: logit(p) = log(p / (1 - p)) where p is the probability.
% Sigmoid Function (Inverse Logit): p = 1 / (1 + exp(-x)) where x is the linear predictor.
%

% for the PF+/PF- experiment
table_rating = readtable('./data/Exp2_ratings_table_postExclusion.csv');

table_all.sub_rep = str2double(table_all.sub_rep);

% only include relevant participants
[ids2copy] = intersect(table_all.sub_rep, table_rating.prolific_id);
table_rating = table_rating(ismember(table_rating.prolific_id, ids2copy),:);

% check the ids match
[~, i_rating, i_effort ] = intersect(table_rating.prolific_id, table_all.sub_rep);

table_all.rating = table_rating.rating;
epsilon = 1e-10; % A small value to avoid logit of 0 or 1
table_all.rating_clipped = min(max(table_rating.rating, epsilon), 1 - epsilon);
table_all.rating_logit = log(table_all.rating_clipped ./ (1 - table_all.rating_clipped));
table_all.rating_arcsine = asin(sqrt(table_rating.rating));
% table_all.rating_binomial = table_all.rating

table_all.rating_logit = log(table_rating.rating ./ (eps + 1 - table_rating.rating));
table_all.failure = strncmp(table_rating.isGoalReached,'FALSE',5);


%% for the PF+/PF- experiment
table_all.is_no_progress= table_rating.ChoseRef==0;

table_all_z= table_all;
table_all_z.effort_level = zscore(table_all_z.effort_level);
table_all_z.max_velocity_x= zscore(table_all_z.max_velocity_x);
table_all_z.max_velocity_y= zscore(table_all_z.max_velocity_y);
table_all_z.rating = zscore(table_all_z.rating);
table_all_z.total_path_length = zscore(table_all_z.total_path_length);
table_all_z.var_velocity_x = zscore(table_all_z.var_velocity_x);
table_all_z.var_velocity_y = zscore(table_all_z.var_velocity_y);

% z score all the continuous variables


% writetable(table_all, '~/Documents/ore_mining/table4Eddie.csv')

mdl = fitlme(table_all_z, 'rating ~ is_no_progress + failure + effort_level + total_path_length+ max_velocity_x + max_velocity_y + var_velocity_x + var_velocity_y + (1 | sub_rep)');
mdl.ModelCriterion.BIC

mdl = fitlme(table_all_z, 'rating ~ is_no_progress + failure + effort_level + total_path_length+ max_velocity_x + max_velocity_y + var_velocity_x + var_velocity_y + (effort_level | sub_rep)');
mdl.ModelCriterion.BIC

mdl = fitlme(table_all_z, 'rating ~ is_no_progress + failure + effort_level + total_path_length+ max_velocity_x + max_velocity_y + var_velocity_x + var_velocity_y + (effort_level + is_no_progress| sub_rep)');
mdl.ModelCriterion.BIC

% fitlme(table_all, 'rating ~ is_no_progress + failure + effort_level + total_path_length+ max_velocity_x + max_velocity_y + var_velocity_x + var_velocity_y + (failure + effort_level + is_no_progress| sub_rep)')
% mdl.ModelCriterion.BIC

size(mdl.randomEffects)


mdl = fitlme(table_all_z, 'rating ~ failure + is_no_progress + effort_level + (is_no_progress + effort_level | sub_rep)')

mdl = fitlme(table_all, 'max_velocity_y ~ is_no_progress + effort_level + (1 | sub_rep)')
mdl = fitlme(table_all, 'var_velocity_y ~ is_no_progress + effort_level + (1 | sub_rep)')
mdl = fitlme(table_all, 'total_path_length ~ is_no_progress + effort_level + (1 | sub_rep)')

mdl = fitglme(table_all_z, 'failure ~ is_no_progress + effort_level + (1 | sub_rep)','Distribution', "Binomial")

% mdl = fitglme(table_all, 'rating ~ is_with_progress + success + effort_level + total_path_length+ max_velocity_x + max_velocity_y + var_velocity_x + var_velocity_y + (1 | sub_rep)', 'Distribution', 'InverseGaussian')
resid = residuals(mdl);
qqplot(resid);

[h, pValue] = lillietest(resid)


% fitlme(table_all, 'effort_level ~ var_x + var_y + max_velocity_x + max_velocity_y + var_velocity_x + var_velocity_y + (1 | sub_rep)')
%
% fitlme(table_all, 'var_velocity_y ~ progress_shown + (1 | sub_rep)')

