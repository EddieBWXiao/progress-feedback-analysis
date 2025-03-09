function out_t = staircase_ThreeChains_ptpSim_omni(realparams,fit_and_vis,modelname,settings)

% Bowen Xiao 2023, quick-and-dirty approach
% to help with simulation & param recovery requiring the staircased task

%run sim for a task with adaptive algorithm
%more flexible settingso
%omni = more flexible than _flex

%% default when without input
fixedparams.extra_eff = 0.3;
if nargin < 2
    fit_and_vis = false;
end
if nargin < 4
    settings.offerAdaptUnit = 0.05; 
    settings.ini_refs = [0.8,0.7,0.6];
    ini_alts = [0.5 0.3 0.7]; %to change to winner of parameter recovery as permutations of offer combinations
    settings.firstOffers = [settings.ini_refs;ini_alts]';
    settings.newStaircaseStarts = [0.3,0.4,0.5,0.6,0.7];
    settings.trials_per_staircase = 15;
    settings.chain_index = NaN;
end

%IMPORTANT: need to pass this bit to the small function... garghhhhhhhh
    %if I learn object-oriented programming will it be easier???
settings.modelname = modelname;
settings.fixedparams = fixedparams;

%task setting
n_trials = settings.trials_per_staircase;

ini_refs = settings.ini_refs;

settings.chain_index = 1;
t1 = staircase_local_onechain(realparams,n_trials,settings);

settings.chain_index = 2;
t2 = staircase_local_onechain(realparams,n_trials,settings);

settings.chain_index = 3;
t3 = staircase_local_onechain(realparams,n_trials,settings);

t_cells = {t1,t2,t3};
out_t = [t1;t2;t3];

%% ==========================================================================
%% ============ optional: further diagnostics, including fitting ============ 
if fit_and_vis
    disp(out_t)
    
    % extract the simulation
    choices_sim = out_t.chose_ref;
    offers = out_t.offers_list;
    
    %======the fitting part (fixed code here!!)======
    fitted = fit_lik_effort_only_dm(offers,choices_sim,modelname);

    %================================================
    
    %======visualisation======
    
    OutT = groupsummary(out_t, 'decision_var', 'mean', 'chose_ref'); %c.f., group_by %>% summarise()
    EffortDiffOffers = OutT.decision_var; %
    MeanChoice = OutT.mean_chose_ref;
    
    %% plot for sigmoid 
    figure;
    
    for refindex = 1:length(settings.ini_refs)
    
        subplot(length(settings.ini_refs),1,refindex)
        
        %painstakingly --> get the x-axix (difference in effort, but also
        %depends on effort of the reference option, as different panels)
        xt = linspace(-1,1,600); %important: this is the scale on the x axis for visualisation!
        x_experiment = offers(:,1)-offers(:,2);

        ref_fixed = settings.ini_refs(refindex);
        possible_refs = ref_fixed*ones(1,600); %ref held constant at a value
        example_offers = [possible_refs',possible_refs'-xt'];
            %only to create... the different combinations
            %so that when we put the example offers in, we get p_choice
            %on offers that the participants never chose
            %xt is the difference between ref & no-ref
        choices_placeholder = rand(length(xt),1);

        [~, ~, p_choice] = lik_effort_only_dm(realparams,example_offers,choices_placeholder,modelname);
        [~, ~, p_fitted] = lik_effort_only_dm(fitted,example_offers,choices_placeholder,modelname);

        plot(xt,p_choice,'-','LineWidth',2.5)
        hold on
        plot(EffortDiffOffers,MeanChoice,'*')  
        scatter(x_experiment,choices_sim, 'filled',... %the simulated choices as filled circles
            'MarkerFaceColor', 'blue',...
            'MarkerFaceAlpha', 0.2,... %
            'MarkerEdgeAlpha', 0); %do not show the edges
        plot(xt,p_fitted,'--','LineWidth',2.5)
        yline(0.5,'--')
        hold off
        ylim([0 1])
        ylabel("p(choose ref)")
        xlabel("△effort, Ref - NotRef")
        %plot the fit
        legend({'virtual participant','percentage choice','binary choices', 'fitted model'}, 'Location', 'SouthWest');
        %dispreal = string(round(realparams,2)); %=> problem: this round()...
            %yields the weirdest bug of all: Index in position 1 is invalid. Array indices must be positive integers or logical values.
        %dispfitted = string(round(fitted,2));
        %title(sprintf("real params: %s   \n fitted params: %s", join(dispreal, ', '), join(dispfitted, ', ')))
        title(sprintf("all ref option at %.2f", ref_fixed))
        
    end
    sgtitle(sprintf("real params: %s   \n fitted params: %s",join(string(realparams), ', '), join(string(fitted), ', ')))
    set(gcf,'Position',[440 43 601 755])
    
    %% plot the staircase
    figure;
    for chain = 1:length(t_cells)
        subplot(1,length(t_cells),chain)
        t_chain = t_cells{chain};
        plot(1:length(t_chain.decision_var),t_chain.decision_var,'-','LineWidth',2, 'DisplayName', '△effort on offer')
        hold on
        histogram(t_chain.decision_var,'Orientation','horizontal','EdgeColor','none','FaceAlpha',0.5, 'DisplayName', 'frequency of △effort offer')
        plot(1:length(t_chain.decision_var),t_chain.chose_ref-0.01,'r*', 'DisplayName', 'Chose ref')
        hold off
        % Set xlabel and ylabel with larger font size
        xlabel("trial (or frequency)")
        if chain == 1
            ylabel("△Effort (Ref - notRef)")
        end
        ylim([-1,1])
        title(sprintf('staircase No. %i, ref = %.2f \n range [%.2f, %.2f]',chain, ini_refs(chain), ini_refs(chain)-0.9, ini_refs(chain)))
    end
    set(gcf,'Position',[1 520 1440 278])
end    
%% ==========================================================================

end
function t_out = staircase_local_onechain(params,n_trials,settings)

% code to create the individual chain

%% pre-allocate
chose_ref = nan(n_trials,1);
p_choose_ref = nan(n_trials,1);
decision_var = nan(n_trials,1);

%% loop to generate the offers
offer = settings.firstOffers(settings.chain_index,:); %initial conditions!!
    %the settings.chain_index is a silly way to invoke the right firstOffer
    %it has been set in the main _flex function
    %and the input to settings.chain_index in the recovery etc...
    %should be NaN, to help find bugs easily
offers_list = nan(n_trials,2); %to generate 

trial = 1;
while trial <= n_trials
    offers_list(trial,:) = offer; %assign the lastoffer into the main list
    decision_var(trial) = offer(1) - offer(2); %difference in effort
    choice_placehold = 1; %again, a placeholder...
    
    [~,~,p_choose_ref(trial)] = lik_effort_only_dm(params,offer,choice_placehold,settings.modelname); %IMPORTANT: this only operates on ONE trial!!
    chose_ref(trial) = two_options_choose(p_choose_ref(trial)); 
    %use the adaptive algorithm!
    offer = staircase_trialgen_local(chose_ref(trial),offer,settings);
    
    trial = trial+1;
end

t_out = table(chose_ref,p_choose_ref,decision_var,offers_list);
%disp(t_out)

end
function new_offers = staircase_trialgen_local(choice,previous_trial_offers,settings)

% local function for the actual staircase algorithm

offerAdaptUnit = settings.offerAdaptUnit;
newStaircaseStarts = settings.newStaircaseStarts;
%newStaircaseStarts = settings.newStaircaseStarts; %possible new starts

%important_minimum = offerAdaptUnit;
important_minimum = 0.2; %% corresponding to lowest_effort_allowed in js


%the two offers (ref always left, alt always right
refOffer = previous_trial_offers(1);
altOffer = previous_trial_offers(2);
    %if first trial, just... enter the right numbers

%below: written by Bowen Xiao in JavaScript
    %translated to MATLAB by ChatGPT
    %checked again by Bowen Xiao
if choice == 1 % ref chosen, need to make alt more attractive
    altOffer = altOffer - offerAdaptUnit; % MATLAB better at maths than js
    % what to do when lower boundary is hit:
    if altOffer < important_minimum %bound for restart
        % randomly draw new staircase start
        altOffer = newStaircaseStarts(randi(numel(newStaircaseStarts)));
    end
elseif choice == 0 % ref not chosen, can make alt less attractive to approach threshold
    altOffer = altOffer + offerAdaptUnit;
    % what to do when upper boundary is hit:
    if altOffer > 0.99 %do NOT allow 100% MVC
        % randomly draw new staircase start
        altOffer = newStaircaseStarts(randi(numel(newStaircaseStarts)));
    end
end

new_offers = [refOffer, altOffer];

end