
%% range to sweep through:
nptp = 1000;
nparams = 2;
modelname = 'additive_nolps';

rng(2024)

%% staircase task settings
settings.ini_refs = [0.7,0.55,0.4];
ini_alts = [0.6,0.5,0.4];
settings.firstOffers = [settings.ini_refs;ini_alts]';
settings.offerAdaptUnit = 0.05; 
settings.newStaircaseStarts = [0.2,0.3,0.4];
settings.trials_per_staircase = 15;
settings.chain_index = NaN;

%% loop for recovery
%prepare to log them
Eq = unifrnd(-0.5,0.5,[nptp,1]);
beta = -(exprnd(30,[nptp,1]));%centered on median of the actual data
lapse = 0;

recovered = nan(nptp,nparams);
for i = 1:nptp
    if mod(i,5) == 0
        fprintf('run %i for recovery completed \n',i)
    end
    
    %% the simulation part (to get staircase task + choices)
    out_t = staircase_ThreeChains_ptpSim_omni([Eq(i),beta(i),lapse],false,modelname,settings);%settings for staircase is inside this function
    choices_sim = out_t.chose_ref;
    offers = out_t.offers_list;
    
    %% the fitting part (fixed code here!!)
    recovered(i,:) = fit_lik_effort_only_dm(offers,choices_sim,modelname);
end

%% how well recovered; 

%to add bounds:
sel = beta < 0 & recovered(:,2) < 0;

r_Eq = [corr(Eq(sel),recovered(sel,1),'type','Pearson'),corr(Eq(sel),recovered(sel,1),'type','Kendall')]; 
r_beta = [corr(log(-beta(sel)),log(-recovered(sel,2)),'type','Pearson'),corr(log(-beta(sel)),log(-recovered(sel,2)),'type','Kendall')];


figure;
subplot(1,2,1)
plot(Eq(sel),recovered(sel,1),'o')
hold on
uline = refline([1 0]); %slope of 1 and intercept of 0
    uline.Color = 'k';
    uline.LineStyle = "--";
hold off
xlabel('Simulated SV_{PF+}')
ylabel('Recovered SV_{PF+}')

subplot(1,2,2)
plot(log(-beta(sel)),log(-recovered(sel,2)),'o')
hold on
uline = refline([1 0]); %slope of 1 and intercept of 0
    uline.Color = 'k';
    uline.LineStyle = "--";
hold off
xlabel('Simulated log(-beta)')
ylabel('Recovered log(-beta)')

hax = findobj(gcf,'type','axes'); 
set(hax,'FontSize',12); % set the properties of _both_ axes

set(gcf,'Position', [205 598 477 200])
saveas(gcf,'figs/parameter_recovery_general.eps','epsc')
