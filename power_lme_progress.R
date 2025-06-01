#following https://aosmith.rbind.io/2020/08/20/simulate-binomial-glmm/#a-single-simulation-for-a-binomial-glmm
#quick power calculations for Devine & Otto effect replications
# Bowen Xiao 2023
library(lmerTest)
library(purrr) 
library(ggplot2)
CurrentSourceWD<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(CurrentSourceWD)

sim_test_intercept<-function(b0 = 0,
                             b1 = 0.4,
                             ptp_var = 0.5^2,
                             n_ptp = 30,
                             n_levels_b1 = 2,
                             n_repeats = 4) {
  #simulate & fit for power calculations of the simple DM
  #test if intercept is non-zero
  #output p value of intercept
  
  #other settings
  n_combo = n_levels_b1#*n_levels_b2  #combination of conditions
  
  #create the variables
  n_rand_letter = 5
  ptp <- rep(stringi::stri_rand_strings(n_ptp, n_rand_letter), 
             #LETTERS[1:n_ptp], #create letters as ptp id (n_ptp < 26... 
             #or seq(n_combo))
             each = n_combo) #repeat the ptp id for each pseudoreplication
  condition.combo <- paste(ptp, 
                           rep(1:n_combo, #
                               times = n_levels_b1), #
                           sep = ".")
  condition <- rep( LETTERS[1:n_levels_b1],
                    times = n_ptp)
  dat <- data.frame(ptp, condition.combo, condition)
  
  #simulate random effect (different mean for each individual)
  ptp_eff = rep(rnorm(n = n_ptp, 
                      mean = 0, 
                      sd = sqrt(ptp_var)), 
                each = n_combo) #should be the same for each participant, just repeated
  
  #calculate the log-odds (the "true" y, without noisy data generation process)
  log_odds = with(dat, b0 + b1*(condition == "A") + ptp_eff)
  p_choice = plogis(log_odds) #convert to proportions / probability, between 0~1
  
  #trials per condition combo
  dat$num_samp = n_repeats
  
  #generate the data! (get the simulated proportion of choices?)
  dat$y = rbinom(n = n_ptp*n_combo, 
                 size = dat$num_samp, 
                 prob = p_choice)
  
  #fit the model
  dat$condition<-factor(dat$condition,levels = LETTERS[1:n_levels_b1])
  mod = glmer(cbind(y, num_samp - y) ~ 1 + condition + (1|ptp), 
              data = dat,
              family = binomial(link = "logit") )
  
  #extract coefficient
  si<-summary(mod)
  return(si$coefficients[1,4]) #get the p value for the intercept
}
sim_test_ratings<-function(n_ptp = 30, #devine 2a 2b only had 60 or 53
                           n_repeats = 4) {
  #simulate & fit for power calculations of the ratings effect
  #test for progress main effect
  #output p value of intercept
  
  #in Exp2B of Devine & Otto, having progress reduced 0.61 from scale 1~9
  #so......... 0.61/(9-1)*100 = 7.625 for our 0~100 scale? modestly go 7.5
  #assume one-to-one effort effect on rating 
  b1 = 1
  b2 = 7.5
  ptp_var = 20 #viarnace from tlx.m2 scaled for 0~100
  rating_var = 100 #a bit large of a variance from the true rating
  
  #task:
  levels_b1 = c(40, 60, 80) # effort levels
  n_levels_b1 = length(levels_b1)
  n_combo = n_levels_b1*2 #effort and progress conds
  
  #create the variables
  n_rand_letter = 5
  ptp <- rep(stringi::stri_rand_strings(n_ptp, n_rand_letter), 
             each = n_combo) #repeat the ptp id for each pseudoreplication
  
  progress <- rep(rep(c("Progress", "No Progress"), each = n_levels_b1), times = n_ptp)
  condition <- rep(rep(levels_b1, times = 2), 
                   times = n_ptp)
  dat <- data.frame(ptp, progress, condition)
  

  
  #simulate random effect (different mean for each individual)
  dat$ptp_eff = rep(rnorm(n = n_ptp, 
                      mean = 0, 
                      sd = sqrt(ptp_var)), 
                each = n_combo) #should be the same for each participant, just repeated
  #IMPORTANT: repeat dat by n_repeats to get the right number of trials
  dat = do.call(rbind, replicate(n_repeats, dat, simplify = FALSE))

  #generate the data! (ooopsss need to go 0 and 1 coding)
  progress_numeric = ifelse(dat$progress == "Progress", 1, 0)
  dat$y = NA
  for(indTrial in 1:nrow(dat)){
    dat$y[indTrial]=rnorm(n = 1, 
                          mean = dat$ptp_eff[indTrial] + b1*dat$condition[indTrial] + b2*progress_numeric[indTrial],
                          sd = sqrt(rating_var))
  }
  
  #fit the model
  mod = lmer(y ~ 1 + condition*progress + (1|ptp), 
              data = dat)
  
  #extract coefficient
  si<-summary(mod)
  return(si$coefficients["progressProgress","Pr(>|t|)"]) #get the p value for progress effect
}


alpha_level = 0.05

#=========FOR DECISION-MAKING=========
#run simulation with different sample sizes + number of repeats and get power
#the power for detecting P(choose progess) with the equal-effort trials

n_N <- c(30,60,80,100)
trial_repeats <- c(5,10,20)

#preallocate
MyPower<-data.frame()
#loop
set.seed(2023123)
for(i in n_N){
  for(j in trial_repeats){
    sims <- replicate(400, sim_test_intercept(b0 = 0.4,
                                              b1 = 0,
                                              ptp_var = 0.5^2,
                                              n_ptp = i,
                                              n_levels_b1 = 4,
                                              n_repeats = j), simplify = FALSE) #output the p value?
    power_est<-mean(sims<alpha_level)
    myAddRow<-c(i,j,power_est)
    MyPower<-rbind(MyPower,myAddRow)
  }
}
names(MyPower)<-c("SampleSize","TrialRepeats","PowerEst")
MyPower$TrialRepeats<-as.factor(MyPower$TrialRepeats)
power_fig1<-ggplot(MyPower,aes(SampleSize,PowerEst,color = TrialRepeats))+
  geom_point()+geom_line()+
  theme_classic()+geom_hline(yintercept=0.95)+
  labs(x="Sample Size",y="Power", color = "Number of repeated trials")+
  theme(legend.position = "bottom",text=element_text(size=18))
power_fig1
ggsave("figs/power_lme_dm.png",width=7,height=5)

#=========FOR RATINGS!=========
n_N <- c(30,60,80,100)
trial_repeats <- c(2,4,6,8)
MyPower<-data.frame()
#loop
set.seed(2023123)
for(i in n_N){
  print(i)
  for(j in trial_repeats){
    print(j)
    sims <- replicate(400, sim_test_ratings(n_ptp = i,
                                            n_repeats = j), simplify = FALSE) #output the p value?
    power_est<-mean(sims<alpha_level)
    myAddRow<-c(i,j,power_est)
    MyPower<-rbind(MyPower,myAddRow)
  }
  
}
names(MyPower)<-c("SampleSize","TrialRepeats","PowerEst")
MyPower$TrialRepeats<-as.factor(MyPower$TrialRepeats)
power_fig2<-ggplot(MyPower,aes(SampleSize,PowerEst,color = TrialRepeats))+
  geom_point()+geom_line()+
  theme_classic()+geom_hline(yintercept=0.95)+
  labs(x="Sample Size",y="Power", color = "Number of repeated trials")+
  theme(legend.position = "bottom",text=element_text(size=18))
power_fig2
ggsave("figs/power_lme_ratings.png",width=7,height=5)

# ===== better plot visual ====
# Modify the legend settings for both plots
power_fig1 <- power_fig1 + 
  theme(legend.position = "bottom",        
        legend.justification = "center",  # Default justification
        legend.direction = "horizontal",  
        legend.title.position = "top", 
        legend.background = element_blank()) # Remove background

power_fig2 <- power_fig2 + 
  theme(legend.position = "bottom",        
        legend.justification = "center",  # Default justification  
        legend.direction = "horizontal",   
        legend.title.position = "top", 
        legend.background = element_blank()) # Remove background

# Now combine and save
ggpubr::ggarrange(power_fig1, power_fig2, nrow = 1, labels = c("A", "B"))
ggsave("figs/power_combined.png", width = 7, height = 4)
# ==============================

# ADDITIONAL: power for just the t tests
pA1<-pwr::pwr.t.test(power = 0.95, 
                d = 0.45, 
                sig.level = 0.05, 
                type = "one.sample", 
                alternative = "greater")
pA1
round(pA1$n, digits = 0)

# ADDITIONAL: power for correlation after the unexpected exclusion
  # note: this seems to differ from G*Power but only by 0.006 something?
pA2<-pwr::pwr.r.test(n = 114,
                r = 0.3, 
                sig.level = 0.05, 
                alternative = "greater")
round(pA2$power,digits = 2) 

