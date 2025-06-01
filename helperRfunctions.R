# Wrapping up repeatedly used ggplot code:
TwoWayScatSum<-function(din,Xvar,Yvar,withinSubjFactor,Ylabel,
                        doge_set = 0.2, jitterW = 0.02,
                        paletteSet = "Dark2",
                        StatSumAlpha=1,RawDataAlpha = 0.8){
  #din is the data frame input; must be long format
  #Xvar and Yvar and withinSubjFactor and Ylabel should be strings
  #doge_set is the doge between two colors, not jitter
  #
  
  #ScatSum: scatter plot + summary statistics
  #caveat: basically incompatible with drawing lines...
  
  outplot<-ggplot(din,aes(x = .data[[Xvar]], y = .data[[Yvar]], color = .data[[withinSubjFactor]])) +
    stat_summary(fun.data = mean_cl_boot, 
                 position = position_dodge(doge_set), #finally found a line that dodges...
                 size = 0.8, alpha = StatSumAlpha) +
    geom_quasirandom(width = jitterW, #control the actual jitter (more when density higher)
                     stroke = 0, 
                     dodge.width=doge_set,
                     alpha = RawDataAlpha) +
    scale_y_continuous(Ylabel)+
    scale_color_brewer(palette = paletteSet)+
    theme_classic()
  
  return(outplot)
}
TwoWaySimpleSum<-function(din,Xvar,Yvar,withinSubjFactor,Ylabel,
                        doge_set = 0.2,
                        paletteSet = "Dark2"){
  outplot<-ggplot(din,aes(x = .data[[Xvar]], y = .data[[Yvar]], color = .data[[withinSubjFactor]])) +
    stat_summary(fun.data = mean_cl_boot, 
                 position = position_dodge(doge_set), #finally found a line that dodges...
                 size = 0.8) +
    scale_y_continuous(Ylabel)+
    scale_color_brewer(palette = paletteSet)+
    theme_classic()
  return(outplot)
}
OneWaySumLine<-function(din,Xvar,Yvar,withinSubjFactor,Ylabel,
                        doge_set = 0.2, jitterW = 0.02,
                        paletteSet = "Dark2" ){
  #din is the data frame input, after summarise()
  #Xvar and Yvar and withinSubjFactor and Ylabel should be strings
  #doge_set is the doge between two colors, not jitter
  #
  
  #summary stats are in the 
  
  outplot<-ggplot(din,aes(x = .data[[Xvar]], y = .data[[Yvar]], color = .data[[withinSubjFactor]])) +
    stat_summary(
      fun.data = mean_cl_boot, size = 1,
      position = position_dodge(doge_set) #finally found a line that dodges...
    ) +
    geom_quasirandom(width = jitterW, #control the actual jitter (more when density higher)
                     stroke = 0, 
                     dodge.width=doge_set,
                     alpha = 0.8) +
    scale_y_continuous(Ylabel)+
    scale_color_brewer(palette = paletteSet)+
    theme_classic()
  
  return(outplot)
}
MeanPtpTrajOneWay<-function(din,Xvar,Yvar,clust = "id",Ylabel = "mean",theTitle = " ",lineAlpha = 0.5,
                            do_legend = "none"){
  outplot<-din %>% 
    group_by(.data[[clust]],.data[[Xvar]],) %>% 
    summarise(mean_get = mean(.data[[Yvar]])) %>% 
    ggplot(aes(x = .data[[Xvar]], y = mean_get)) + 
    labs(title = theTitle, y = Ylabel)+
    geom_line(aes(color=.data[[clust]]),alpha = lineAlpha)+
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = do_legend)
  return(outplot)
}
check_ResidNorm<-function(theObj){
  #helper function to check LMM assumptions & visualise
  resid<-unname(residuals(theObj)) #unname.. named vector worst
  hist(resid)
  car::qqPlot(resid)
  ks.test(resid,'pnorm')
}
trial_lm_extract <- function(id, trialn, data) {
  # Define a function to fit linear regression and return a data frame with the core info
  d1 <- data[data$id == id & data$trialn == trialn, ]
  
  if (nrow(d1) > 0) {
    mtrial <- lm(speed ~ epoch_pump, d1)
    ds <- summary(mtrial)$coefficients
    tstat <- ds["epoch_pump", "t value"]
    
    effort_type<-unique(d1$effort_type)
  } else {
    tstat <- NA
    effort_type<-NA
  }
  
  dout<-data.frame(id, trial = trialn, tstat,effort_type)
  return(dout)
}
LMMlevel1TwoWayVis<-function(din,modelIn,Xform,Yform,Ylabel,ColorBy,clust = "id",VisSingle = F){
  #Xform: example "~epoch_pump"
  #modelIn: obj of the fitted model
  
  #IMPORTANT: if "VisSingle", din must be the dataframe corresponding to that one cluster
  
  #adapted from https://github.com/m-clark/mixed-models-with-R/blob/master/random_intercepts.Rmd
  
  outplot<-din %>%
    modelr::add_predictions(modelIn, var = 'pred_get') %>% #creates a new column on old mega
    group_by(.data[[clust]], #this is the cluster, e.g., participant, class, fishtank etc.
             .data[[ColorBy]]) %>%
    plot_ly(colors=c("red","blue")) %>% #this might only work if ColorBy has two levels...
    add_lines(
      x =  formula(Xform),
      y =  ~ pred_get,
      color = formula(paste0("~", ColorBy)),
      opacity = .4,
      size = I(1),
      name = ~paste('predictions for', .data[[ColorBy]]) #this is the legend for the lines?
    ) %>%
    layout(yaxis = list(title = Ylabel))
  if(VisSingle){
    outplot<-outplot %>% 
      add_markers(
        x =  formula(Xform),
        y =  formula(Yform),
        color = formula(paste0("~", ColorBy)),
        size = I(3),
        name = ~paste('raw data for ', .data[[ColorBy]])
      )
  }
  
  return(outplot)
}
CorrSmoothLmPlot<-function(din,Xvar,Yvar,Xlabel = Xvar,Ylabel=Yvar,
                           addRefLine=F,addHline=F,addVline=F,addCardinals=F,manual_fontsize=16,fit_lm=T,add_corr_label=T){
  #theme_set(theme_classic()+theme(text = element_text(size = 16))) #why isn't this working?
  
  #scatter plot between two variables and geom_smooth
  getCor<-cor.test(din[,c(Xvar)],din[,c(Yvar)])
  corrlabel <- sprintf("Pearson's r = %.2f, p = %.3f",getCor$estimate[[1]],getCor$p.value)

  #actual plot
  outplot<-ggplot(din, aes(x = .data[[Xvar]], y = .data[[Yvar]])) +
                    geom_point(alpha = 0.8)
  if(fit_lm){
    outplot<-outplot+geom_smooth(method=lm,
                #formula = formula(paste0(Yvar,"~",Xvar))
    )
  }
  
  if(addCardinals){
    addHline=T
    addVline=T
  }
  if(addRefLine){
    outplot<-outplot+geom_abline(intercept = 0, slope = 1,colour = "red",linetype="dashed")
  }
  if(addHline){
    outplot<-outplot+geom_hline(yintercept = 0, slope = 1,colour = "red",linetype="dashed")
  }
  if(addVline){
    outplot<-outplot+geom_vline(xintercept = 0, slope = 1,colour = "red",linetype="dashed")
  }
  if(add_corr_label){
    outplot<-outplot+labs(title=corrlabel)
  }
  outplot<-outplot+labs(y= Ylabel, x = Xlabel)+    
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5),text = element_text(size=manual_fontsize))

  return(outplot)
}

# twice-used functions for manuscript write-up
write_up_mega_wrangle<-function(mega, set_tC){
  # wrange the mega table
  # set_tC differs for Exp1 and Exp2
  
  mega<-mega %>% mutate(effort_type = recode(as.numeric(progress_shown),
                                             `1` = "with-progress", `0`="no-progress"))
  #alright mutate/recode cannot into `TRUE` for logical...
  mega$effort_type<-factor(mega$effort_type,
                           levels = c("no-progress","with-progress")) #so no-progress will look red, more intuitive
  #label the trials across each chain
  mega$trial_in_chain<-NA
  mega$chain_ind <- mega$record_currentChain+1
  for(t in 1:nrow(mega)){
    if(mega$trialn[t]==1){
      tC = set_tC #c(0,0,0) for Exp2, c(0,0,0,0) for Exp1
    }
    tC[mega$chain_ind[t]]=tC[mega$chain_ind[t]]+1
    mega$trial_in_chain[t] = tC[mega$chain_ind[t]]
  }
  return(mega)
}
choice_func_plot<-function(mega, set_title){
  # to visualise group-level mean choice & ptp-level curves
  # summary first, then plot
  choice_dEff<-mega %>% 
    group_by(prolific_id,dOffer) %>% 
    summarize(pChooseRef = mean(ChoseRef==1))
  effSum<-mega %>% 
    group_by(dOffer) %>% 
    summarize(pChooseRef = mean(ChoseRef==1),
              n_data = n())
  levels2keep1<-names(table(effSum$dOffer[effSum$n_data>20]))
  levels2keep<-as.numeric(levels2keep1)
  figOut<-ggplot(choice_dEff[choice_dEff$dOffer %in% levels2keep,], 
                aes(x = dOffer, y = pChooseRef,color = prolific_id)) +
    geom_line(aes(group = prolific_id),
              #color = "grey",
              alpha=0.12) +
    stat_summary(fun.data = mean_cl_boot, 
                 shape = 21, fill = "white", #this is to make the stat summary hollow
                 size = 0.8,
                 color= "black")+
    labs(title = set_title, 
         x = "△effort", 
         y = "p(Choose PF+)")+
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none")+theme(text = element_text(size = 16))
  return(list(figOut = figOut,choice_dEff=choice_dEff))
}
SVPFhist<-function(dptp){
  # Bootstrapping to get 95% CI for eqpoint -- no exclu
  boot_result <- boot(dptp$eqpoint, mean_func, R = 5000)
  boot_ci <- boot.ci(boot_result, type = "basic")
  mean_eqpoint_1 <- mean(dptp$eqpoint)
  ci_lower_1 <- boot_ci$basic[4]  # Lower bound of 95% CI
  ci_upper_1 <- boot_ci$basic[5]  # Upper bound of 95% CI
  
  #the PSE at group level is small
  print(mean(dptp$eqpoint))
  print(c(ci_lower_1,ci_upper_1))
  print(mean(dptp$eqpoint>=0.05))
  
  # Create a temporary histogram to get the bin counts
  hist_data <- ggplot_build(ggplot(dptp, aes(x = eqpoint)) +
                              geom_histogram(color = "blue", fill = "white"))$data[[1]]
  # Calculate the y position as 105% of the maximum bin count
  y_position_1 <- max(hist_data$count) * 1.05
  
  dptp %>% ggplot(aes(x=eqpoint))+
    geom_histogram(color="blue",fill="white")+
    geom_vline(xintercept=0,linetype = 3,color="red",size=1.2)+
    geom_point(aes(x = mean_eqpoint_1, y = y_position_1), 
               shape = 21, color = "black", 
               size = 1.2, stroke = 1) +  # Mean dot
    geom_errorbarh(aes(y = y_position_1, xmin = ci_lower_1, xmax = ci_upper_1), 
                   color = "black", height = 0.5, size = 1.2) +  # 95% CI as horizontal error bar
    labs(x = expression(SV["PF+"]), 
         y = "Number of participants")+
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none")+theme(text = element_text(size = 16))
}
