library(tidyverse)
library(data.table)
library(stringr)
library(ggplot2)
library(viridis)
library(magrittr)
library(ggpubr)
library(ggthemes)
library(cowplot)
library(lme4)
library(car)

# set data paths
path_exp <- "C:/Users/cstone/OneDrive - UNSW/Documents/Projects/my_experiments/val_decode/"
path_out <- paste0(path_exp, 'output/')
path_data <- paste0(path_exp, 'data/')

# load data
files_beh <- list.files(path_data, pattern = '.*(beh.txt)', recursive = T)
# data_beh <- fread(file.path(path_data, files_beh[1]), stringsAsFactors = T)
data_beh <- rbindlist(lapply(file.path(path_data, files_beh), fread))
files_dfun <- list.files(path_out, pattern = 'dfun_cue_rc_avg') ################ Cue-locked, reward-contingency blocks - change here for different epochs
# data_dfun <- fread(file.path(path_out, files_dfun[1]), stringsAsFactors = T)
data_dfun <- rbindlist(lapply(file.path(path_out, files_dfun), fread))
# files_dfun_by_val <- list.files(path_out, pattern = 'dfun_by_val_cue_rc_avg')
# data_dfun_by_val <- rbindlist(lapply(file.path(path_out, files_dfun_by_val), fread))
## Run analysis ----------------------------------------------------------------
n_tpoints = 282 # epochs are from -0.1 s to 1 s relative to cue onset
ts <- seq(-0.1, 1, length.out=n_tpoints) # put time samples into actual times

# define model RDMs
# Trig codes: 1, 2, 3, 4, 5, 6, 7, 8  
vec_stim <- c(1, 0, 1, 0, 1, 0, 1, 0, # 110 - high value, rule toward, target left
              0, 1, 0, 1, 0, 1, 0, 1, # 210 - high, toward, right
              1, 0, 1, 0, 1, 0, 1, 0, # 310 - high, away, left
              0, 1, 0, 1, 0, 1, 0, 1, # 410 - high, away, right
              1, 0, 1, 0, 1, 0, 1, 0, # 510 - low, toward, left
              0, 1, 0, 1, 0, 1, 0, 1, # 610 - low, toward, right
              1, 0, 1, 0, 1, 0, 1, 0, # 710 - low, away, left
              0, 1, 0, 1, 0, 1, 0, 1) # 810 - low, away, right
mat_stim <- matrix(vec_stim, 8)
vec_stim_by_val <- c(1, 0, 1, 0, 0, 1, 0, 1,
                     0, 1, 0, 1, 1, 0, 1, 0,
                     1, 0, 1, 0, 0, 1, 0, 1,
                     0, 1, 0, 1, 1, 0, 1, 0,
                     0, 1, 0, 1, 1, 0, 1, 0,
                     1, 0, 1, 0, 0, 1, 0, 1,
                     0, 1, 0, 1, 1, 0, 1, 0,
                     1, 0, 1, 0, 0, 1, 0, 1)
mat_stim_by_val <- matrix(vec_stim_by_val, 8)
vec_resp <- c(1, 0, 0, 1, 1, 0, 0, 1,
              0, 1, 1, 0, 0, 1, 1, 0,
              0, 1, 1, 0, 0, 1, 1, 0,
              1, 0, 0, 1, 1, 0, 0, 1,
              1, 0, 0, 1, 1, 0, 0, 1,
              0, 1, 1, 0, 0, 1, 1, 0,
              0, 1, 1, 0, 0, 1, 1, 0,
              1, 0, 0, 1, 1, 0, 0, 1)
mat_resp <- matrix(vec_resp, 8, 8)
vec_resp_by_val <- c(1, 0, 1, 0, 0, 1, 1, 0,
                     0, 1, 0, 1, 1, 0, 0, 1,
                     1, 0, 1, 0, 0, 1, 1, 0,
                     0, 1, 0, 1, 1, 0, 0, 1,
                     0, 1, 0, 1, 1, 0, 0, 1,
                     1, 0, 1, 0, 0, 1, 1, 0,
                     1, 0, 1, 0, 0, 1, 1, 0,
                     0, 1, 0, 1, 1, 0, 0, 1)
mat_resp_by_val <- matrix(vec_resp_by_val, 8, 8, byrow=T) 
vec_resp_by_val2 <- c(1, 0, 0, 1, 0, 1, 0, 1,
                      0, 1, 1, 0, 1, 0, 1, 0,
                      0, 1, 1, 0, 1, 0, 1, 0,
                      1, 0, 0, 1, 0, 1, 0, 1,
                      0, 1, 1, 0, 1, 0, 1, 0,
                      1, 0, 0, 1, 0, 1, 0, 1,
                      0, 1, 1, 0, 1, 0, 1, 0,
                      1, 0, 0, 1, 0, 1, 0, 1)
mat_resp_by_val2 <- matrix(vec_resp_by_val2, 8, 8, byrow=T)
vec_resp_by_val3 <- c(1, 0, 1, 0, 1, 0, 0, 1,
                      0, 1, 0, 1, 0, 1, 1, 0,
                      1, 0, 1, 0, 1, 0, 0, 1,
                      0, 1, 0, 1, 0, 1, 1, 0,
                      1, 0, 1, 0, 1, 0, 0, 1,
                      0, 1, 0, 1, 0, 1, 1, 0,
                      0, 1, 0, 1, 0, 1, 1, 0,
                      1, 0, 1, 0, 1, 0, 0, 1)
mat_resp_by_val3 <- matrix(vec_resp_by_val3, 8, 8, byrow=T)
vec_rule <- c(1, 1, 0, 0, 1, 1, 0, 0,
              1, 1, 0, 0, 1, 1, 0, 0,
              0, 0, 1, 1, 0, 0, 1, 1,
              0, 0, 1, 1, 0, 0, 1, 1,
              1, 1, 0, 0, 1, 1, 0, 0,
              1, 1, 0, 0, 1, 1, 0, 0,
              0, 0, 1, 1, 0, 0, 1, 1,
              0, 0, 1, 1, 0, 0, 1, 1)
mat_rule <- matrix(vec_rule, 8, 8)
vec_rule_by_val <- c(1, 1, 1, 1, 0, 0, 1, 1,
                     1, 1, 1, 1, 0, 0, 1, 1,
                     1, 1, 1, 1, 0, 0, 1, 1,
                     1, 1, 1, 1, 0, 0, 1, 1,
                     0, 0, 0, 0, 1, 1, 0, 0,
                     0, 0, 0, 0, 1, 1, 0, 0,
                     1, 1, 1, 1, 0, 0, 1, 1,
                     1, 1, 1, 1, 0, 0, 1, 1)
mat_rule_by_val <- matrix(vec_rule_by_val, 8, 8, byrow=T) 
vec_val <- c(1, 1, 1, 1, 0, 0, 0, 0,
             1, 1, 1, 1, 0, 0, 0, 0,
             1, 1, 1, 1, 0, 0, 0, 0,
             1, 1, 1, 1, 0, 0, 0, 0,
             0, 0, 0, 0, 1, 1, 1, 1,
             0, 0, 0, 0, 1, 1, 1, 1,
             0, 0, 0, 0, 1, 1, 1, 1,
             0, 0, 0, 0, 1, 1, 1, 1)
mat_val <-matrix(vec_val, 8)
# mat_conj <- diag(1, 8, 8)

# Run analysis per subject and save results
rslts_all <- data.table()
rslts_by_val <- data.table()
rslts_int <- data.table()
rslts_beh <- data.table()
for (sub in unique(data_dfun[, subID])) {
  
  # update progress 
  print(paste0('Analysing ', sub))
  
  ## Wrangling data ##
  # get condition RTs
  data_beh_sub <- data_beh[Subject == sub & Accuracy == 1 & Block < 13] ######## Reward contingency blocks
  data_beh_sub_av <- data_beh_sub[, 
                                  .(MeanRT=mean(RT)),
                                  by = .(DistractorValue, ResponseRule)
                                  ][order(ResponseRule, DistractorValue)]
  rslts_beh_sub <- data.table(
    subID = sub,
    vmac_away = data_beh_sub_av[1, MeanRT] - data_beh_sub_av[2, MeanRT], # high minus low
    vmac_toward = data_beh_sub_av[3, MeanRT] - data_beh_sub_av[4, MeanRT] # high minus low
  )
  rslts_beh <- rbind(rslts_beh, rslts_beh_sub)
  
  # put data_dfun into long format
  data_dfun_sub <- data_dfun[subID == sub,]
  data_dfun_long <- transpose(data_dfun_sub[, dfun_110:dfun_810, ])
  data_dfun_long[, class := as.integer(paste0(seq(1, 8), '10'))]
  data_dfun_long <- melt(data_dfun_long, id.vars='class', 
                         variable.name='trial_by_tpoint', 
                         value.name='dfun')
  data_dfun_ids = data.table( # create data table of ID variables
    y = rep(data_dfun_sub[, y], each=8),
    tpoint = rep(data_dfun_sub[, tpoint], each=8),
    subID = rep(data_dfun_sub[, subID], each=8)) 
  data_dfun_long <- cbind(data_dfun_ids, data_dfun_long) # add ID variables back to data
  setorder(data_dfun_long, y, tpoint, trial_by_tpoint, class) # make sure things are ordered correctly
  
  # add in RDMs vectors for each trial type
  data_dfun_long[, ':=' 
      (stim = case_when(
        unique(y) %in% c(110, 310, 510, 710) ~ mat_stim[, 1], 
        T ~ mat_stim[, 2]),
       resp = case_when(
        unique(y) %in% c(110, 410, 510, 810) ~ mat_resp[, 1],
        T ~ mat_resp[, 7]),
       rule = case_when(
        unique(y) %in% c(110, 210, 510, 610) ~ mat_rule[, 1],
        T ~ mat_rule[, 3]),
       val = case_when(
        unique(y) %in% c(110, 210, 310, 410) ~ mat_val[, 1],
        T ~ mat_val[, 5]),
       conj = case_when(
        y == class ~ 1,
        T ~ 0),
       stim_by_val = case_when(
        unique(y) %in% c(110, 310, 610, 810) ~ mat_stim_by_val[, 1],
        T ~ mat_stim_by_val[, 2]),
       resp_by_val = case_when(
        unique(y) %in% c(110, 310, 610, 710) ~ mat_resp_by_val[, 1],
        T ~ mat_resp_by_val[, 2]), 
       resp_by_val2 = case_when(
        unique(y) %in% c(110, 410, 610, 810) ~ mat_resp_by_val2[, 1],
        T ~ mat_resp_by_val2[, 2]),
       resp_by_val3 = case_when(
          unique(y) %in% c(110, 310, 510, 810) ~ mat_resp_by_val3[, 1],
          T ~ mat_resp_by_val3[, 2]),
       rule_by_val = case_when(
        unique(y) %in% c(510, 610) ~ mat_rule_by_val[, 5],
        T ~ mat_rule_by_val[, 1])), 
      by = trial_by_tpoint]
  
  # add in RT nuisance vector
  # data_dfun_long <- data_dfun_long[data_beh_sub_av[, .(ZMeanRT, y)], on='y']
  
  ## Running analysis ## 
  
  # 1) Loop through time points and run analysis for all data conditions
  for (t in seq(0, n_tpoints - 1)) { # subtract 1 to account for Python indexing starting at 0
    
    # subset data per time point
    data_dfun_t <- data_dfun_long[tpoint == t, ]
    
    # compute model
    # mdl <- lm(dfun ~ stim + resp + rule + val, data = t_dat)
    mdl_t <- lsfit(x=data_dfun_t[, .(stim, resp, rule, val, conj)], 
                   y=data_dfun_t[, dfun])
    
    # add betas to data table
    rslts_t <- data.table(
      subID = sub,
      time = ts[t+1],
      int = mdl_t$coefficients['Intercept'],
      stim = mdl_t$coefficients['stim'],
      resp = mdl_t$coefficients['resp'],
      rule = mdl_t$coefficients['rule'],
      val = mdl_t$coefficients['val'],
      conj = mdl_t$coefficients['conj'])
    
    # append data from current time point to overall data table
    rslts_all <- rbind(rslts_all, rslts_t)
  }
  
  # 2) Run analysis separately for high and low value trials
  # for (value in c('hi', 'lo')) {
  #   
  #   # subset trials by value
  #   data_dfun_val <- data_dfun_long[value==value]
  #   
  #   # loop through times
  #   for (t in seq(0, n_tpoints - 1)) { 
  #     
  #     # subset data per time point
  #     data_dfun_t <- data_dfun_val[tpoint == t, ]
  #     
  #     # compute model
  #     mdl_t <- lsfit(x=data_dfun_t[, .(stim, resp, rule, conj, ZMeanRT)], 
  #                    y=data_dfun_t[, dfun])
  #     
  #     # add betas to data table
  #     rslts_t <- data.table(
  #       subID = sub,
  #       value = value,
  #       time = ts[t+1],
  #       int = mdl_t$coefficients['Intercept'],
  #       stim = mdl_t$coefficients['stim'],
  #       resp = mdl_t$coefficients['resp'],
  #       rule = mdl_t$coefficients['rule'],
  #       val = mdl_t$coefficients['val'],
  #       conj = mdl_t$coefficients['conj'],
  #       rt = mdl_t$coefficients['ZMeanRT'])
  #     
  #     # append data from current time point to overall data table
  #     rslts_by_val <- rbind(rslts_by_val, rslts_t)
  #   }
  # }
  
  # 3) Run analysis for alternate rule and response RDMs
  for (t in seq(0, n_tpoints - 1)) { # subtract 1 to account for Python indexing starting at 0
    
    # subset data per time point
    data_dfun_t <- data_dfun_long[tpoint == t, ]
    
    # compute models
    mdl_t_stim <- lsfit(x=data_dfun_t[, .(stim, stim_by_val)], 
                        y=data_dfun_t[, dfun])
    mdl_t_resp <- lsfit(x=data_dfun_t[, .(resp, resp_by_val)], 
                        y=data_dfun_t[, dfun])
    mdl_t_resp2 <- lsfit(x=data_dfun_t[, .(resp, resp_by_val2)], 
                        y=data_dfun_t[, dfun])
    mdl_t_resp3 <- lsfit(x=data_dfun_t[, .(resp, resp_by_val3)], 
                         y=data_dfun_t[, dfun])
    mdl_t_rule <- lsfit(x=data_dfun_t[, .(rule, rule_by_val)], 
                        y=data_dfun_t[, dfun])
    
    # add betas to data table
    rslts_t <- data.table(
      subID = sub,
      tpoint = t,
      time = ts[t+1],
      stim = mdl_t_stim$coefficients['stim'],
      stim_by_val = mdl_t_stim$coefficients['stim_by_val'],
      resp = mdl_t_resp$coefficients['resp'],
      resp_by_val = mdl_t_resp$coefficients['resp_by_val'],
      resp2 = mdl_t_resp2$coefficients['resp'],
      resp_by_val2 = mdl_t_resp2$coefficients['resp_by_val2'],
      resp3 = mdl_t_resp3$coefficients['resp'],
      resp_by_val3 = mdl_t_resp3$coefficients['resp_by_val3'],
      rule = mdl_t_rule$coefficients['rule'],
      rule_by_val = mdl_t_rule$coefficients['rule_by_val'])
    
    # append data from current time point to overall data table
    rslts_int <- rbind(rslts_int, rslts_t)
  }
}

## Relationship to behavioural VMAC effect
rslts_int <- rslts_int[rslts_beh, on='subID']
rslts_beh_by_rdm <- data.table()
for (t in seq(0, n_tpoints - 1)) { # 
  
  # subset data
  rslts_int_t <- rslts_int[tpoint==t,]
  # run model
  mdl_beh_t <- lm(cbind(vmac_away, vmac_toward) ~ resp + resp_by_val, data=rslts_int_t)
  # save output
  coefs_away = coef(summary(mdl_beh_t)[1])[[1]] # index 1 to get 'away'
  coefs_toward = coef(summary(mdl_beh_t)[2])[[1]] # index 2 to get 'toward
  rslts_beh_by_rdm_t <- data.table(
    tpoint=t,
    time = ts[t+1],
    Rule = rep(c('away', 'toward'), each=3), 
    Effect = rep(c('int', 'resp', 'resp_by_val'), times=2),
    beta = c(coefs_away[1:3, 'Estimate'], coefs_toward[1:3, 'Estimate']),
    std_err = c(coefs_away[1:3, 'Std. Error'], coefs_toward[1:3, 'Std. Error']), 
    t_val = c(coefs_away[1:3, 't value'], coefs_toward[1:3, 't value']),
    p_val = c(coefs_away[1:3, 'Pr(>|t|)'], coefs_toward[1:3, 'Pr(>|t|)'])
  )
  rslts_beh_by_rdm <- rbind(rslts_beh_by_rdm, rslts_beh_by_rdm_t)
}

## Plot RDM results ------------------------------------------------------------
# theme for plotting 
my_theme <- function() {
  theme(
    # all lines
    line = element_line(linewidth=1),
    # axes
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14.5),
    axis.ticks=element_line(linewidth=1),
    axis.ticks.length=unit(.125, "cm"),
    # font
    text = element_text(family='sans', 
                        face='plain',
                        color='black',
                        size=14),
    # legend
    legend.key=element_rect(fill="transparent"),
    legend.position='none',
    legend.text=element_text(size=12.5),
    legend.title=element_text(size=14),
    # panel
    panel.background=element_rect(fill='white', 
                                  colour='white')
  )
}
# calculate standard error
se_function <- function(x) {
  se <- sd(x)/sqrt(length(x))
}

## plot all
rslts_all_p <- rslts_all[, lapply(.SD, mean), by=time, 
                 .SDcols=c('stim', 'resp', 'rule', 'val', 'conj')]
rslts_all_se_p <- rslts_all[, lapply(.SD, se_function), by=time, 
                  .SDcols=c('stim', 'resp', 'rule', 'val', 'conj')]
rslts_all_p <- rslts_all_p[rslts_all_se_p,
                           on='time', 
                           j = .(time = time,
                                 stim = stim,
                                 rule = rule,
                                 resp = resp,
                                 val = val,
                                 conj = conj,
                                 stim_se = i.stim,
                                 resp_se = i.resp,
                                 rule_se = i.rule,
                                 val_se = i.val,
                                 conj_se = i.conj)]

plot_all <- ggplot(rslts_all_p, aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=stim, color='stimulus'), linewidth=1) +
  # geom_ribbon(aes(y=stim, ymin=stim-stim_se, ymax=stim+stim_se, fill='stimulus'), alpha=.25) +
  geom_line(aes(y=resp, color='response'), linewidth=1) +
  # geom_ribbon(aes(y=resp, ymin=resp-resp_se, ymax=resp+resp_se, fill='response'), alpha=.25) +
  geom_line(aes(y=rule, color='rule'), linewidth=1) +
  # geom_ribbon(aes(y=rule, ymin=rule-rule_se, ymax=rule+rule_se, fill='rule'), alpha=.25) +
  geom_line(aes(y=val, color='value'), linewidth=1) + 
  # geom_ribbon(aes(y=val, ymin=val-val_se, ymax=val+val_se, fill='value'), alpha=.25) +
  geom_line(aes(y=conj, color='conjunction'), linewidth=1) +
  # geom_ribbon(aes(y=conj, ymin=conj-conj_se, ymax=conj+conj_se, fill='conjunction'), alpha=.25) +
  
  # customise
  scale_y_continuous(name='Beta', 
                     breaks=c(0, 0.25, 0.5, 0.75),
                     labels=c('0.0', '0.25', '0.50', '0.75'),
                     # limits=c(0.375, 1),
                     expand=expansion(mult = 0.1)) + 
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  scale_color_manual(name='',
                     breaks=c('stimulus', 'response', 'rule', 'value', 'conjunction'),
                     values=c('stimulus' = '#fdb42f',
                              'response' = '#ed7953',
                              'rule' = '#9c179e',
                              'value' = '#5c01a6',
                              'conjunction'  = '#2c115f'),
                     labels=c('stimulus', 'response', 'rule', 'value', 'conjunction')) +
  # scale_fill_manual(name='',
  #                   breaks=c('stimulus', 'response', 'rule', 'value', 'conjunction'),
  #                   values=c('stimulus' = '#fdb42f',
  #                            'response' = '#ed7953',
  #                            'rule' = '#9c179e',
  #                            'value' = '#5c01a6',
  #                            'conjunction'  = '#2c115f'),
  #                   labels=c('stimulus', 'response', 'rule', 'value', 'conjunction')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(0, 0.75)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')


png(paste0(path_out, 'plt_all.png'),
    width=8, height=4, units='in', res=400)
plot(plot_all)
dev.off()

## plot high vs. low
# average across subjects
rslts_by_val_p <- rslts_by_val[, lapply(.SD, mean), by=c('time', 'value'), 
                         .SDcols=c('stim', 'resp', 'rule', 'val', 'conj', 'rt')]
# find SE
rslts_by_val_se_p <- rslts_by_val[, lapply(.SD, se_function), by=c('time', 'value'), 
                            .SDcols=c('stim', 'resp', 'rule', 'val', 'conj', 'rt')]
# join SE results to mean results
rslts_by_val_p <- rslts_by_val_p[rslts_by_val_se_p,
                                 on=c('time', 'value'),
                                 j = .(time = time,
                                       value = value,
                                       stim = stim,
                                       rule = rule,
                                       resp = resp,
                                       val = val,
                                       conj = conj,
                                       rt = rt,
                                       stim_se = i.stim,
                                       resp_se = i.resp,
                                       rule_se = i.rule,
                                       val_se = i.val,
                                       conj_se = i.conj,
                                       rt_se = i.rt)]
# put data.table into long format for plotting
rslts_by_val_p <- rslts_by_val_p[, 
                      melt(.SD, 
                      id.vars=c('time', 'value'), 
                      measure.vars=list(c('stim', 'rule', 'resp', 
                                          'val', 'conj', 'rt'),
                                        c('stim_se', 'rule_se', 'resp_se', 
                                          'val_se', 'conj_se', 'rt_se')),
                      variable.name='component',
                      value.name=c('beta', 'beta_se'))
                      ][, component := case_when(
                           component == 1 ~ 'stim',
                           component == 2 ~ 'rule',
                           component == 3 ~ 'resp',
                           component == 4 ~ 'val',
                           component == 5 ~ 'conj',
                           T ~ 'rt')
                        ]

plot_by_val <- ggplot(rslts_by_val_p, 
                       aes(x=time, y=beta, color=value),
                       linewidth=1) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)), 
            aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-0.1, 0.2)), 
            aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line() +
  facet_grid('component') + 
  # customise
  scale_y_continuous(name='Beta', 
                     breaks=c(0, 0.25, 0.5, 0.75),
                     labels=c('0.0', '0.25', '0.50', '0.75'),
                     # limits=c(0.375, 1),
                     expand=expansion(mult = 0.1)) + 
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  scale_color_manual(name='value',
                     breaks=c('high', 'low'),
                     values=c('high' = '#ed7953',
                              'low' = '#9c179e'),
                     labels=c('high', 'low')) +
  # scale_fill_manual(name='',
  #                   breaks=c('stimulus', 'response', 'rule', 'value', 'conjunction'),
  #                   values=c('stimulus' = '#fdb42f',
  #                            'response' = '#ed7953',
  #                            'rule' = '#9c179e',
  #                            'value' = '#5c01a6',
  #                            'conjunction'  = '#2c115f'),
  #                   labels=c('stimulus', 'response', 'rule', 'value', 'conjunction')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(0, 0.75)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')



## plot int
rslts_int_p <- rslts_int[, lapply(.SD, mean), by=time, 
                         .SDcols=c('resp', 'resp_by_val', 'resp_by_val', 
                                   'resp_by_val', 'rule', 'rule_by_val')]
rslts_int_se_p <- rslts_int[, lapply(.SD, se_function), by=time, 
                            .SDcols=c('resp', 'resp_by_val', 'resp_by_val2', 
                                      'resp_by_val3', 'rule', 'rule_by_val')]
rslts_int_p <- rslts_int_p[rslts_int_se_p,
                           on='time', 
                           j = .(time = time,
                                 rule = rule,
                                 rule_by_val = rule_by_val,
                                 resp = resp,
                                 resp_by_val = resp_by_val,
                                 resp_by_val2 = resp_by_val2,
                                 resp_by_val3 = resp_by_val3,
                                 rule_se = i.rule,
                                 rule_by_val_se = i.rule_by_val,
                                 resp_se = i.resp,
                                 resp_by_val_se = i.resp_by_val,
                                 resp_by_val2_se = i.resp_by_val2,
                                 resp_by_val3_se = i.resp_by_val3)]


plot_int <- ggplot(rslts_int_p, aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=resp, color='response'), linewidth=1) +
  # geom_ribbon(aes(y=resp, ymin=resp-resp_se, ymax=resp+resp_se, fill='stimulus'), alpha=.25) +
  geom_line(aes(y=resp_by_val, color='response_by_val'), linewidth=1, linetype=2) +
  # geom_ribbon(aes(y=resp_alt, ymin=resp_alt-resp_alt_se, ymax=resp_alt+resp_alt_se, fill='response_alt'), alpha=.25) +
  geom_line(aes(y=resp_by_val2, color='response_by_val2'), linewidth=1, linetype=2) +
  geom_line(aes(y=resp_by_val3, color='response_by_val3'), linewidth=1, linetype=2) +
  
  # geom_line(aes(y=rule, color='rule'), linewidth=1) +
  # geom_ribbon(aes(y=rule, ymin=rule-rule_se, ymax=rule+rule_se, fill='rule'), alpha=.25) +
  # geom_line(aes(y=rule_alt, color='rule_alt'), linewidth=1, linetype=2) + 
  # geom_ribbon(aes(y=rule_alt, ymin=rule_alt-rule_alt_se, ymax=rule_alt+rule_alt_se, fill='rule_alt'), alpha=.25) +
  
  # customise
  scale_y_continuous(name='Beta', 
                     breaks=c(0, 0.25, 0.5, 0.75),
                     labels=c('0.0', '0.25', '0.50', '0.75'),
                     # limits=c(0.375, 1),
                     expand=expansion(mult = 0.1)) + 
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  # scale_color_manual(name='',
  #                    breaks=c('response', 'response_alt', 'rule', 'rule_alt'),
  #                    values=c('response' = '#ed7953',
  #                             'response_alt' = '#ed7953',
  #                             'rule' = '#5c01a6',
  #                             'rule_alt' = '#5c01a6'),
  #                    labels=c('response', 'response_alt', 'rule', 'rule_alt')) +
  # scale_fill_manual(name='',
  #                   breaks=c('response', 'response_alt', 'rule', 'rule_alt'),
  #                   values=c('response' = '#ed7953',
  #                            'response_alt' = '#ed7953',
  #                            'rule' = '#5c01a6',
  #                            'rule_alt' = '#5c01a6'),
  #                   labels=c('response', 'response_alt', 'rule', 'rule_alt')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(0, 0.75)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')


png(paste0(path_out, 'plt_int.png'),
    width=8, height=4, units='in', res=400)
plot(plot_int)
dev.off()


## plot behavioural VMAC effect
rslts_beh_melt <- melt(rslts_beh, 
                       id.vars='subID',
                       variable.name='rule',
                       value.name='vmac_effect')[order(subID, rule)]

ggplot(data=rslts_beh_melt,
       aes(rule, vmac_effect)) + 
  
  # add zero line
  geom_line(data=data.frame(x=c('vmac_away', 'vmac_toward'), y=c(0, 0)), 
            aes(x, y, group=1), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(group=subID),
            alpha=0.15,
            position=position_jitter(width=0.1, seed=1234)) +
  geom_point(aes(color=rule),
             position=position_jitter(width=0.1, seed=1234)) +
  geom_tufteboxplot(median.type='line',
                    width=3,
                    voffset=0.01,
                    hoffset=0,
                    position=position_nudge(x=c(-0.2, 0.2))) + 
  # customise
  geom_rangeframe(sides='lb', color='black') +
  geom_rangeframe(data=data.frame(x=c('vmac_away', 'vmac_toward'), y=c(-0.05, 0.025)),
                  aes(x, y), size=1, color='black') +
  scale_x_discrete(name='Rule',
                   labels=c('away', 'toward')) +
  scale_y_continuous(name='VMAC effect (high - low) (ms)',
                     breaks=c(-0.05, -0.025, 0, 0.025),
                     labels=c('-50', '-25', '0', '25'),
                     limits=c(-0.060, 0.040)) + 
    
  my_theme() + 
  scale_color_tableau()
  

# plot relationship between behavioural vmac effect and RDMs
plot_beh_rdm <- ggplot(rslts_beh_by_rdm[effect != 'int', ], 
                       aes(x=time, y=beta)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.6, 0.6)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-0.6, 0.6)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(group=interaction(rule, effect),
                color=rule,
                #linetype=effect
                ),
            linewidth=1) + 
  facet_wrap('effect') + 

  # customise
  scale_y_continuous(name='Beta', 
                     breaks=c(-0.6, -0.3, 0, 0.3, 0.6),
                     labels=c('-0.6', '-0.3', '0.0', '0.3', '0.6'),
                     limits=c(-0.65, 0.65),
                     expand=expansion(mult = 0.1)) + 
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  # scale_color_manual(name='Rule',
  #                    breaks=c('away', 'toward'),
  #                    values=c('away' = '#ed7953',
  #                             'toward' = '#5c01a6'),
  #                    labels=c('away', 'toward')) +
  # scale_fill_manual(name='',
  #                   breaks=c('response', 'response_alt', 'rule', 'rule_alt'),
  #                   values=c('response' = '#ed7953',
  #                            'response_alt' = '#ed7953',
  #                            'rule' = '#5c01a6',
  #                            'rule_alt' = '#5c01a6'),
  #                   labels=c('response', 'response_alt', 'rule', 'rule_alt')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-0.6, 0.6)),
                  aes(x, y), size=1, color='black') +
  my_theme() + 
  scale_color_tableau() + 
  theme(legend.position = 'top')





## Plot time course averaged over number of matching features ------------------
# create model RDM vectors
stim_vec <- c(1, 0, 1, 0, 1, 0, 1, 0)
resp_vec <- c(1, 0, 0, 1, 1, 0, 0, 1)
rule_vec <- c(1, 1, 0, 0, 1, 1, 0, 0)
val_vec <- c(1, 1, 1, 1, 0, 0, 0, 0)

dt_all <- data.table()
for (sub in unique(dfun_data[, subID])) {
  
  # store model RDM vectors in data table
  dt <- data.table(
    class = as.integer(paste0(seq(1, 8), '10')),
    stim = stim_vec,
    resp = resp_vec,
    rule = rule_vec,
    val = val_vec
  )
  # wrangle decoding data into long format
  dat <- transpose(dfun_data[, dfun_110:dfun_810, ])
  dat[, class := as.integer(paste0(seq(1, 8), '10'))]
  dat <- melt(dat, id.vars='class', varaible.name='trial', value.name='dfun')
  d = data.table(
    y = rep(data[, y], each=8),
    tpoint = rep(data[, tpoint], each=8),
    #score = rep(data[, score], each=8),
    subID = rep(data[, subID], each=8)
  ) 
  dat <- cbind(d, dat) # add back ID variables
  dat <- dat[dt, on='class'][order(variable)] # add RDM vectors
  
  ##  check how the decision function unfolds over time
  # dat[, match := ifelse(y==class, 1, 0)] # identify the trial type that matches class of current trial
  # categorise number of feature matches between trial types
  DT <- data.table()
  for (row_1 in seq(1, nrow(dt))) {
    y = dt[row_1, 1]
    for (row_2 in seq(1, nrow(dt))) {
      
      class = dt[row_2, 1]
      comp = dt[row_1, 2:5] == dt[row_2, 2:5]
      row_dt <- data.table(y=as.integer(y),
                           class=as.integer(class),
                           stim=comp[1],
                           resp=comp[2],
                           rule=comp[3],
                           val=comp[4],
                           label='lab')
      DT <- rbind(DT, row_dt)
    }  
  }
  DT[, n_match := rowSums(.SD), .SDcols = c("stim", "resp", "rule", "val")] 
  
  for (row in seq(1, nrow(DT))) {
    # create empty label
    lab = ''
    # add matching variable characters
    if (DT[row, 'stim'] == T) {
      lab <- paste0(lab, 'stim', ".")}
    if (DT[row, 'resp'] == T) {
      lab <- paste0(lab, 'resp', ".")}
    if (DT[row, 'rule'] == T) {
      lab <- paste0(lab, 'rule', ".")}
    if (DT[row, 'val'] == T) {
      lab <- paste0(lab, 'val', ".")}
    DT[row, label := lab]
  }
  # add number of feature matches to main data table
  dat <- dat[DT[, .(y, class, n_match, label)], on=c('y', 'class')]
  dat$n_match <- as.factor(dat$n_match)
  dat$label <- as.factor(dat$label)
  # append output
  dt_all <- rbind(dt_all, dat)
}

# create data table for plotting
p_dat <- dat[, .(meandf=mean(dfun)), by=.(tpoint, label , subID) #n_match
][, .(meanDF=mean(meandf),
      seDF=sd(meandf)/sqrt(length(unique(data$subID)))),
  by=.(tpoint, label)] #n_match
p_dat$t <- rep(seq(from=-0.1, to=1, length.out=282), times=8) # 4

p <- ggplot() + 
  
  # add veritcal lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-1.8, -0.3)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-1.8, -0.3)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(data=p_dat,
            aes(x=t,
                y=meanDF,
                color=label), # n_match
            linewidth=1) +
  # geom_ribbon(data=p_dat,
  #             aes(t,
  #                 y=meanDF,
  #                 ymin=meanDF - seDF,
  #                 ymax=meanDF + seDF,
  #                 fill=label), #n_match
  #             colour=NA,
  #             alpha=0.2,
  #             show.legend=F) +
  # customise
  scale_y_continuous(name='Decision function', 
                     breaks=c(-1.8, -1.5, -1.2, - 0.9, -0.6, -0.3),
                     labels=c('-1.8', '-1.5', '-1.2', '-0.9', '-0.6', '-0.3'),
                     # limits=c(0.375, 1),
                     expand=expansion(mult = 0.1)) + 
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  # scale_color_manual(name='Number of matching variables',
  #                    breaks=c('1', '2', '3', '4'),
  #                    values=c('1' = '#fdb42f', 
  #                             '2' = '#ed7953',
  #                             '3' = '#9c179e',
  #                             '4' = '#5c01a6'),
  #                    labels=c('1', '2', '3', '4')) +
  # scale_fill_manual(name='',
  #                   breaks=c('1', '2', '3', '4'),
  #                   values=c('1' = '#fdb42f', 
  #                             '2' = '#ed7953',
  #                             '3' = '#9c179e',
  #                             '4' = '#5c01a6')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-0.3, -1.8)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')


png(paste0(data_path, 'plt_cue_rc_avg_decFun_8.png'),
    width=8, height=4, units='in', res=400)
plot(p)
dev.off()


