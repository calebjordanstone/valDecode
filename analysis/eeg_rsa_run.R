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
library(stats)
library(BayesFactor)
library(ggthemes)
library(paletteer)
library(corrplot)

# Set data paths
# path_exp <- ""
# path_out <- ""
# path_data <- ""

# Load data 
# Note: loads data files containing decision function values per subject per 
# time point per trial for a given phase (reward/extinction). Change file names
# here to get data from a different phase
files_dfun <- list.files(path_out, pattern = 'dfun_cue_rc_sub')
data_dfun <- rbindlist(lapply(file.path(path_out, files_dfun), fread))

### Define RDMs ----------------------------------------------------------------
# define full model RDMs
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
vec_resp <- c(1, 0, 0, 1, 1, 0, 0, 1,
              0, 1, 1, 0, 0, 1, 1, 0,
              0, 1, 1, 0, 0, 1, 1, 0,
              1, 0, 0, 1, 1, 0, 0, 1,
              1, 0, 0, 1, 1, 0, 0, 1,
              0, 1, 1, 0, 0, 1, 1, 0,
              0, 1, 1, 0, 0, 1, 1, 0,
              1, 0, 0, 1, 1, 0, 0, 1)
mat_resp <- matrix(vec_resp, 8, 8)
vec_rule <- c(1, 1, 0, 0, 1, 1, 0, 0,
              1, 1, 0, 0, 1, 1, 0, 0,
              0, 0, 1, 1, 0, 0, 1, 1,
              0, 0, 1, 1, 0, 0, 1, 1,
              1, 1, 0, 0, 1, 1, 0, 0,
              1, 1, 0, 0, 1, 1, 0, 0,
              0, 0, 1, 1, 0, 0, 1, 1,
              0, 0, 1, 1, 0, 0, 1, 1)
mat_rule <- matrix(vec_rule, 8, 8)
vec_val <- c(1, 1, 1, 1, 0, 0, 0, 0,
             1, 1, 1, 1, 0, 0, 0, 0,
             1, 1, 1, 1, 0, 0, 0, 0,
             1, 1, 1, 1, 0, 0, 0, 0,
             0, 0, 0, 0, 1, 1, 1, 1,
             0, 0, 0, 0, 1, 1, 1, 1,
             0, 0, 0, 0, 1, 1, 1, 1,
             0, 0, 0, 0, 1, 1, 1, 1)
mat_val <-matrix(vec_val, 8)

# turn RDMs into contrast vectors
vec_stim_con <- ifelse(vec_stim == 0, -1, 1)
vec_resp_con <- ifelse(vec_resp == 0, -1, 1)
vec_rule_con <- ifelse(vec_rule == 0, -1, 1)
vec_val_con <- ifelse(vec_val == 0, -1, 1)

# create interaction vectors
vec_stim_by_val <- vec_stim_con*vec_val_con
vec_resp_by_val <- vec_resp_con*vec_val_con
vec_rule_by_val <- vec_rule_con*vec_val_con

# put interaction into non-zero intercept format
vec_stim_by_val <- ifelse(vec_stim_by_val == -1, 0, 1)
vec_resp_by_val <- ifelse(vec_resp_by_val == -1, 0, 1)
vec_rule_by_val <- ifelse(vec_rule_by_val == -1, 0, 1)
mat_stim_by_val <- matrix(vec_stim_by_val, 8)
mat_resp_by_val <- matrix(vec_resp_by_val, 8)
mat_rule_by_val <- matrix(vec_rule_by_val, 8)

### Run analysis ---------------------------------------------------------------
## Cue-locked ------------------------------------------------------------------
n_tpoints = 282 # epochs are from -0.1 s to 1 s relative to cue onset
ts <- seq(-0.1, 1, length.out=n_tpoints) # put time samples into actual times
rslts_cue_rc <- data.table() # create empty data.table to store results
rslts_cue_rc_by_val_int <- data.table()

for (sub in unique(data_dfun[, subID])) {
  
  # update progress 
  print(paste0('Analysing ', sub))
  
  ## Wrangling data ##
  # put data_dfun into long format
  data_dfun_sub <- data_dfun[subID == sub,]
  data_dfun_sub <- data_dfun_sub[, 
                                 lapply(.SD, mean), 
                                 .SDcols = dfun_110:dfun_810, 
                                 by=c('event_type', 'event_group', 'RT', 
                                      'y', 'tpoint', 'subID')]
  data_dfun_long <- transpose(data_dfun_sub[, dfun_110:dfun_810])
  data_dfun_long[, class := as.integer(paste0(seq(1, 8), '10'))]
  data_dfun_long <- melt(data_dfun_long, id.vars='class',
                         variable.name='trial_by_tpoint',
                         value.name='dfun')
  
  # create data table of ID variables
  data_dfun_ids = data.table( 
    y = rep(data_dfun_sub[, y], each=8),
    tpoint = rep(data_dfun_sub[, tpoint], each=8),
    event_group = rep(data_dfun_sub[, event_group], each=8),
    RT = rep(data_dfun_sub[, RT], each=8),
    subID = rep(data_dfun_sub[, subID], each=8))
  data_dfun_long <- cbind(data_dfun_ids, data_dfun_long) # add ID variables back to data
  setorder(data_dfun_long, y, tpoint, trial_by_tpoint, class) # make sure things are ordered correctly

  # add RDM vectors
  class_N <- data_dfun_long[, .N , by=y]
  repeats <- class_N[1, 2]/8
  rule_vec <- c(rep(mat_rule[, 1], times=repeats), # 110
                rep(mat_rule[, 2], times=repeats), # 210
                rep(mat_rule[, 3], times=repeats), # 310
                rep(mat_rule[, 4], times=repeats), # 410
                rep(mat_rule[, 5], times=repeats), # 510
                rep(mat_rule[, 6], times=repeats), # 610
                rep(mat_rule[, 7], times=repeats), # 710
                rep(mat_rule[, 8], times=repeats)) # 810
  stim_vec <- c(rep(mat_stim[, 1], times=repeats), # 110
                rep(mat_stim[, 2], times=repeats), # 210
                rep(mat_stim[, 3], times=repeats), # 310
                rep(mat_stim[, 4], times=repeats), # 410
                rep(mat_stim[, 5], times=repeats), # 510
                rep(mat_stim[, 6], times=repeats), # 610
                rep(mat_stim[, 7], times=repeats), # 710
                rep(mat_stim[, 8], times=repeats)) # 810
  resp_vec <- c(rep(mat_resp[, 1], times=repeats), # 110
                rep(mat_resp[, 2], times=repeats), # 210
                rep(mat_resp[, 3], times=repeats), # 310
                rep(mat_resp[, 4], times=repeats), # 410
                rep(mat_resp[, 5], times=repeats), # 510
                rep(mat_resp[, 6], times=repeats), # 610
                rep(mat_resp[, 7], times=repeats), # 710
                rep(mat_resp[, 8], times=repeats)) # 810
  val_vec <- c(rep(mat_val[, 1], times=repeats), # 110
               rep(mat_val[, 2], times=repeats), # 210
               rep(mat_val[, 3], times=repeats), # 310
               rep(mat_val[, 4], times=repeats), # 410
               rep(mat_val[, 5], times=repeats), # 510
               rep(mat_val[, 6], times=repeats), # 610
               rep(mat_val[, 7], times=repeats), # 710
               rep(mat_val[, 8], times=repeats)) # 810
  rule_by_val_vec <- c(rep(mat_rule_by_val[, 1], times=repeats), # 110
                       rep(mat_rule_by_val[, 2], times=repeats), # 210
                       rep(mat_rule_by_val[, 3], times=repeats), # 310
                       rep(mat_rule_by_val[, 4], times=repeats), # 410
                       rep(mat_rule_by_val[, 5], times=repeats), # 510
                       rep(mat_rule_by_val[, 6], times=repeats), # 610
                       rep(mat_rule_by_val[, 7], times=repeats), # 710
                       rep(mat_rule_by_val[, 8], times=repeats)) # 810
  stim_by_val_vec <- c(rep(mat_stim_by_val[, 1], times=repeats), # 110
                       rep(mat_stim_by_val[, 2], times=repeats), # 210
                       rep(mat_stim_by_val[, 3], times=repeats), # 310
                       rep(mat_stim_by_val[, 4], times=repeats), # 410
                       rep(mat_stim_by_val[, 5], times=repeats), # 510
                       rep(mat_stim_by_val[, 6], times=repeats), # 610
                       rep(mat_stim_by_val[, 7], times=repeats), # 710
                       rep(mat_stim_by_val[, 8], times=repeats)) # 810
  resp_by_val_vec <- c(rep(mat_resp_by_val[, 1], times=repeats), # 110
                       rep(mat_resp_by_val[, 2], times=repeats), # 210
                       rep(mat_resp_by_val[, 3], times=repeats), # 310
                       rep(mat_resp_by_val[, 4], times=repeats), # 410
                       rep(mat_resp_by_val[, 5], times=repeats), # 510
                       rep(mat_resp_by_val[, 6], times=repeats), # 610
                       rep(mat_resp_by_val[, 7], times=repeats), # 710
                       rep(mat_resp_by_val[, 8], times=repeats)) # 810
  # add vectors to data table
  data_dfun_long[, ':=' (rule = rule_vec,
                         stim = stim_vec,
                         resp = resp_vec,
                         val = val_vec,
                         conj = ifelse(y==class, 1, 0),
                         rule_by_val = rule_by_val_vec,
                         stim_by_val = stim_by_val_vec,
                         resp_by_val = resp_by_val_vec)]
  
  # make sure everything is ordered properly
  setorder(data_dfun_long, trial_by_tpoint, class)
  
  ## Running analysis ##
  
  # 1) Loop through time points and run analysis for all data conditions
  for (t in seq(0, n_tpoints - 1)) { # subtract 1 to account for Python indexing starting at 0

    # subset data per time point
    data_dfun_t <- data_dfun_long[tpoint == t, ]

    # compute model
    mdl_t <- lsfit(x=data_dfun_t[, .(rule, stim, resp, val, conj)], 
                   y=data_dfun_t[, dfun])

    # add betas to data table
    rslts_t <- data.table(
      subID = sub,
      tpoint=t,
      time = ts[t+1],
      int = mdl_t$coefficients['Intercept'],
      rule = mdl_t$coefficients['rule'],
      stim = mdl_t$coefficients['stim'],
      resp = mdl_t$coefficients['resp'],
      val = mdl_t$coefficients['val'],
      conj = mdl_t$coefficients['conj']
    )

    # append data from current time point to overall data table
    rslts_cue_rc <- rbind(rslts_cue_rc, rslts_t)
  }
  
  # 2) Run analysis with interaction with value
  for (t in seq(0, n_tpoints - 1)) {

    # subset data per time point
    data_dfun_t <- data_dfun_long[tpoint==t, ]

    # compute model
    mdl_rule_t <- lsfit(x=data_dfun_t[, .(rule, stim, resp, val, conj, rule_by_val)],
                        y=data_dfun_t[, dfun])
    mdl_stim_t <- lsfit(x=data_dfun_t[, .(rule, stim, resp, val, conj, stim_by_val)], 
                        y=data_dfun_t[, dfun])
    mdl_resp_t <- lsfit(x=data_dfun_t[, .(rule, stim, resp, val, conj, resp_by_val)], 
                        y=data_dfun_t[, dfun])

    # add betas to data table
    rslts_t <- data.table(
      subID = sub,
      tpoint=t,
      time = ts[t+1],
      # rule
      rule_int = mdl_rule_t$coefficients['Intercept'],
      rule_rule = mdl_rule_t$coefficients['rule'],
      rule_stim = mdl_rule_t$coefficients['stim'],
      rule_resp = mdl_rule_t$coefficients['resp'],
      rule_val= mdl_rule_t$coefficients['val'],
      rule_conj = mdl_rule_t$coefficients['conj'],
      rule_by_val = mdl_rule_t$coefficients['rule_by_val'],
      # stim
      stim_int = mdl_stim_t$coefficients['Intercept'],
      stim_rule = mdl_stim_t$coefficients['rule'],
      stim_stim = mdl_stim_t$coefficients['stim'],
      stim_resp = mdl_stim_t$coefficients['resp'],
      stim_val = mdl_stim_t$coefficients['val'],
      stim_conj = mdl_stim_t$coefficients['conj'],
      stim_by_val = mdl_stim_t$coefficients['stim_by_val'],
      # resp
      resp_int = mdl_resp_t$coefficients['Intercept'],
      resp_rule = mdl_resp_t$coefficients['rule'],
      resp_stim = mdl_resp_t$coefficients['stim'],
      resp_resp = mdl_resp_t$coefficients['resp'],
      resp_val = mdl_resp_t$coefficients['val'],
      resp_conj = mdl_resp_t$coefficients['conj'],
      resp_by_val = mdl_resp_t$coefficients['resp_by_val']
    )

    # append data from current time point to overall data table
    rslts_cue_rc_by_val_int <- rbind(rslts_cue_rc_by_val_int, rslts_t)
  }
}

# save results
write_csv(rslts_cue_rc, paste0(path_out, 'rslts_cue_rc.csv'))
write_csv(rslts_cue_rc_by_val_int, paste0(path_out, 'rslts_cue_rc_by_val_int.csv'))

