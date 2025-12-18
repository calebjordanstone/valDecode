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

# set data paths
path_exp <- "C:/Users/cstone/OneDrive - UNSW/Documents/Projects/my_experiments/val_decode/"
path_out <- paste0(path_exp, 'output/')
path_data <- paste0(path_exp, 'data/')

# load data
files_beh <- list.files(path_data, pattern = '.*(beh.txt)', recursive = T)
# data_beh <- fread(file.path(path_data, files_beh[1]), stringsAsFactors = T)
data_beh <- rbindlist(lapply(file.path(path_data, files_beh), fread))
files_dfun <- list.files(path_out, pattern = 'dfun_cue_rc_avg_etc_sub') ################ cue-locked, reward contingency blocks - change here for different epochs
data_dfun <- rbindlist(lapply(file.path(path_out, files_dfun), fread))
# files_dfun_by_val <- list.files(path_out, pattern = 'dfun_by_val_cue_rc_avg_sub') ## Cue-locked, reward-contingency blocks - change here for different epochs 
# data_dfun_by_val <- rbindlist(lapply(file.path(path_out, files_dfun_by_val), fread))
# files_dcd_by_val <- list.files(path_out, pattern = 'dcd_by_val_cue_rc_avg_etc_sub') ## Cue-locked, reward-contingency blocks - change here for different epochs 
# data_dcd_by_val <- rbindlist(lapply(file.path(path_out, files_dcd_by_val), fread))

### Define matrices -------------------------------------------------------------
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


# vec_stim_by_val <- c(1, 0, 1, 0, 0, 1, 0, 1,
#                      0, 1, 0, 1, 1, 0, 1, 0,
#                      1, 0, 1, 0, 0, 1, 0, 1,
#                      0, 1, 0, 1, 1, 0, 1, 0,
#                      0, 1, 0, 1, 1, 0, 1, 0,
#                      1, 0, 1, 0, 0, 1, 0, 1,
#                      0, 1, 0, 1, 1, 0, 1, 0,
#                      1, 0, 1, 0, 0, 1, 0, 1)
# mat_stim_by_val <- matrix(vec_stim_by_val, 8)
# vec_stim_by_val_resp <- c(1, 0, 0, 1, 0, 1, 1, 0,
#                           0, 1, 1, 0, 1, 0, 0, 1,
#                           0, 1, 1, 0, 1, 0, 0, 1,
#                           1, 0, 0, 1, 0, 1, 1, 0,
#                           0, 1, 1, 0, 1, 0, 0, 1,
#                           1, 0, 0, 1, 0, 1, 1, 0,
#                           1, 0, 0, 1, 0, 1, 1, 0,
#                           0, 1, 1, 0, 1, 0, 0, 1)
# mat_stim_by_val_resp <- matrix(vec_stim_by_val_resp, 8)
# vec_stim_by_val <- c(1, -1, 1, -1, 0, 0, 0, 0, 
#                      -1, 1, -1, 1, 0, 0, 0, 0, 
#                      1, -1, 1, -1, 0, 0, 0, 0, 
#                      -1, 1, -1, 1, 0, 0, 0, 0, 
#                      0, 0, 0, 0, 1, -1, 1, -1, 
#                      0, 0, 0, 0, -1, 1, -1, 1, 
#                      0, 0, 0, 0, 1, -1, 1, -1, 
#                      0, 0, 0, 0, -1, 1, -1, 1)
# mat_stim_by_val <- matrix(vec_stim_by_val, 8)

# vec_resp_by_val <- c(1, 0, 1, 0, 0, 1, 1, 0,
#                      0, 1, 0, 1, 1, 0, 0, 1,
#                      1, 0, 1, 0, 0, 1, 1, 0,
#                      0, 1, 0, 1, 1, 0, 0, 1,
#                      0, 1, 0, 1, 1, 0, 0, 1,
#                      1, 0, 1, 0, 0, 1, 1, 0,
#                      1, 0, 1, 0, 0, 1, 1, 0,
#                      0, 1, 0, 1, 1, 0, 0, 1)
# mat_resp_by_val <- matrix(vec_resp_by_val, 8, 8, byrow=T)
# vec_resp_by_val2 <- c(1, 0, 0, 1, 0, 1, 0, 1,
#                       0, 1, 1, 0, 1, 0, 1, 0,
#                       0, 1, 1, 0, 1, 0, 1, 0,
#                       1, 0, 0, 1, 0, 1, 0, 1,
#                       0, 1, 1, 0, 1, 0, 1, 0,
#                       1, 0, 0, 1, 0, 1, 0, 1,
#                       0, 1, 1, 0, 1, 0, 1, 0,
#                       1, 0, 0, 1, 0, 1, 0, 1)
# mat_resp_by_val2 <- matrix(vec_resp_by_val2, 8, 8, byrow=T)
# vec_resp_by_val3 <- c(1, 0, 1, 0, 1, 0, 0, 1,
#                       0, 1, 0, 1, 0, 1, 1, 0,
#                       1, 0, 1, 0, 1, 0, 0, 1,
#                       0, 1, 0, 1, 0, 1, 1, 0,
#                       1, 0, 1, 0, 1, 0, 0, 1,
#                       0, 1, 0, 1, 0, 1, 1, 0,
#                       0, 1, 0, 1, 0, 1, 1, 0,
#                       1, 0, 1, 0, 1, 0, 0, 1)
# mat_resp_by_val3 <- matrix(vec_resp_by_val3, 8, 8, byrow=T)

# vec_rule_by_val <- c(1, 1, 1, 1, 0, 0, 1, 1,
#                      1, 1, 1, 1, 0, 0, 1, 1,
#                      1, 1, 1, 1, 0, 0, 1, 1,
#                      1, 1, 1, 1, 0, 0, 1, 1,
#                      0, 0, 0, 0, 1, 1, 0, 0,
#                      0, 0, 0, 0, 1, 1, 0, 0,
#                      1, 1, 1, 1, 0, 0, 1, 1,
#                      1, 1, 1, 1, 0, 0, 1, 1)
# mat_rule_by_val <- matrix(vec_rule_by_val, 8, 8, byrow=T)


# define model RDMs wout value
# vec_stim_no_val <- c(1, 0, 1, 0, 
#                      0, 1, 0, 1, 
#                      1, 0, 1, 0,
#                      0, 1, 0, 1)
# mat_stim_no_val <- matrix(vec_stim_no_val, 4)
# vec_resp_no_val <- c(1, 0, 0, 1,
#                      0, 1, 1, 0,
#                      0, 1, 1, 0,
#                      1, 0, 0, 1)
# mat_resp_no_val <- matrix(vec_resp_no_val, 4)
# vec_rule_no_val <- c(1, 1, 0, 0,
#                      1, 1, 0, 0,
#                      0, 0, 1, 1,
#                      0, 0, 1, 1)
# mat_rule_no_val <- matrix(vec_rule_no_val, 4)



# # swap 1s and 0s around so intercept is the conjunction
# vec_stim_zero <- ifelse(vec_stim == 0, 1, 0)
# vec_resp_zero <- ifelse(vec_resp == 0, 1, 0)
# vec_rule_zero <- ifelse(vec_rule == 0, 1, 0)
# vec_val_zero <- ifelse(vec_val == 0, 1, 0)
# 
# mat_stim_zero <- matrix(vec_stim_zero, 8)
# mat_resp_zero <- matrix(vec_resp_zero, 8)
# mat_rule_zero <- matrix(vec_rule_zero, 8)
# mat_val_zero <- matrix(vec_val_zero, 8)

# turn RDMs into contrast vectors
vec_stim_con <- ifelse(vec_stim == 0, -1, 1)
vec_resp_con <- ifelse(vec_resp == 0, -1, 1)
vec_rule_con <- ifelse(vec_rule == 0, -1, 1)
vec_val_con <- ifelse(vec_val == 0, -1, 1)

# mat_stim_con <- matrix(vec_stim_con, 8)
# mat_resp_con <- matrix(vec_resp_con, 8)
# mat_rule_con <- matrix(vec_rule_con, 8)
# mat_val_con <- matrix(vec_val_con, 8)

# create interaction vectors
vec_stim_by_val <- vec_stim_con*vec_val_con
vec_resp_by_val <- vec_resp_con*vec_val_con
vec_rule_by_val <- vec_rule_con*vec_val_con

# # put interaction vector into zero-intercept format
# vec_stim_by_val_zero <- ifelse(vec_stim_by_val == 1, 0, 1)
# vec_resp_by_val_zero <- ifelse(vec_resp_by_val == 1, 0, 1)
# vec_rule_by_val_zero <- ifelse(vec_rule_by_val == 1, 0, 1)
# 
# mat_stim_by_val_zero <- matrix(vec_stim_by_val_zero, 8)
# mat_resp_by_val_zero <- matrix(vec_resp_by_val_zero, 8)
# mat_rule_by_val_zero <- matrix(vec_rule_by_val_zero, 8)

# put interaction into non-zero intercept format
vec_stim_by_val <- ifelse(vec_stim_by_val == -1, 0, 1)
vec_resp_by_val <- ifelse(vec_resp_by_val == -1, 0, 1)
vec_rule_by_val <- ifelse(vec_rule_by_val == -1, 0, 1)

mat_stim_by_val <- matrix(vec_stim_by_val, 8)
mat_resp_by_val <- matrix(vec_resp_by_val, 8)
mat_rule_by_val <- matrix(vec_rule_by_val, 8)

# heck correlations between vectors
vec_cor <- data.table(
  int = rep(1, times=64), 
  stim = vec_stim,
  #stim_zero = vec_stim_zero,
  stim_by_val = vec_stim_by_val,
  resp = vec_resp,
  #resp_zero = vec_resp_zero,
  resp_by_val = vec_resp_by_val,
  rule = vec_rule,
  #rule_zero = vec_rule_zero,
  rule_by_val = vec_rule_by_val,
  val = vec_val,
  #val_zero = vec_val_zero,
  conj = as.vector(diag(1, 8))
)
cor_mat <- cor(vec_cor)

corrplot(cor_mat,
         method = "color",
         type = "lower",
         addCoef.col = "black",
         number.cex=1.5,
         tl.cex = 1.5,
         cl.cex=1)

### Run analysis ---------------------------------------------------------------
## Cue-locked ------------------------------------------------------------------
n_tpoints = 282 # epochs are from -0.1 s to 1 s relative to cue onset
ts <- seq(-0.1, 1, length.out=n_tpoints) # put time samples into actual times
#ts <- seq(-1, 0.1, length.out=n_tpoints)

rslts_cue_rc_all <- data.table()
# rslts_cue_rc_by_val <- data.table()
rslts_cue_rc_by_val_int <- data.table()
# rslts_cue_rc_int <- data.table()
# rslts_cue_rc_beh <- data.table()
for (sub in unique(data_dfun[, subID])) {
  
  # update progress 
  print(paste0('Analysing ', sub))
  
  ## Wrangling data ##
  # get condition RTs
  data_beh_sub <- data_beh[Subject == sub & Accuracy == 1 & Block < 13] ## change this if looking at extinction blocks
  data_beh_sub_av <- data_beh_sub[,
                                  .(MeanRT=mean(RT)),
                                  by = .(DistractorValue, ResponseRule, TargetPosition)
  ][order(DistractorValue, -ResponseRule, TargetPosition)]
  data_beh_sub_av[, ':=' (zMeanRT = scale(MeanRT),
                          class = as.integer(paste0(seq(1, 8), '10')))]
  
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
  # data_dfun_long[, ':=' (
  #   stim = case_when(
  #     unique(y) %in% c(110, 310, 510, 710) ~ mat_stim[, 1],
  #     T ~ mat_stim[, 2]),
  #   resp = case_when(
  #     unique(y) %in% c(110, 410, 510, 810) ~ mat_resp[, 1],
  #     T ~ mat_resp[, 2]),
  #   rule = case_when(
  #     unique(y) %in% c(110, 210, 510, 610) ~ mat_rule[, 1],
  #     T ~ mat_rule[, 3]),
  #   val = case_when(
  #     unique(y) %in% c(110, 210, 310, 410) ~ mat_val[, 1],
  #     T ~ mat_val[, 5]),
  #   conj = case_when(
  #     y == class ~ 1,
  #     T ~ 0),
  #   stim_by_val = case_when(
  #     unique(y) %in% c(110, 310, 610, 810) ~ mat_stim_by_val[, 1],
  #     T ~ mat_stim_by_val[, 2]),
  #   resp_by_val = case_when(
  #     unique(y) %in% c(110, 410, 610, 710) ~ mat_resp_by_val[, 1],
  #     T ~ mat_resp_by_val[, 2]),
  #   rule_by_val = case_when(
  #     unique(y) %in% c(110, 210, 710, 810) ~ mat_rule_by_val[, 1],
  #     T ~ mat_rule_by_val[, 3])
  # ),
  # by = trial_by_tpoint]
  # 
  
  class_N <- data_dfun_long[, .N , by=y]
  repeats <- class_N[1, 2]/8
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
  rule_vec <- c(rep(mat_rule[, 1], times=repeats), # 110
                rep(mat_rule[, 2], times=repeats), # 210
                rep(mat_rule[, 3], times=repeats), # 310
                rep(mat_rule[, 4], times=repeats), # 410
                rep(mat_rule[, 5], times=repeats), # 510
                rep(mat_rule[, 6], times=repeats), # 610
                rep(mat_rule[, 7], times=repeats), # 710
                rep(mat_rule[, 8], times=repeats)) # 810
  val_vec <- c(rep(mat_val[, 1], times=repeats), # 110
               rep(mat_val[, 2], times=repeats), # 210
               rep(mat_val[, 3], times=repeats), # 310
               rep(mat_val[, 4], times=repeats), # 410
               rep(mat_val[, 5], times=repeats), # 510
               rep(mat_val[, 6], times=repeats), # 610
               rep(mat_val[, 7], times=repeats), # 710
               rep(mat_val[, 8], times=repeats)) # 810
  # add vectors to data table
  data_dfun_long[, ':=' (stim = stim_vec,
                         resp = resp_vec,
                         rule = rule_vec,
                         val = val_vec,
                         conj =ifelse(y==class, 1, 0))]
  
  
  # add RT vector 
  data_dfun_long <- data_dfun_long[data_beh_sub_av[, .(zMeanRT, class)], on='class']
  setorder(data_dfun_long, trial_by_tpoint, class)
  
  # # remove conjunction rows
  # data_dfun_long_noconj <- data_dfun_long[conj != 1,]
  
  # put data_dfun_by_val into long format
  # data_dfun_by_val_sub <- data_dfun_by_val[subID == sub,]
  # data_dfun_by_val_long <- transpose(data_dfun_by_val_sub[, dfun_tl:dfun_ar, ])
  # data_dfun_by_val_long[, class := c('tl', 'tr', 'al', 'ar')]
  # data_dfun_by_val_long <- melt(data_dfun_by_val_long,
  #                               id.vars='class',
  #                               variable.name='trial_by_tpoint',
  #                               value.name='dfun')
  # data_dfun_by_val_ids = data.table( # create data table of ID variables
  #   y = rep(data_dfun_by_val_sub[, y], each=4),
  #   tpoint = rep(data_dfun_by_val_sub[, tpoint], each=4),
  #   subID = rep(data_dfun_by_val_sub[, subID], each=4),
  #   value = rep(data_dfun_by_val_sub[, value], each=4))
  # data_dfun_by_val_long <- cbind(data_dfun_by_val_ids,
  #                                data_dfun_by_val_long) # add ID variables back to data
  # data_dfun_by_val_long$class <- factor(data_dfun_by_val_long$class,
  #                                       levels = c("tl", "tr", "al", "ar"))
  # setorder(data_dfun_by_val_long, value, y, tpoint, trial_by_tpoint, class) # make sure things are ordered correctly
  # 
  # # add in RDM vectors for each trial type
  # data_dfun_by_val_long[, ':='
  #                (stim = case_when(
  #                  unique(y) %in% c(110, 310, 510, 710) ~ mat_stim_no_val[, 1],
  #                  T ~ mat_stim_no_val[, 2]),
  #                 resp = case_when(
  #                  unique(y) %in% c(110, 410, 510, 810) ~ mat_resp_no_val[, 1],
  #                  T ~ mat_resp_no_val[, 2]),
  #                 rule = case_when(
  #                  unique(y) %in% c(110, 210, 510, 610) ~ mat_rule_no_val[, 1],
  #                  T ~ mat_rule_no_val[, 3]),
  #                 conj = case_when(
  #                  (unique(y) %in% c(110, 510)) & (class == 'tl') ~ 1,
  #                  (unique(y) %in% c(210, 610)) & (class == 'tr') ~ 1,
  #                  (unique(y) %in% c(310, 710)) & (class == 'al') ~ 1,
  #                  (unique(y) %in% c(410, 810)) & (class == 'ar') ~ 1,
  #                  T ~ 0)),
  #                by = trial_by_tpoint]
  # 
  # # remove conjunction
  # data_dfun_by_val_long_noconj <- data_dfun_by_val_long[conj != 1,]
  
  ## Running analysis ## 
  
  # 1) Loop through time points and run analysis for all data conditions
  for (t in seq(0, n_tpoints - 1)) { # subtract 1 to account for Python indexing starting at 0

    # subset data per time point
    data_dfun_t <- data_dfun_long[tpoint == t, ] # data_dfun_long

    # compute model
    # mdl <- lm(dfun ~ stim + resp + rule + val, data = t_dat)
    mdl_t <- lsfit(x=data_dfun_t[, .(rule, stim, resp, val, conj, zMeanRT)],
                   y=data_dfun_t[, dfun])

    # mdl_t_zero <- lsfit(x=data_dfun_t[, .(rule_zero, stim_zero, resp_zero, val_zero, conj_zero)], # note - removed conj
    #                y=data_dfun_t[, dfun])
    #

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
      conj = mdl_t$coefficients['conj'],
      rt = mdl_t$coefficients['zMeanRT']
      )

    # append data from current time point to overall data table
    rslts_cue_rc_all <- rbind(rslts_cue_rc_all, rslts_t)
  }

  # 2) Run analysis separately for high and low value trials
  # for (val in c('hi', 'lo')) { ################################################# Also try running this with value as a predictor
  # 
  #   # subset trials by value
  #   data_dfun_val <- data_dfun_by_val_long[value==val, ] #  data_dfun_by_val_long_noconj
  # 
  #   # loop through times
  #   for (t in seq(0, n_tpoints - 1)) {
  # 
  #     # subset data per time point
  #     data_dfun_val_t <- data_dfun_val[tpoint==t, ]
  # 
  #     # compute model
  #     mdl_t <- lsfit(x=data_dfun_val_t[, .(stim, resp, rule, conj)], # conj
  #                    y=data_dfun_val_t[, dfun])
  #     
  #     d <- data_dfun_val_t[1:40, rule:conj]
  #     #d$trial <- rep(seq(1:5), each=4)
  #     d$y <- seq(1:40)
  #     d_melt <- melt(d, 
  #                    id.vars='y', 
  #                    measure.vars = c('rule', 'conj'))
  #     d_melt$value <- as.factor(d_melt$value)
  #     
  #     
  #     ggplot() + 
  #       geom_tile(data=d_melt, 
  #                 aes(x=variable, 
  #                     y=-y,
  #                     fill=value),
  #                 color='grey') + 
  #       scale_fill_manual(values=c('white', 'black')) + 
  #       theme_minimal() + theme(axis.text.x = element_text(size=14))
  #     
  # 
  #     mdl_t_stim <- lsfit(x=data_dfun_val_t[, .(stim, conj)],
  #                         y=data_dfun_val_t[, dfun])
  #     mdl_t_resp <- lsfit(x=data_dfun_val_t[, .(resp, conj)],
  #                         y=data_dfun_val_t[, dfun])
  #     mdl_t_rule <- lsfit(x=data_dfun_val_t[, .(rule, conj)],
  #                         y=data_dfun_val_t[, dfun])
  # 
  #     # add betas to data table
  #     rslts_t <- data.table(
  #       subID = sub,
  #       value = val,
  #       tpoint=t,
  #       time = ts[t+1],
  #       # int = mdl_t$coefficients['Intercept'],
  #       # stim = mdl_t$coefficients['stim'],
  #       # resp = mdl_t$coefficients['resp'],
  #       # rule = mdl_t$coefficients['rule']
  #       #conj = mdl_t$coefficients['conj']
  #       stim = mdl_t_stim$coefficients['stim'],
  #       stim_conj = mdl_t_stim$coefficients['conj'],
  #       resp = mdl_t_resp$coefficients['resp'],
  #       resp_conj = mdl_t_resp$coefficients['conj'],
  #       rule = mdl_t_rule$coefficients['rule'],
  #       rule_conj = mdl_t_rule$coefficients['conj']
  #       )
  # 
  #     # append data from current time point to overall data table
  #     rslts_by_val <- rbind(rslts_by_val, rslts_t)
  #   }
  # }

  # 2.5) Run analysis with interaction with value
  # for (t in seq(0, n_tpoints - 1)) {
  # 
  #   # subset data per time point
  #   data_dfun_val_t <- data_dfun_long[tpoint==t, ]
  # 
  # 
  #   # compute model
  #   # mdl_rule_t <- lsfit(x=data_dfun_val_t[, .(rule, stim, resp, val, conj, rule_by_val, zMeanRT)],
  #   #                     y=data_dfun_val_t[, dfun])
  #   # mdl_stim_t <- lsfit(x=data_dfun_val_t[, .(rule, stim, resp, val, conj, stim_by_val, zMeanRT)],
  #   #                     y=data_dfun_val_t[, dfun])
  #   # mdl_resp_t <- lsfit(x=data_dfun_val_t[, .(rule, stim, resp, val, conj, resp_by_val, zMeanRT)],
  #   #                     y=data_dfun_val_t[, dfun])
  #   
  #   # mdl_rule_t_zero <- lsfit(x=data_dfun_val_t[, .(rule_zero, stim_zero, resp_zero, val_zero, conj_zero, rule_by_val_zero)],
  #   #                     y=data_dfun_val_t[, dfun])
  #   # mdl_stim_t_zero <- lsfit(x=data_dfun_val_t[, .(rule_zero, stim_zero, resp_zero, val_zero, conj_zero, stim_by_val_zero)],
  #   #                     y=data_dfun_val_t[, dfun])
  #   # mdl_resp_t_zero <- lsfit(x=data_dfun_val_t[, .(rule_zero, stim_zero, resp_zero, val_zero, conj_zero, resp_by_val_zero)],
  #   #                     y=data_dfun_val_t[, dfun])
  #   
  #   mdl <- lm(dfun ~ rule + stim + resp + val + conj + rule_by_val + stim_by_val + resp_by_val + zMeanRT, data=data_dfun_val_t)
  #   mdl_t <- lsfit(x=data_dfun_val_t[, .(rule, stim, resp, val, conj, rule_by_val, stim_by_val, resp_by_val, zMeanRT)],
  #                  y=data_dfun_val_t[, dfun])
  # 
  #   # add betas to data table
  #   rslts_t <- data.table(
  #     subID = sub,
  #     tpoint=t,
  #     time = ts[t+1],
  #     int = mdl_t$coeffivients['Intercept'],
  #     rule = mdl_t$coefficients['rule'],
  #     stim = mdl_t$coefficients['stim'],
  #     resp = mdl_t$coefficients['resp'],
  #     val= mdl_t$coefficients['val'],
  #     conj = mdl_t$coefficients['conj'],
  #     rule_by_val = mdl_t$coefficients['rule_by_val'],
  #     stim_by_val = mdl_t$coefficients['stim_by_val'],
  #     resp_by_val = mdl_t$coefficients['resp_by_val'],
  #     rt = mdl_t$coefficients['zMeanRT']
  #   #   # rule
  #   #   rule_int = mdl_rule_t$coefficients['Intercept'],
  #   #   rule_rule = mdl_rule_t$coefficients['rule'],
  #   #   rule_stim = mdl_rule_t$coefficients['stim'],
  #   #   rule_resp = mdl_rule_t$coefficients['resp'],
  #   #   rule_val= mdl_rule_t$coefficients['val'],
  #   #   rule_conj = mdl_rule_t$coefficients['conj'],
  #   #   rule_by_val = mdl_rule_t$coefficients['rule_by_val'],
  #   #   rule_rt = mdl_rule_t$coefficients['zMeanRT'],
  #   #   # stim
  #   #   stim_int = mdl_stim_t$coefficients['Intercept'],
  #   #   stim_rule = mdl_stim_t$coefficients['rule'],
  #   #   stim_stim = mdl_stim_t$coefficients['stim'],
  #   #   stim_resp = mdl_stim_t$coefficients['resp'],
  #   #   stim_val = mdl_stim_t$coefficients['val'],
  #   #   stim_conj = mdl_stim_t$coefficients['conj'],
  #   #   stim_by_val = mdl_stim_t$coefficients['stim_by_val'],
  #   #   stim_rt = mdl_stim_t$coefficients['zMeanRT'],
  #   #   # resp
  #   #   resp_int = mdl_resp_t$coefficients['Intercept'],
  #   #   resp_rule = mdl_resp_t$coefficients['rule'],
  #   #   resp_stim = mdl_resp_t$coefficients['stim'],
  #   #   resp_resp = mdl_resp_t$coefficients['resp'],
  #   #   resp_val = mdl_resp_t$coefficients['val'],
  #   #   resp_conj = mdl_resp_t$coefficients['conj'],
  #   #   resp_by_val = mdl_resp_t$coefficients['resp_by_val'],
  #   #   resp_rt = mdl_resp_t$coefficients['zMeanRT']
  #     # # rule zero
  #     # rule_int_zero = mdl_rule_t_zero$coefficients['Intercept'],
  #     # rule_rule_zero = mdl_rule_t_zero$coefficients['rule_zero'],
  #     # rule_stim_zero = mdl_rule_t_zero$coefficients['stim_zero'],
  #     # rule_resp_zero = mdl_rule_t_zero$coefficients['resp_zero'],
  #     # rule_val_zero= mdl_rule_t_zero$coefficients['val_zero'],
  #     # rule_conj_zero = mdl_rule_t_zero$coefficients['conj_zero'],
  #     # rule_by_val_zero = mdl_rule_t_zero$coefficients['rule_by_val_zero'],
  #     # # stim zero
  #     # stim_int_zero = mdl_stim_t_zero$coefficients['Intercept'],
  #     # stim_rule_zero = mdl_stim_t_zero$coefficients['rule_zero'],
  #     # stim_stim_zero = mdl_stim_t_zero$coefficients['stim_zero'],
  #     # stim_resp_zero = mdl_stim_t_zero$coefficients['resp_zero'],
  #     # stim_val_zero = mdl_stim_t_zero$coefficients['val_zero'],
  #     # stim_conj_zero = mdl_stim_t_zero$coefficients['conj_zero'],
  #     # stim_by_val_zero = mdl_stim_t_zero$coefficients['stim_by_val_zero'],
  #     # # resp zero
  #     # resp_int_zero = mdl_resp_t_zero$coefficients['Intercept'],
  #     # resp_rule_zero = mdl_resp_t_zero$coefficients['rule_zero'],
  #     # resp_stim_zero = mdl_resp_t_zero$coefficients['stim_zero'],
  #     # resp_resp_zero = mdl_resp_t_zero$coefficients['resp_zero'],
  #     # resp_val_zero = mdl_resp_t_zero$coefficients['val_zero'],
  #     # resp_conj_zero = mdl_resp_t_zero$coefficients['conj_zero'],
  #     # resp_by_val_zero = mdl_resp_t_zero$coefficients['resp_by_val_zero']
  #   )
  # 
  #   # append data from current time point to overall data table
  #   rslts_cue_rc_by_val_int <- rbind(rslts_cue_rc_by_val_int, rslts_t)
  # }

  # 3) Run analysis for alternate rule and response RDMs
  # for (t in seq(0, n_tpoints - 1)) { # subtract 1 to account for Python indexing starting at 0
  # 
  #   # subset data per time point
  #   data_dfun_t <- data_dfun_long[tpoint == t, ]
  # 
  #   # compute models
  #   mdl_t_stim <- lsfit(x=data_dfun_t[, .(stim, val, stim_by_val, conj)],
  #                       y=data_dfun_t[, dfun])
  #   # mdl_t_stim2 <- lsfit(x=data_dfun_t[, .(stim, stim_by_val, stim_by_val_resp, conj)], ## 22/10/25 add this next time
  #   #                      y=data_dfun_t[, dfun])
  # 
  #   mdl_t_resp <- lsfit(x=data_dfun_t[, .(resp, val, resp_by_val, conj)],
  #                       y=data_dfun_t[, dfun])
  #   # mdl_t_resp2 <- lsfit(x=data_dfun_t[, .(resp, resp_by_val2)],
  #   #                     y=data_dfun_t[, dfun])
  #   # mdl_t_resp3 <- lsfit(x=data_dfun_t[, .(resp, resp_by_val3)],
  #   #                      y=data_dfun_t[, dfun])
  #   mdl_t_rule <- lsfit(x=data_dfun_t[, .(rule, val, rule_by_val, conj)],
  #                       y=data_dfun_t[, dfun])
  #   # mdl_t_all <- lsfit(x=data_dfun_t[, .(stim, resp, rule, stim_by_val, resp_by_val, rule_by_val, conj)],
  #   #                    y=data_dfun_t[, dfun])
  #   # mdl_t_rand1 <- lsfit(x=data_dfun_t[, .(rand1, conj)],
  #   #                      y=data_dfun_t[, dfun])
  #   # mdl_t_rand2 <- lsfit(x=data_dfun_t[, .(rand2, conj)], # rand2
  #   #                       y=data_dfun_t[, dfun])
  #   # mdl_t_rand22 <- lsfit(x=data_dfun_t[, .(rand22, conj)], # rand2
  #   #                      y=data_dfun_t[, dfun])
  # 
  #   # add betas to data table
  #   rslts_t <- data.table(
  #     subID = sub,
  #     tpoint = t,
  #     time = ts[t+1],
  #     stim = mdl_t_stim$coefficients['stim'],
  #     stim_by_val = mdl_t_stim$coefficients['stim_by_val'],
  #     stim_val = mdl_t_stim$coefficients['val'],
  #     stim_conj = mdl_t_stim$coefficients['conj'],
  #     resp = mdl_t_resp$coefficients['resp'],
  #     resp_by_val = mdl_t_resp$coefficients['resp_by_val'],
  #     resp_val = mdl_t_resp$coefficients['val'],
  #     resp_conj = mdl_t_resp$coefficients['conj'],
  #     rule = mdl_t_rule$coefficients['rule'],
  #     rule_by_val = mdl_t_rule$coefficients['rule_by_val'],
  #     rule_val= mdl_t_rule$coefficients['val'],
  #     rule_conj = mdl_t_rule$coefficients['conj']
  #     # stim = mdl_t_all$coefficients['stim'],
  #     # stim_by_val = mdl_t_all$coefficients['stim_by_val'],
  #     # resp = mdl_t_all$coefficients['resp'],
  #     # resp_by_val = mdl_t_all$coefficients['resp_by_val'],
  #     # rule = mdl_t_all$coefficients['rule'],
  #     # rule_by_val = mdl_t_all$coefficients['rule_by_val'],
  #     # conj = mdl_t_all$coefficients['conj'],
  #     # rand1 = mdl_t_rand1$coefficients['rand1'],
  #     # conj1 = mdl_t_rand1$coefficients['conj'],
  #     # rand2 = mdl_t_rand2$coefficients['rand2'],
  #     # conj2= mdl_t_rand2$coefficients['conj'],
  #     # rand22 = mdl_t_rand22$coefficients['rand22'],
  #     # conj22= mdl_t_rand22$coefficients['conj']
  #     )
  # 
  #   # append data from current time point to overall data table
  #   rslts_cue_rc_int <- rbind(rslts_cue_rc_int, rslts_t)
  # }
}

# save results
write_csv(rslts_cue_rc_all, paste0(path_out, 'rslts_cue_rc_all.csv'))
write_csv(rslts_cue_rc_by_val_int, paste0(path_out, 'rslts_cue_rc_by_val_int.csv'))
# write_csv(rslts_cue_rc_int, paste0(path_out, 'rslts_cue_rc_int.csv'))
#write_csv(rslts_cue_ex_beh, paste0(path_out, 'rslts_cue_ex_beh.csv'))

## Fdbc-locked ------------------------------------------------------------------
n_tpoints = 282 # epochs are from -0.1 s to 1 s relative to cue onset
ts <- seq(-1, 0.1, length.out=n_tpoints) # put time samples into actual times

rslts_fdbc_ex_all <- data.table()
# rslts_fdbc_by_val <- data.table()
rslts_fdbc_ex_by_val_int <- data.table()
#rslts_fdbc_int <- data.table()
# rslts_fdbc_beh <- data.table()
for (sub in unique(data_dfun[, subID])) {
  
  # update progress 
  print(paste0('Analysing ', sub))
  
  # put data_dfun into long format
  data_dfun_sub <- data_dfun[subID == sub,]
  data_dfun_long <- transpose(data_dfun_sub[, dfun_110:dfun_810, ])
  data_dfun_long[, class := as.integer(paste0(seq(1, 8), '40'))]
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
 data_dfun_long[, ':=' (
                   stim = case_when(
                     unique(y) %in% c(140, 340, 540, 740) ~ mat_stim[, 1],
                     T ~ mat_stim[, 2]),
                   resp = case_when(
                     unique(y) %in% c(140, 440, 540, 840) ~ mat_resp[, 1],
                     T ~ mat_resp[, 2]),
                   rule = case_when(
                     unique(y) %in% c(140, 240, 540, 640) ~ mat_rule[, 1],
                     T ~ mat_rule[, 3]),
                   val = case_when(
                     unique(y) %in% c(140, 240, 340, 440) ~ mat_val[, 1],
                     T ~ mat_val[, 5]),
                   conj = case_when(
                     y == class ~ 1,
                     T ~ 0),
                   stim_zero = case_when(
                     unique(y) %in% c(140, 340, 540, 740) ~ mat_stim_zero[, 1],
                     T ~ mat_stim_zero[, 2]),
                   resp_zero = case_when(
                     unique(y) %in% c(140, 440, 540, 840) ~ mat_resp_zero[, 1],
                     T ~ mat_resp_zero[, 2]),
                   rule_zero = case_when(
                     unique(y) %in% c(140, 240, 540, 640) ~ mat_rule_zero[, 1],
                     T ~ mat_rule_zero[, 3]),
                   val_zero = case_when(
                     unique(y) %in% c(140, 240, 340, 440) ~ mat_val_zero[, 1],
                     T ~ mat_val_zero[, 5]),
                   conj_zero = case_when(
                     y == class ~ 0,
                     T ~ 1),
                   stim_by_val = case_when(
                     unique(y) %in% c(140, 340, 640, 840) ~ mat_stim_by_val[, 1],
                     T ~ mat_stim_by_val[, 2]),
                   resp_by_val = case_when(
                     unique(y) %in% c(140, 440, 640, 740) ~ mat_resp_by_val[, 1],
                     T ~ mat_resp_by_val[, 2]),
                   rule_by_val = case_when(
                     unique(y) %in% c(140, 240, 740, 840) ~ mat_rule_by_val[, 1],
                     T ~ mat_rule_by_val[, 3]),
                   stim_by_val_zero = case_when(
                     unique(y) %in% c(140, 340, 640, 840) ~ mat_stim_by_val_zero[, 1],
                     T ~ mat_stim_by_val_zero[, 2]),
                   resp_by_val_zero = case_when(
                     unique(y) %in% c(140, 440, 640, 740) ~ mat_resp_by_val_zero[, 1],
                     T ~ mat_resp_by_val_zero[, 2]),
                   rule_by_val_zero = case_when(
                     unique(y) %in% c(140, 240, 740, 840) ~ mat_rule_by_val_zero[, 1],
                     T ~ mat_rule_by_val_zero[, 3])
                 ),
                 by = trial_by_tpoint]

  # # remove conjunction rows
  # data_dfun_long_noconj <- data_dfun_long[conj != 1,]
  
  # put data_dfun_by_val into long format
  # data_dfun_by_val_sub <- data_dfun_by_val[subID == sub,]
  # data_dfun_by_val_long <- transpose(data_dfun_by_val_sub[, dfun_tl:dfun_ar, ])
  # data_dfun_by_val_long[, class := c('tl', 'tr', 'al', 'ar')]
  # data_dfun_by_val_long <- melt(data_dfun_by_val_long,
  #                               id.vars='class',
  #                               variable.name='trial_by_tpoint',
  #                               value.name='dfun')
  # data_dfun_by_val_ids = data.table( # create data table of ID variables
  #   y = rep(data_dfun_by_val_sub[, y], each=4),
  #   tpoint = rep(data_dfun_by_val_sub[, tpoint], each=4),
  #   subID = rep(data_dfun_by_val_sub[, subID], each=4),
  #   value = rep(data_dfun_by_val_sub[, value], each=4))
  # data_dfun_by_val_long <- cbind(data_dfun_by_val_ids,
  #                                data_dfun_by_val_long) # add ID variables back to data
  # data_dfun_by_val_long$class <- factor(data_dfun_by_val_long$class,
  #                                       levels = c("tl", "tr", "al", "ar"))
  # setorder(data_dfun_by_val_long, value, y, tpoint, trial_by_tpoint, class) # make sure things are ordered correctly
  # 
  # # add in RDM vectors for each trial type
  # data_dfun_by_val_long[, ':='
  #                (stim = case_when(
  #                  unique(y) %in% c(110, 310, 510, 710) ~ mat_stim_no_val[, 1],
  #                  T ~ mat_stim_no_val[, 2]),
  #                 resp = case_when(
  #                  unique(y) %in% c(110, 410, 510, 810) ~ mat_resp_no_val[, 1],
  #                  T ~ mat_resp_no_val[, 2]),
  #                 rule = case_when(
  #                  unique(y) %in% c(110, 210, 510, 610) ~ mat_rule_no_val[, 1],
  #                  T ~ mat_rule_no_val[, 3]),
  #                 conj = case_when(
  #                  (unique(y) %in% c(110, 510)) & (class == 'tl') ~ 1,
  #                  (unique(y) %in% c(210, 610)) & (class == 'tr') ~ 1,
  #                  (unique(y) %in% c(310, 710)) & (class == 'al') ~ 1,
  #                  (unique(y) %in% c(410, 810)) & (class == 'ar') ~ 1,
  #                  T ~ 0)),
  #                by = trial_by_tpoint]
  # 
  # # remove conjunction
  # data_dfun_by_val_long_noconj <- data_dfun_by_val_long[conj != 1,]
  
  ## Running analysis ## 
  
  # 1) Loop through time points and run analysis for all data conditions
  for (t in seq(0, n_tpoints - 1)) { # subtract 1 to account for Python indexing starting at 0

    # subset data per time point
    data_dfun_t <- data_dfun_long[tpoint == t, ] # data_dfun_long_noconj

    # compute model
    mdl_t <- lsfit(x=data_dfun_t[, .(rule, stim, resp, val, conj)], # note - removed conj
                   y=data_dfun_t[, dfun])
    
    mdl_t_zero <- lsfit(x=data_dfun_t[, .(rule_zero, stim_zero, resp_zero, val_zero, conj_zero)], # note - removed conj
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
      conj = mdl_t$coefficients['conj'],
      int_zero = mdl_t_zero$coefficients['Intercept'],
      rule_zero = mdl_t_zero$coefficients['rule_zero'],
      stim_zero = mdl_t_zero$coefficients['stim_zero'],
      resp_zero = mdl_t_zero$coefficients['resp_zero'],
      val_zero = mdl_t_zero$coefficients['val_zero'],
      conj_zero = mdl_t_zero$coefficients['conj_zero']
    )
    
    # append data from current time point to overall data table
    rslts_fdbc_ex_all <- rbind(rslts_fdbc_ex_all, rslts_t)
  }
  
  # 2) Run analysis separately for high and low value trials
  # for (val in c('hi', 'lo')) { ################################################# Also try running this with value as a predictor
  # 
  #   # subset trials by value
  #   data_dfun_val <- data_dfun_by_val_long[value==val, ] #  data_dfun_by_val_long_noconj
  # 
  #   # loop through times
  #   for (t in seq(0, n_tpoints - 1)) {
  # 
  #     # subset data per time point
  #     data_dfun_val_t <- data_dfun_val[tpoint==t, ]
  # 
  #     # compute model
  #     mdl_t <- lsfit(x=data_dfun_val_t[, .(stim, resp, rule, conj)], # conj
  #                    y=data_dfun_val_t[, dfun])
  #     
  #     d <- data_dfun_val_t[1:40, rule:conj]
  #     #d$trial <- rep(seq(1:5), each=4)
  #     d$y <- seq(1:40)
  #     d_melt <- melt(d, 
  #                    id.vars='y', 
  #                    measure.vars = c('rule', 'conj'))
  #     d_melt$value <- as.factor(d_melt$value)
  #     
  #     
  #     ggplot() + 
  #       geom_tile(data=d_melt, 
  #                 aes(x=variable, 
  #                     y=-y,
  #                     fill=value),
  #                 color='grey') + 
  #       scale_fill_manual(values=c('white', 'black')) + 
  #       theme_minimal() + theme(axis.text.x = element_text(size=14))
  #     
  # 
  #     mdl_t_stim <- lsfit(x=data_dfun_val_t[, .(stim, conj)],
  #                         y=data_dfun_val_t[, dfun])
  #     mdl_t_resp <- lsfit(x=data_dfun_val_t[, .(resp, conj)],
  #                         y=data_dfun_val_t[, dfun])
  #     mdl_t_rule <- lsfit(x=data_dfun_val_t[, .(rule, conj)],
  #                         y=data_dfun_val_t[, dfun])
  # 
  #     # add betas to data table
  #     rslts_t <- data.table(
  #       subID = sub,
  #       value = val,
  #       tpoint=t,
  #       time = ts[t+1],
  #       # int = mdl_t$coefficients['Intercept'],
  #       # stim = mdl_t$coefficients['stim'],
  #       # resp = mdl_t$coefficients['resp'],
  #       # rule = mdl_t$coefficients['rule']
  #       #conj = mdl_t$coefficients['conj']
  #       stim = mdl_t_stim$coefficients['stim'],
  #       stim_conj = mdl_t_stim$coefficients['conj'],
  #       resp = mdl_t_resp$coefficients['resp'],
  #       resp_conj = mdl_t_resp$coefficients['conj'],
  #       rule = mdl_t_rule$coefficients['rule'],
  #       rule_conj = mdl_t_rule$coefficients['conj']
  #       )
  # 
  #     # append data from current time point to overall data table
  #     rslts_by_val <- rbind(rslts_by_val, rslts_t)
  #   }
  # }
  
  # 2.5) Run analysis with interaction with value
  for (t in seq(0, n_tpoints - 1)) {

    # subset data per time point
    data_dfun_val_t <- data_dfun_long[tpoint==t, ]
    
    
    # compute model
    mdl_rule_t <- lsfit(x=data_dfun_val_t[, .(rule, stim, resp, val, conj, rule_by_val)],
                        y=data_dfun_val_t[, dfun])
    mdl_stim_t <- lsfit(x=data_dfun_val_t[, .(rule, stim, resp, val, conj, stim_by_val)],
                        y=data_dfun_val_t[, dfun])
    mdl_resp_t <- lsfit(x=data_dfun_val_t[, .(rule, stim, resp, val, conj, resp_by_val)],
                        y=data_dfun_val_t[, dfun])
    
    mdl_rule_t_zero <- lsfit(x=data_dfun_val_t[, .(rule_zero, stim_zero, resp_zero, val_zero, conj_zero, rule_by_val_zero)],
                             y=data_dfun_val_t[, dfun])
    mdl_stim_t_zero <- lsfit(x=data_dfun_val_t[, .(rule_zero, stim_zero, resp_zero, val_zero, conj_zero, stim_by_val_zero)],
                             y=data_dfun_val_t[, dfun])
    mdl_resp_t_zero <- lsfit(x=data_dfun_val_t[, .(rule_zero, stim_zero, resp_zero, val_zero, conj_zero, resp_by_val_zero)],
                             y=data_dfun_val_t[, dfun])
    
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
      resp_by_val = mdl_resp_t$coefficients['resp_by_val'],
      # rule zero
      rule_int_zero = mdl_rule_t_zero$coefficients['Intercept'],
      rule_rule_zero = mdl_rule_t_zero$coefficients['rule_zero'],
      rule_stim_zero = mdl_rule_t_zero$coefficients['stim_zero'],
      rule_resp_zero = mdl_rule_t_zero$coefficients['resp_zero'],
      rule_val_zero= mdl_rule_t_zero$coefficients['val_zero'],
      rule_conj_zero = mdl_rule_t_zero$coefficients['conj_zero'],
      rule_by_val_zero = mdl_rule_t_zero$coefficients['rule_by_val_zero'],
      # stim zero
      stim_int_zero = mdl_stim_t_zero$coefficients['Intercept'],
      stim_rule_zero = mdl_stim_t_zero$coefficients['rule_zero'],
      stim_stim_zero = mdl_stim_t_zero$coefficients['stim_zero'],
      stim_resp_zero = mdl_stim_t_zero$coefficients['resp_zero'],
      stim_val_zero = mdl_stim_t_zero$coefficients['val_zero'],
      stim_conj_zero = mdl_stim_t_zero$coefficients['conj_zero'],
      stim_by_val_zero = mdl_stim_t_zero$coefficients['stim_by_val_zero'],
      # resp zero
      resp_int_zero = mdl_resp_t_zero$coefficients['Intercept'],
      resp_rule_zero = mdl_resp_t_zero$coefficients['rule_zero'],
      resp_stim_zero = mdl_resp_t_zero$coefficients['stim_zero'],
      resp_resp_zero = mdl_resp_t_zero$coefficients['resp_zero'],
      resp_val_zero = mdl_resp_t_zero$coefficients['val_zero'],
      resp_conj_zero = mdl_resp_t_zero$coefficients['conj_zero'],
      resp_by_val_zero = mdl_resp_t_zero$coefficients['resp_by_val_zero']
    )

    # append data from current time point to overall data table
    rslts_fdbc_ex_by_val_int <- rbind(rslts_fdbc_ex_by_val_int, rslts_t)
  }
  
  # 3) Run analysis for alternate rule and response RDMs
  # for (t in seq(0, n_tpoints - 1)) { # subtract 1 to account for Python indexing starting at 0
  # 
  #   # subset data per time point
  #   data_dfun_t <- data_dfun_long[tpoint == t, ]
  # 
  #   # compute models
  #   mdl_t_stim <- lsfit(x=data_dfun_t[, .(stim, stim_by_val, conj)],
  #                       y=data_dfun_t[, dfun])
  #   # mdl_t_stim2 <- lsfit(x=data_dfun_t[, .(stim, stim_by_val, stim_by_val_resp, conj)], ## 22/10/25 add this next time
  #   #                      y=data_dfun_t[, dfun])
  # 
  #   mdl_t_resp <- lsfit(x=data_dfun_t[, .(resp, resp_by_val, conj)],
  #                       y=data_dfun_t[, dfun])
  #   # mdl_t_resp2 <- lsfit(x=data_dfun_t[, .(resp, resp_by_val2)],
  #   #                     y=data_dfun_t[, dfun])
  #   # mdl_t_resp3 <- lsfit(x=data_dfun_t[, .(resp, resp_by_val3)],
  #   #                      y=data_dfun_t[, dfun])
  #   mdl_t_rule <- lsfit(x=data_dfun_t[, .(rule, rule_by_val, conj)],
  #                       y=data_dfun_t[, dfun])
  #   # mdl_t_all <- lsfit(x=data_dfun_t[, .(stim, resp, rule, stim_by_val, resp_by_val, rule_by_val, conj)],
  #   #                    y=data_dfun_t[, dfun])
  #   # mdl_t_rand1 <- lsfit(x=data_dfun_t[, .(rand1, conj)],
  #   #                      y=data_dfun_t[, dfun])
  #   # mdl_t_rand2 <- lsfit(x=data_dfun_t[, .(rand2, conj)], # rand2
  #   #                       y=data_dfun_t[, dfun])
  #   # mdl_t_rand22 <- lsfit(x=data_dfun_t[, .(rand22, conj)], # rand2
  #   #                      y=data_dfun_t[, dfun])
  # 
  #   # add betas to data table
  #   rslts_t <- data.table(
  #     subID = sub,
  #     tpoint = t,
  #     time = ts[t+1],
  #     stim = mdl_t_stim$coefficients['stim'],
  #     stim_by_val = mdl_t_stim$coefficients['stim_by_val'],
  #     stim_conj = mdl_t_stim$coefficients['conj'],
  #     resp = mdl_t_resp$coefficients['resp'],
  #     resp_by_val = mdl_t_resp$coefficients['resp_by_val'],
  #     resp_conj = mdl_t_resp$coefficients['conj'],
  #     # resp2 = mdl_t_resp2$coefficients['resp'],
  #     # resp_by_val2 = mdl_t_resp2$coefficients['resp_by_val2'],
  #     # resp3 = mdl_t_resp3$coefficients['resp'],
  #     # resp_by_val3 = mdl_t_resp3$coefficients['resp_by_val3'],
  #     rule = mdl_t_rule$coefficients['rule'],
  #     rule_by_val = mdl_t_rule$coefficients['rule_by_val'],
  #     rule_conj = mdl_t_rule$coefficients['conj']
  #     # stim = mdl_t_all$coefficients['stim'],
  #     # stim_by_val = mdl_t_all$coefficients['stim_by_val'],
  #     # resp = mdl_t_all$coefficients['resp'],
  #     # resp_by_val = mdl_t_all$coefficients['resp_by_val'],
  #     # rule = mdl_t_all$coefficients['rule'],
  #     # rule_by_val = mdl_t_all$coefficients['rule_by_val'],
  #     # conj = mdl_t_all$coefficients['conj'],
  #     # rand1 = mdl_t_rand1$coefficients['rand1'],
  #     # conj1 = mdl_t_rand1$coefficients['conj'],
  #     # rand2 = mdl_t_rand2$coefficients['rand2'],
  #     # conj2= mdl_t_rand2$coefficients['conj'],
  #     # rand22 = mdl_t_rand22$coefficients['rand22'],
  #     # conj22= mdl_t_rand22$coefficients['conj']
  #     )
  # 
  #   # append data from current time point to overall data table
  #   rslts_fdbc_rc_int <- rbind(rslts_fdbc_rc_int, rslts_t)
  # }
}

# save results
write_csv(rslts_fdbc_ex_all, paste0(path_out, 'rslts_fdbc_ex_all.csv'))
# write_csv(rslts_fdbc_by_val_int, paste0(path_out, 'rslts_fdbc_by_val_int.csv'))
write_csv(rslts_fdbc_ex_by_val_int, paste0(path_out, 'rslts_fdbc_ex_by_val_int.csv'))
# write_csv(rslts_fdbc_beh, paste0(path_out, 'rslts_fdbc_beh.csv'))
