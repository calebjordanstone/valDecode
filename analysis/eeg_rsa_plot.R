## Notes: This script plots the results of the main effect RSA analysis, 
## produced by the script "eeg_rsa_run.R", found at https://github.com/calebjordanstone/valDecode/tree/main/analysis 
## Input data can be found in "rsa_results_files.zip".
## This scipt also uses cleaned behavioural data files to find mean RTs for each phase. Cleaned
## behavioural data files can be founr in "cleaned_beh_files.zip" at https://osf.io/2x3a8/files/osfstorage

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
source(file.path("my_theme.R"))

# Set data paths
# path_exp <- ""
# path_out <- ""
# path_data <- ""

# write function to extract p_vals
extract_t_1samp <- function(x) {
  res <- t.test(x, mu=0)
  p_val <- res$p.value
  return(p_val)
}

# set come constants 
n_tpoints = 282 # epochs are from -0.1 s to 1 s relative to cue onset
ts <- seq(-0.1, 1, length.out=n_tpoints)
exclude <- c('sub-12') 

## load EEG data
rslts_cue_rc_all <- fread(paste0(path_out, 'rslts_cue_rc.csv'))
rslts_cue_rc_by_val_int <- fread(paste0(path_out, 'rslts_cue_rc_by_val_int.csv'))
rslts_cue_ex_all <- fread(paste0(path_out, 'rslts_cue_ex.csv'))
rslts_cue_ex_by_val_int <- fread(paste0(path_out, 'rslts_cue_ex_by_val_int.csv'))

# load behavioural data
files_beh_rc <- list.files(path_out, pattern = 'beh_rc_sub', recursive = F)
files_beh_ex <- list.files(path_out, pattern = 'beh_ex_sub', recursive = F)
dt_rc <- rbindlist(lapply(file.path(path_out, files_beh_rc), fread))
dt_rc <- dt_rc[!(Subject %in% exclude), ]
dt_ex <- rbindlist(lapply(file.path(path_out, files_beh_ex), fread))
dt_ex <- dt_ex[!(Subject %in% exclude), ]

# find mean RT per phase
gavRT_rc <- dt_rc[, .(avRT = mean(RT)), by=Subject][, .(gavRT=mean(avRT))]
gavRT_ex <- dt_ex[, .(avRT = mean(RT)), by=Subject][, .(gavRT=mean(avRT))]

## Plot reward phase -----------------------------------------------------------
# apply t-test function
rslts_cue_rc_all_pval <- rslts_cue_rc_all[!(subID %in% exclude),
                                        lapply(.SD, extract_t_1samp), 
                                        .SDcols=int:conj,
                                        by=c('tpoint', 'time')
][, 
  melt(.SD, id.vars=c('tpoint', 'time'),
       variable.name='effect',
       value.name='pval'),
  .SDcols=tpoint:conj,
]

# adjust p-values
rslts_cue_rc_all_pval <- rslts_cue_rc_all_pval[effect %in% c('rule', 'stim', 'resp', 'val', 'conj'),
                                               .(pval=p.adjust(pval, method = 'fdr')), 
                                               by=effect]
# add column for time in ms
rslts_cue_rc_all_pval$time <- rep(ts, times=5)

# add significance lines for plotting
rslts_cue_rc_all_pval[, sigLine := case_when(
  effect == 'rule' & pval < .05 ~ -0.10,
  effect == 'stim' & pval < .05 ~ -0.12,
  effect == 'resp' & pval < .05 ~ -0.14,
  effect == 'val' & pval < .05 ~ -0.16,
  effect == 'conj' & pval < .05 ~ -0.18
)]
rslts_cue_rc_all_pval <- dcast(rslts_cue_rc_all_pval, time ~ effect, value.var = 'sigLine')

# average over subjects
rslts_cue_rc_all_p <- rslts_cue_rc_all[!(subID %in% exclude), 
                                       lapply(.SD, mean), by=time, 
                                       .SDcols=c('int', 'stim', 'resp', 'rule', 'val', 'conj')] 
# add significance lines to plotting data.table
rslts_cue_rc_all_p <- rslts_cue_rc_all_p[rslts_cue_rc_all_pval, 
                                         on='time']
# create plot 
plot_cue_rc <- ggplot(rslts_cue_rc_all_p, aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.15, 0.15)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-0.15, 0.15)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-0.1, 1), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  # add mean RT data lines
  geom_point(data=data.frame(x=0.909779, y=0), # 0.909 = gavRT_rc + 0.5 s
            aes(x, y),
            fill='white',
            alpha=1,
            size=3,
            stroke=1.25,
            shape=23,
            color='black') +
  # add data
  geom_line(aes(y=stim, color='stimulus'), linewidth=1) +
  geom_line(aes(y=resp, color='response'), linewidth=1) +
  geom_line(aes(y=rule, color='rule'), linewidth=1) +
  geom_line(aes(y=val, color='value'), linewidth=1) + 
  
  # add significance lines
  geom_line(aes(y=i.stim, color='stimulus'), linewidth=1) +
  geom_line(aes(y=i.resp, color='response'), linewidth=1) +
  geom_line(aes(y=i.rule, color='rule'), linewidth=1) +
  geom_line(aes(y=i.val, color='value'), linewidth=1) +
  
  # customise
  scale_y_continuous(name='Beta',
                     breaks=c(-0.2, 0, 0.2),
                     labels=c('-0.2', '0.0', '0.2'),
                     expand=expansion(mult = 0.1)) +
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  scale_color_manual(name='',
                     breaks=c('rule', 'stimulus', 'response', 'value', 'conjunction', 'int'),
                     values=c('rule' = '#9467bd',
                              'stimulus' = '#17becf',
                              'response' = '#e377c2',
                              'value' = '#ff7f0e',
                              'conjunction'  = 'black',
                              'int' = 'green'),
                     labels=c('rule', 'stimulus', 'response', 'value', 'conjunction', 'int')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-0.2, 0.2)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')

# save plot
svg(paste0(path_out, 'fig_cue_rc.svg'),
    width=8, height=4)
plot(plot_cue_rc)
dev.off()

## Plot extinction phase -------------------------------------------------------
# apply t-test function
rslts_cue_ex_all_pval <- rslts_cue_ex_all[!(subID %in% exclude),
                                          lapply(.SD, extract_t_1samp), 
                                          .SDcols=int:conj, 
                                          by=c('tpoint', 'time')
][, 
  melt(.SD, id.vars=c('tpoint', 'time'),
       variable.name='effect',
       value.name='pval'),
  .SDcols=tpoint:conj, #rt
]
# correct p-values
rslts_cue_ex_all_pval <- rslts_cue_ex_all_pval[effect %in% c('rule', 'stim', 'resp', 'val', 'conj'), 
                                               .(pval=p.adjust(pval, method = 'fdr')), 
                                               by=effect]

# add column for time in ms
rslts_cue_ex_all_pval$time <- rep(ts, times=5)

# add significance lines for plotting
rslts_cue_ex_all_pval[, sigLine := case_when(
  effect == 'rule' & pval < .05 ~ -0.10,
  effect == 'stim' & pval < .05 ~ -0.12,
  effect == 'resp' & pval < .05 ~ -0.14,
  effect == 'val' & pval < .05 ~ -0.16,
  effect == 'conj' & pval < .05 ~ -0.18 #rt
)]
rslts_cue_ex_all_pval <- dcast(rslts_cue_ex_all_pval, time ~ effect, value.var = 'sigLine')

# average over subjects
rslts_cue_ex_all_p <- rslts_cue_ex_all[!(subID %in% exclude), 
                                       lapply(.SD, mean), by=time, 
                                       .SDcols=c('int', 'stim', 'resp', 'rule', 
                                                 'val', 'conj')]
# add significance lines to plotting data.table
rslts_cue_ex_all_p <- rslts_cue_ex_all_p[rslts_cue_ex_all_pval, 
                                         on='time']

# create plot
plot_cue_ex <- ggplot(rslts_cue_ex_all_p, aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.15, 0.15)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-0.15, 0.15)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-0.1, 1), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add mean RT data lines
  geom_point(data=data.frame(x=0.8980177, y=0), # 0.898 = gavRT_ex + 0.5 s
             aes(x, y),
             fill='white',
             alpha=1,
             size=3,
             stroke=1.25,
             shape=23,
             color='black') +
  
  # add data
  geom_line(aes(y=stim, color='stimulus'), linewidth=1) +
  geom_line(aes(y=resp, color='response'), linewidth=1) +
  geom_line(aes(y=rule, color='rule'), linewidth=1) +
  geom_line(aes(y=val, color='value'), linewidth=1) + 
  
  # add significance lines
  geom_line(aes(y=i.stim, color='stimulus'), linewidth=1) +
  geom_line(aes(y=i.resp, color='response'), linewidth=1) +
  geom_line(aes(y=i.rule, color='rule'), linewidth=1) +
  geom_line(aes(y=i.val, color='value'), linewidth=1) +

  # customise
  scale_y_continuous(name='Beta',
                     breaks=c(-0.2, 0, 0.2),
                     labels=c('-0.2', '0.0', '0.2'),
                     expand=expansion(mult = 0.1)) +
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  scale_color_manual(name='',
                     breaks=c('rule', 'stimulus', 'response', 'value', 'conjunction'),
                     values=c('rule' = '#9467bd',
                              'stimulus' = '#17becf',
                              'response' = '#e377c2',
                              'value' = '#ff7f0e',
                              'conjunction'  = 'black'),
                     labels=c('rule', 'stimulus', 'response', 'value', 'conjunction')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-0.2, 0.2)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')

# save plot
svg(paste0(path_out, 'fig_cue_ex.svg'),
    width=8, height=4)
plot(plot_cue_ex)
dev.off()
