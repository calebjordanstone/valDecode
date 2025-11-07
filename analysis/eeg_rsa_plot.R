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

# set data paths
path_exp <- "C:/Users/cstone/OneDrive - UNSW/Documents/Projects/my_experiments/val_decode/"
path_out <- paste0(path_exp, 'output/')
path_data <- paste0(path_exp, 'data/')

# write function to extract BFs
extract_bf_1samp <- function(x) {
  res <- ttestBF(x, mu=0)
  bf <- log10(as.numeric(as.vector(res))[1])
  return(bf)
}

# vmac_subs <- c('sub-01', 'sub-04', 'sub-05', 'sub-06', 
#                'sub-09', 'sub-10', 'sub-13', 'sub-20', 
#                'sub-21', 'sub-28')

#### Cue Locked Epochs #### ====================================================

## load data
rslts_cue_rc_all <- fread(paste0(path_out, 'rslts_cue_rc_all.csv'))
rslts_cue_rc_by_val_int <- fread(paste0(path_out, 'rslts_cue_rc_by_val_int.csv'))
#rslts_cue_rc_int <- fread(paste0(path_out, 'rslts_cue_rc_int.csv'))
rslts_cue_ex_all <- fread(paste0(path_out, 'rslts_cue_ex_all.csv'))
rslts_cue_ex_by_val_int <- fread(paste0(path_out, 'rslts_cue_ex_by_val_int.csv'))


# filter data to include only those who show VMAC effect
# rslts_cue_rc_all <- rslts_cue_rc_all[subID %in% vmac_subs,]

## Plot cue_rc_all -------------------------------------------------------------
# apply BF function
rslts_cue_rc_all_bf <- rslts_cue_rc_all[, 
                          lapply(.SD, extract_bf_1samp), 
                          .SDcols=int:conj_zero, 
                          by=c('tpoint', 'time')
][, 
  melt(.SD, id.vars=c('tpoint', 'time'),
       variable.name='effect',
       value.name='BF'),
  .SDcols=tpoint:conj_zero,
][, ':=' (BF_strong = ifelse(BF > 1, BF, NA),
          BF_subs = ifelse(BF > 0.5, BF, NA))]
rslts_cue_rc_all_bf$effect <- factor(rslts_cue_rc_all_bf$effect, 
                              levels = c('int', 'rule', 'stim', 'resp', 'val', 'conj',
                                         'int_zero', 'rule_zero', 'stim_zero', 'resp_zero', 'val_zero', 'conj_zero'))

# average over subjects
rslts_cue_rc_all_p <- rslts_cue_rc_all[, lapply(.SD, mean), by=time, 
                         .SDcols=c('int', 'stim', 'resp', 'rule', 'val', 'conj',
                                   'int_zero', 'rule_zero', 'stim_zero', 'resp_zero', 'val_zero', 'conj_zero')] 

plot_cue_rc_all <- ggplot(rslts_cue_rc_all_p, aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.15, 0.15)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-0.15, 0.15)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  # geom_line(aes(y=int_zero, color='int'), linewidth=1) +
  # geom_line(aes(y=conj_zero, color='conjunction'), linewidth=1) +
  geom_line(aes(y=stim_zero, color='stimulus'), linewidth=1) +
  geom_line(aes(y=resp_zero, color='response'), linewidth=1) +
  geom_line(aes(y=rule_zero, color='rule'), linewidth=1) +
  geom_line(aes(y=val_zero, color='value'), linewidth=1) + 
  
  # customise
  scale_y_continuous(name='Beta',
                     breaks=c(-0.2, 0, 0.2),
                     labels=c('-0.2', '0.0', '0.2'),
                     # limits=c(0.375, 1),
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

plot_cue_rc_all_bfs <- ggplot(rslts_cue_rc_all_bf[effect %in% c('rule_zero', 'stim_zero', 'resp_zero', 'val_zero')], 
                              aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong, color=effect), linewidth=1) +
  facet_grid(rows='effect') + 
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     #expand=expansion(mult=.1)) + 
                     limits=c(-5, 10)) +  
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  scale_color_manual(name='',
                     breaks=c('rule_zero', 'stim_zero', 'resp_zero', 'val_zero'),
                     values=c('rule_zero' = '#9467bd',
                              'stim_zero' = '#17becf',
                              'resp_zero' = '#e377c2',
                              'val_zero' = '#ff7f0e'
                              #'conj'  = 'black'
                              ),
                     labels=c('rule', 'stim', 'resp', 'val')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme() 

fig_cue_rc_all <- plot_grid(plot_cue_rc_all, 
                            plot_cue_rc_all_bfs, 
                            ncol=1, nrow=2,
                            axis='tlbr',
                            rel_heights=c(1, 1),
                            align="v")

svg(paste0(path_out, 'fig_cue_rc_all_zeroIntercept.svg'),
    width=8, height=8)
plot(fig_cue_rc_all)
dev.off()

## Plot cue_ex_all -------------------------------------------------------------
# apply BF function
rslts_cue_ex_all_bf <- rslts_cue_ex_all[, 
                          lapply(.SD, extract_bf_1samp), 
                          .SDcols=int:conj_zero,
                          by=c('tpoint', 'time')
][, 
  melt(.SD, id.vars=c('tpoint', 'time'),
       variable.name='effect',
       value.name='BF'),
  .SDcols=tpoint:conj_zero
][, ':=' (BF_strong = ifelse(BF > 1, BF, NA),
          BF_subs = ifelse(BF > 0.5, BF, NA))]
rslts_cue_ex_all_bf$effect <- factor(rslts_cue_ex_all_bf$effect, 
                              levels = c('rule', 'stim', 'resp', 'val', 'conj',
                                         'rule_zero', 'stim_zero', 'resp_zero', 'val_zero', 'conj_zero'))

# average over subjects
rslts_cue_ex_all_p <- rslts_cue_ex_all[, lapply(.SD, mean), by=time, 
                         .SDcols=c('stim', 'resp', 'rule', 'val', 'conj',
                                   'stim_zero', 'resp_zero', 'rule_zero', 'val_zero', 'conj_zero')] 

plot_cue_ex_all <- ggplot(rslts_cue_ex_all_p, aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.15, 0.15)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-0.15, 0.15)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  # geom_line(aes(y=conj, color='conjunction'), linewidth=1) +
  geom_line(aes(y=stim_zero, color='stimulus'), linewidth=1) +
  geom_line(aes(y=resp_zero, color='response'), linewidth=1) +
  geom_line(aes(y=rule_zero, color='rule'), linewidth=1) +
  geom_line(aes(y=val_zero, color='value'), linewidth=1) + 
  
  # customise
  scale_y_continuous(name='Beta',
                     breaks=c(-0.2, 0, 0.2),
                     labels=c('-0.2', '0.0', '0.2'),
                     # limits=c(0.375, 1),
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

plot_cue_ex_all_bfs <- ggplot(rslts_cue_ex_all_bf[effect %in% c('rule_zero', 'stim_zero', 'resp_zero', 'val_zero')], aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong, color=effect), linewidth=1) +
  facet_grid(rows='effect') + 
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     # expand=expansion(mult=.1)) + 
                     limits=c(-5, 10)) +  
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  scale_color_manual(name='',
                     breaks=c('rule_zero', 'stim_zero', 'resp_zero', 'val_zero', 'conj_zero'),
                     values=c('rule_zero' = '#9467bd',
                              'stim_zero' = '#17becf',
                              'resp_zero' = '#e377c2',
                              'val_zero' = '#ff7f0e',
                              'conj_zero'  = 'black'),
                     labels=c('rule', 'stim', 'resp', 'val', 'conj')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme() 


fig_cue_ex_all <- plot_grid(plot_cue_ex_all, 
                            plot_cue_ex_all_bfs, 
                     ncol=1, nrow=2,
                     axis='tlbr',
                     rel_heights=c(1, 1),
                     align="v")

svg(paste0(path_out, 'fig_cue_ex_all_zeroIntercept.svg'),
    width=8, height=10)
plot(fig_cue_ex_all)
dev.off()


## Plot cue_rc_by_val_int ------------------------------------------------------
# apply BF function to rslts_by_val_int
rslts_cue_rc_by_val_int_bf <- rslts_cue_rc_by_val_int[, 
                                        lapply(.SD, extract_bf_1samp), 
                                        .SDcols=rule_int:resp_by_val_zero,
                                        by=c('tpoint', 'time')
][, 
  melt(.SD, id.vars=c('tpoint', 'time'),
       variable.name='effect',
       value.name='BF'),
  .SDcols=tpoint:resp_by_val_zero
][, ':=' (BF_strong = ifelse(BF > 1, BF, NA),
          BF_subs = ifelse(BF > 0.5, BF, NA))]

# average across subjects
rslts_cue_rc_by_val_int_p <- rslts_cue_rc_by_val_int[, 
                                       lapply(.SD, mean), by=c('time'), 
                                       .SDcols=c('rule_int', 'rule_rule', 'rule_stim', 'rule_resp', 'rule_val', 'rule_conj', 'rule_by_val',
                                                 'stim_int', 'stim_rule', 'stim_stim', 'stim_resp', 'stim_val', 'stim_conj', 'stim_by_val',
                                                 'resp_int', 'resp_rule', 'resp_stim', 'resp_resp', 'resp_val', 'resp_conj', 'resp_by_val',
                                                 'rule_int_zero', 'rule_rule_zero', 'rule_stim_zero', 'rule_resp_zero', 'rule_val_zero', 'rule_conj_zero', 'rule_by_val_zero',
                                                 'stim_int_zero', 'stim_rule_zero', 'stim_stim_zero', 'stim_resp_zero', 'stim_val_zero', 'stim_conj_zero', 'stim_by_val_zero',
                                                 'resp_int_zero', 'resp_rule_zero', 'resp_stim_zero', 'resp_resp_zero', 'resp_val_zero', 'resp_conj_zero', 'resp_by_val_zero'
                                                 )]

## rule, stim, resp x val interaction
plot_cue_rc_by_val_int <- ggplot() + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.15, 0.15)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-0.15, 0.15)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  # geom_line(data=rslts_cue_rc_by_val_int_p,
  #           aes(x=time, y=rule_int), color='grey',
  #           linewidth=1) +
  # geom_line(data=rslts_cue_rc_by_val_int_p,
  #           aes(x=time, y=rule_rule), color='#9467bd',
  #           linewidth=1,
  #           linetype=1) +
  # geom_line(data=rslts_cue_rc_by_val_int_p,
  #           aes(x=time, y=rule_stim), color='#17becf',
  #           linewidth=1,
  #           linetype=1) +
  # geom_line(data=rslts_cue_rc_by_val_int_p,
  #           aes(x=time, y=rule_resp), color='#e377c2',
  #           linewidth=1,
  #           linetype=1) +
  # geom_line(data=rslts_cue_rc_by_val_int_p,
  #           aes(x=time, y=rule_val), color='#ff7f0e',
  #           linewidth=1,
  #           linetype=1) +
  # geom_line(data=rslts_cue_rc_by_val_int_p,
  #           aes(x=time, y=rule_conj), color='black',
  #           linewidth=1,
  #           linetype=1) +
  geom_line(data=rslts_cue_rc_by_val_int_p,
            aes(x=time, y=rule_by_val_zero), color='#9467bd',
            linewidth=1,
            linetype=1) +
  geom_line(data=rslts_cue_rc_by_val_int_p,
            aes(x=time, y=stim_by_val_zero), color='#17becf',
            linewidth=1,
            linetype=1) +
  geom_line(data=rslts_cue_rc_by_val_int_p,
            aes(x=time, y=resp_by_val_zero), color='#e377c2',
            linewidth=1,
            linetype=1) +


  # customise
  scale_y_continuous(name='Beta',
                     breaks=c(-0.2, 0, 0.2),
                     labels=c('-0.2', '0.0', '0.2'),
                     # limits=c(0.375, 1),
                     expand=expansion(mult = 0.1)) +
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-0.2, 0.2)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')

plot_cue_rc_by_val_int_bf <- ggplot(rslts_cue_rc_by_val_int_bf[effect %in% c('rule_by_val_zero', 'stim_by_val_zero', 'resp_by_val_zero')], 
                              aes(x=time), linewidth=1) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong, color=effect), linewidth=1) +
  facet_grid(rows='effect') + 
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     # expand=expansion(mult=.1)) + 
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  scale_color_manual(name='',
                     breaks=c('rule_by_val_zero', 'stim_by_val_zero', 'resp_by_val_zero'),
                     values=c('rule_by_val_zero' = '#9467bd',
                              'stim_by_val_zero' = '#17becf',
                              'resp_by_val_zero' = '#e377c2')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme() 



fig_cue_rc_by_val_int <- plot_grid(plot_cue_rc_by_val_int, 
                                   plot_cue_rc_by_val_int_bf,
                                   ncol=1, nrow=2,
                                   axis='tlbr',
                                   rel_heights=c(1, 1),
                                   align="v")

svg(paste0(path_out, 'fig_cue_rc_by_val_int_zeroIntercept.svg'),
    width=8, height=8)
plot(fig_cue_rc_by_val_int)
dev.off()

## Plot cue_ex_by_val_int ------------------------------------------------------
# apply BF function to rslts_by_val_int
rslts_cue_ex_by_val_int_bf <- rslts_cue_ex_by_val_int[, 
                                        lapply(.SD, extract_bf_1samp), 
                                        .SDcols=rule_int:resp_by_val_zero,
                                        by=c('tpoint', 'time')
][, 
  melt(.SD, id.vars=c('tpoint', 'time'),
       variable.name='effect',
       value.name='BF'),
  .SDcols=tpoint:resp_by_val_zero
][, ':=' (BF_strong = ifelse(BF > 1, BF, NA),
          BF_subs = ifelse(BF > 0.5, BF, NA))]

# average across subjects
rslts_cue_ex_by_val_int_p <- rslts_cue_ex_by_val_int[, 
                                       lapply(.SD, mean), by=c('time'), 
                                       .SDcols=c('rule_int', 'rule_rule', 'rule_stim', 'rule_resp', 'rule_val', 'rule_conj', 'rule_by_val',
                                                 'stim_int', 'stim_rule', 'stim_stim', 'stim_resp', 'stim_val', 'stim_conj', 'stim_by_val',
                                                 'resp_int', 'resp_rule', 'resp_stim', 'resp_resp', 'resp_val', 'resp_conj', 'resp_by_val',
                                                 'rule_int_zero', 'rule_rule_zero', 'rule_stim_zero', 'rule_resp_zero', 'rule_val_zero', 'rule_conj_zero', 'rule_by_val_zero',
                                                 'stim_int_zero', 'stim_rule_zero', 'stim_stim_zero', 'stim_resp_zero', 'stim_val_zero', 'stim_conj_zero', 'stim_by_val_zero',
                                                 'resp_int_zero', 'resp_rule_zero', 'resp_stim_zero', 'resp_resp_zero', 'resp_val_zero', 'resp_conj_zero', 'resp_by_val_zero'
                                       )]

plot_cue_ex_by_val_int<- ggplot() + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.15, 0.15)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-0.15, 0.15)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  geom_line(data=rslts_cue_ex_by_val_int_p,
            aes(x=time, y=rule_by_val_zero), color='#9467bd',
            linewidth=1,
            linetype=1) +
  geom_line(data=rslts_cue_ex_by_val_int_p,
            aes(x=time, y=stim_by_val_zero), color='#17becf',
            linewidth=1,
            linetype=1) +
  geom_line(data=rslts_cue_ex_by_val_int_p,
            aes(x=time, y=resp_by_val_zero), color='#e377c2',
            linewidth=1,
            linetype=1) +
  
  # customise
  scale_y_continuous(name='Beta', 
                     breaks=c(-0.2, 0, 0.2),
                     labels=c('0.0', '0.25', '0.50'),
                     expand=expansion(mult = 0.1)) + 
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-0.2, 0.2)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')

plot_cue_ex_by_val_int_bf <- ggplot(rslts_cue_ex_by_val_int_bf[effect %in% c('rule_by_val_zero', 'stim_by_val_zero', 'resp_by_val_zero')],
                       aes(x=time), linewidth=1) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong, color=effect), linewidth=1) +
  facet_grid(rows='effect') + 
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     # expand=expansion(mult=.1)) + 
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  scale_color_manual(name='',
                     breaks=c('rule_by_val_zero', 'stim_by_val_zero', 'resp_by_val_zero'),
                     values=c('rule_by_val_zero' = '#9467bd',
                              'stim_by_val_zero' = '#17becf',
                              'resp_by_val_zero' = '#e377c2')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme() 


fig_cue_ex_by_val_int <- plot_grid(plot_cue_ex_by_val_int, 
                                   plot_cue_ex_by_val_int_bf,
                                   ncol=1, nrow=2,
                                   axis='tlbr',
                                   rel_heights=c(1, 1),
                                   align="v")

svg(paste0(path_out, 'fig_cue_ex_by_val_int_zeroIntercept.svg'),
    width=8, height=8)
plot(fig_cue_ex_by_val_int)
dev.off()


## Plot cue_rc_by_val -----------------------------------------------------------

# apply BF function to rslts_val
rslts_by_val_bf <- rslts_by_val[, 
                                lapply(.SD, extract_bf_1samp), 
                                .SDcols=stim:rule, 
                                by=c('value', 'tpoint', 'time')
][, 
  melt(.SD, id.vars=c('value', 'tpoint', 'time'),
       variable.name='effect',
       value.name='BF'),
  .SDcols=value:rule
][, ':=' (BF_strong = ifelse(BF > 1, BF, NA),
          BF_subs = ifelse(BF > 0.5, BF, NA))]

# get paired BFs for rslts_val
rslts_by_val_wide <- dcast(rslts_by_val,
                           subID + tpoint + time ~ value, value.var = c('stim', 'resp', 'rule'
                                                                        #'stim_conj', 'resp_conj', 'rule_conj'
                           )) # 'conj'
rslts_by_val_bf_paired <- data.table()
for (t in seq(0, n_tpoints - 1)) {
  # subset data
  rslts_by_val_wide_t <- rslts_by_val_wide[tpoint==t, ]
  # run test for each effect
  bf_stim <- ttestBF(rslts_by_val_wide_t$stim_hi, 
                     rslts_by_val_wide_t$stim_lo, 
                     mu=0, paired=T)
  bf_stim <- log10(as.numeric(as.vector(bf_stim))[1])
  bf_resp <- ttestBF(rslts_by_val_wide_t$resp_hi, 
                     rslts_by_val_wide_t$resp_lo, 
                     mu=0, paired=T)
  bf_resp <- log10(as.numeric(as.vector(bf_resp))[1])
  bf_rule <- ttestBF(rslts_by_val_wide_t$rule_hi, 
                     rslts_by_val_wide_t$rule_lo, 
                     mu=0, paired=T)
  bf_rule <- log10(as.numeric(as.vector(bf_rule))[1])
  # bf_conj <- ttestBF(rslts_by_val_wide_t$conj_hi, 
  #                    rslts_by_val_wide_t$conj_lo, 
  #                    mu=0, paired=T)
  # bf_conj <- log10(as.numeric(as.vector(bf_conj))[1])
  # save output
  bf_df <- data.table(tpoint = t,
                      time = ts[t+1],
                      stim=bf_stim,
                      stim_subs=ifelse(bf_stim > 0.5, bf_stim, NA),
                      stim_strong=ifelse(bf_stim > 1, bf_stim, NA),
                      resp=bf_resp,
                      resp_subs=ifelse(bf_resp > 0.5, bf_resp, NA),
                      resp_strong=ifelse(bf_resp > 1, bf_resp, NA),
                      rule=bf_rule,
                      rule_subs=ifelse(bf_rule > 0.5, bf_rule, NA),
                      rule_strong=ifelse(bf_rule > 1, bf_rule, NA)
                      # conj=bf_conj,
                      # conj_subs=ifelse(bf_conj > 0.5, bf_conj, NA)
  )
  
  rslts_by_val_bf_paired <- rbind(rslts_by_val_bf_paired, bf_df)
}

# average across subjects
rslts_by_val_p <- rslts_by_val[, lapply(.SD, mean), by=c('time', 'value'), 
                               .SDcols=c('stim', 'resp', 'rule'
                                         #'stim_conj', 'resp_conj', 'rule_conj'
                               )] #, 'conj'
# # find SE
# rslts_by_val_se_p <- rslts_by_val[, lapply(.SD, se_function), by=c('time', 'value'), 
#                             .SDcols=c('stim', 'resp', 'rule', 'val', 'conj', 'rt')]
# # join SE results to mean results
# rslts_by_val_p <- rslts_by_val_p[rslts_by_val_se_p,
#                                  on=c('time', 'value'),
#                                  j = .(time = time,
#                                        value = value,
#                                        stim = stim,
#                                        rule = rule,
#                                        resp = resp,
#                                        val = val,
#                                        conj = conj,
#                                        rt = rt,
#                                        stim_se = i.stim,
#                                        resp_se = i.resp,
#                                        rule_se = i.rule,
#                                        val_se = i.val,
#                                        conj_se = i.conj,
#                                        rt_se = i.rt)]

# put data.table into long format for plotting
# rslts_by_val_p <- rslts_by_val_p[,
#                       melt(.SD,
#                       id.vars=c('time', 'value'),
#                       measure.vars=c('stim', 'rule', 'resp', 'conj'),
#                                         # c('stim_se', 'rule_se', 'resp_se',
#                                         #   'val_se', 'conj_se', 'rt_se')
#                                         # ),
#                       variable.name='component',
#                       value.name=c('beta')) #c('beta', 'beta_se')
#                       ]

## stimulus
plot_stim_by_val <- ggplot() + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-0.1, 0.2)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(data=rslts_by_val_p, 
            aes(x=time, y=stim, color=value),
            linewidth=1) +
  # geom_line(data=rslts_by_val_p,
  #           aes(x=time, y=stim_conj, color=value),
  #           linetype=2,
  #           linewidth=1) +
  
  # customise
  scale_y_continuous(name='Beta', 
                     breaks=c(0, 0.25, 0.5),
                     labels=c('0.0', '0.25', '0.50'),
                     # limits=c(0.375, 1),
                     expand=expansion(mult = 0.1)) + 
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  scale_color_manual(name='Reward',
                     breaks=c('hi', 'lo'),
                     values=c('hi' = '#F28E2B',
                              'lo' = '#4E79A7'),
                     labels=c('high', 'low')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(0, 0.5)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')

plot_stim_by_val_bf_high <- ggplot(rslts_by_val_bf[effect=='stim' & value=='hi'], 
                                   aes(x=time),
                                   linewidth=1) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong), color='#F28E2B', linewidth=1) +
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     # expand=expansion(mult=.1)) + 
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  # scale_color_manual(name='Reward',
  #                    breaks=c('hi', 'lo'),
  #                    values=c('hi' = '#F28E2B',
  #                             'lo' = '#4E79A7'),
  #                    labels=c('high', 'low')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')

plot_stim_by_val_bf_low <- ggplot(rslts_by_val_bf[effect=='stim' & value=='lo'], 
                                  aes(x=time),
                                  linewidth=1) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong), color='#4E79A7', linewidth=1) +
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     # expand=expansion(mult=.1)) + 
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  # scale_color_manual(name='Reward',
  #                    breaks=c('hi', 'lo'),
  #                    values=c('hi' = '#F28E2B',
  #                             'lo' = '#4E79A7'),
  #                    labels=c('high', 'low')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')

plot_stim_by_val_bf_paired <- ggplot(rslts_by_val_bf_paired, 
                                     aes(x=time),
                                     linewidth=1) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
            aes(x, y),  linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
            aes(x, y),  linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y),  linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=stim), color='grey', linewidth=1) +
  # geom_line(aes(y=stim_strong, color='black'), linewidth=1) +
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     # expand=expansion(mult=.1)) + 
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme() 

fig_stim_by_val <- plot_grid(plot_stim_by_val, 
                             plot_stim_by_val_bf_high,
                             plot_stim_by_val_bf_low,
                             plot_stim_by_val_bf_paired,
                             ncol=1, nrow=4,
                             axis='tlbr',
                             rel_heights=c(1, .33, .33, .33),
                             align="v")

svg(paste0(path_out, 'fig_stim_by_val_noconj.svg'),
    width=8, height=8)
plot(fig_stim_by_val)
dev.off()

## response
plot_resp_by_val <- ggplot() + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)),
            aes(x, y),  linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-0.1, 0.2)),
            aes(x, y),  linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)),
            aes(x, y),  linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(data=rslts_by_val_p, 
            aes(x=time, y=resp, color=value),
            linewidth=1) +
  # geom_line(data=rslts_by_val_p, 
  #           aes(x=time, y=resp_conj, color=value),
  #           linetype=2,
  #           linewidth=1) +
  
  # customise
  scale_y_continuous(name='Beta', 
                     breaks=c(0, 0.25, 0.5),
                     labels=c('0.0', '0.25', '0.50'),
                     expand=expansion(mult = 0.1)) + 
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  scale_color_manual(name='Reward',
                     breaks=c('hi', 'lo'),
                     values=c('hi' = '#F28E2B',
                              'lo' = '#4E79A7'),
                     labels=c('high', 'low')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(0, 0.5)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')

plot_resp_by_val_bf_high <- ggplot(rslts_by_val_bf[effect=='resp' & value=='hi'], 
                                   aes(x=time),
                                   linewidth=1) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
            aes(x, y),  linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
            aes(x, y),  linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong), color='#F28E2B', linewidth=1) +
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')

plot_resp_by_val_bf_low <- ggplot(rslts_by_val_bf[effect=='resp' & value=='lo'], 
                                  aes(x=time),
                                  linewidth=1) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
            aes(x, y),  linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y),  linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong), color='#4E79A7', linewidth=1) +
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')

plot_resp_by_val_bf_paired <- ggplot(rslts_by_val_bf_paired, 
                                     aes(x=time),
                                     linewidth=1) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y),  linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=resp), color='grey', linewidth=1) +
  geom_line(aes(y=resp_strong), color='black', linewidth=1) +
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme() 

fig_resp_by_val <- plot_grid(plot_resp_by_val, 
                             plot_resp_by_val_bf_high,
                             plot_resp_by_val_bf_low,
                             plot_resp_by_val_bf_paired,
                             ncol=1, nrow=4,
                             axis='tlbr',
                             rel_heights=c(1, .33, .33, .33),
                             align="v")

svg(paste0(path_out, 'fig_resp_by_val.svg'),
    width=8, height=8)
plot(fig_resp_by_val)
dev.off()

## rule
plot_rule_by_val <- ggplot() + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)), 
            aes(x, y),  linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-0.1, 0.2)), 
            aes(x, y),  linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(data=rslts_by_val_p, 
            aes(x=time, y=rule, color=value),
            linewidth=1) +
  # geom_line(data=rslts_by_val_p, 
  #           aes(x=time, y=rule_conj, color=value),
  #           linetype=2,
  #           linewidth=1) +
  
  # customise
  scale_y_continuous(name='Beta', 
                     breaks=c(0, 0.25, 0.5),
                     labels=c('0.0', '0.25', '0.50'),
                     expand=expansion(mult = 0.1)) + 
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  scale_color_manual(name='Reward',
                     breaks=c('hi', 'lo'),
                     values=c('hi' = '#F28E2B',
                              'lo' = '#4E79A7'),
                     labels=c('high', 'low')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(0, 0.5)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')

plot_rule_by_val_bf_high <- ggplot(rslts_by_val_bf[effect=='rule' & value=='hi'], 
                                   aes(x=time),
                                   linewidth=1) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
            aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
            aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  # geom_line(aes(y=BF, color=BF), linewidth=1) +
  geom_line(aes(y=BF_strong), color='#F28E2B', linewidth=1) +
  # geom_line(aes(y=BF_subs, color=BF_subs), linewidth=1) +
  
  # customise
  # scale_colour_continuous(type = "viridis") +
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'none')

plot_rule_by_val_bf_low <- ggplot(rslts_by_val_bf[effect=='rule' & value=='lo'], 
                                  aes(x=time),
                                  linewidth=1) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
            aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
            aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong), color='#4E79A7', linewidth=1) +
  # geom_line(aes(y=BF_strong, color=BF_strong), linewidth=1) +
  
  
  # customise
  # scale_colour_continuous(type = "gradient") +
  scale_colour_gradientn(colours = bf_palette) + 
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = c(0.2, 0.2)) 

plot_rule_by_val_bf_paired <- ggplot(rslts_by_val_bf_paired, 
                                     aes(x=time),
                                     linewidth=1) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
            aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
            aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=rule), color='grey', linewidth=1) +
  # geom_line(aes(y=rule_strong), color='black', linewidth=1) +
  
  # customise
  # scale_colour_continuous(palette = paletteer_c("ggthemes::Red", 30), 
  #                         type = "gradient") +
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme() 

fig_rule_by_val <- plot_grid(plot_rule_by_val, 
                             plot_rule_by_val_bf_high,
                             plot_rule_by_val_bf_low,
                             plot_rule_by_val_bf_paired,
                             ncol=1, nrow=4,
                             axis='tlbr',
                             rel_heights=c(1, .33, .33, .33),
                             align="v")

svg(paste0(path_out, 'fig_rule_by_val_noconj.svg'),
    width=8, height=8)
plot(fig_rule_by_val)
dev.off()

## conj
# plot_conj_by_val <- ggplot() + 
#   
#   # add reference lines
#   geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)), 
#             aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
#   geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-0.1, 0.2)), 
#             aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
#   geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
#             aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
#   
#   # add data
#   geom_line(data=rslts_by_val_p, 
#             aes(x=time, y=conj, color=value),
#             linewidth=1) +
#   
#   # customise
#   scale_y_continuous(name='Beta', 
#                      breaks=c(0, 0.25, 0.5),
#                      labels=c('0.0', '0.25', '0.50'),
#                      expand=expansion(mult = 0.1)) + 
#   scale_x_continuous(name='Time from rule onset (s)',
#                      breaks=c(0, 0.3, 0.6, 0.9),
#                      labels=c('0.0', '0.3', '0.6', '0.9')) +
#   scale_color_manual(name='Reward',
#                      breaks=c('hi', 'lo'),
#                      values=c('hi' = '#F28E2B',
#                               'lo' = '#4E79A7'),
#                      labels=c('high', 'low')) +
#   geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(0, 0.5)),
#                   aes(x, y), size=1, color='black') +
#   my_theme() + theme(legend.position = 'top')
# 
# plot_conj_by_val_bf_high <- ggplot(rslts_by_val_bf[effect=='conj' & value=='hi'], 
#                                    aes(x=time),
#                                    linewidth=1) + 
#   
#   # add reference lines
#   geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
#             aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
#   geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
#             aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
#   geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
#             aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
#   
#   # add data
#   geom_line(aes(y=BF), color='grey', linewidth=1) +
#   geom_line(aes(y=BF_subs), color='#F28E2B', linewidth=1) +
#   
#   # customise
#   scale_y_continuous(name='BF (log10)',
#                      breaks=c(-4, 0, 4),
#                      limits=c(-7, 7)) +  
#   scale_x_continuous(name='Time from rule onset (s)',
#                      breaks=c(0, 0.3, 0.6, 0.9),
#                      labels=c('0.0', '0.3', '0.6', '0.9')) +
#   geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
#                   aes(x, y), size=1, color='black') +
#   my_theme() + theme(legend.position = 'top')
# 
# plot_conj_by_val_bf_low <- ggplot(rslts_by_val_bf[effect=='conj' & value=='lo'], 
#                                   aes(x=time),
#                                   linewidth=1) + 
#   
#   # add reference lines
#   geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
#             aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
#   geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
#             aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
#   geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
#             aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
#   
#   # add data
#   geom_line(aes(y=BF), color='grey', linewidth=1) +
#   geom_line(aes(y=BF_subs), color='#4E79A7', linewidth=1) +
#   
#   # customise
#   scale_y_continuous(name='BF (log10)',
#                      breaks=c(-4, 0, 4),
#                      limits=c(-7, 7)) +  
#   scale_x_continuous(name='Time from rule onset (s)',
#                      breaks=c(0, 0.3, 0.6, 0.9),
#                      labels=c('0.0', '0.3', '0.6', '0.9')) +
#   geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
#                   aes(x, y), size=1, color='black') +
#   my_theme() + theme(legend.position = 'top')
# 
# plot_conj_by_val_bf_paired <- ggplot(rslts_by_val_bf_paired, 
#                                      aes(x=time),
#                                      linewidth=1) + 
#   
#   # add reference lines
#   geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
#             aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
#   geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
#             aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
#   geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
#             aes(x, y), color = 'grey', linetype = 3, linewidth=0.8, alpha = 0.4) +
#   
#   # add data
#   geom_line(aes(y=conj), color='grey', linewidth=1) +
#   geom_line(aes(y=conj_subs), color='black', linewidth=1) +
#   
#   # customise
#   scale_y_continuous(name='BF (log10)',
#                      breaks=c(-4, 0, 4),
#                      limits=c(-7, 7)) +  
#   scale_x_continuous(name='Time from rule onset (s)',
#                      breaks=c(0, 0.3, 0.6, 0.9),
#                      labels=c('0.0', '0.3', '0.6', '0.9')) +
#   geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
#                   aes(x, y), size=1, color='black') +
#   my_theme() 
# 
# fig_conj_by_val <- plot_grid(plot_conj_by_val, 
#                              plot_conj_by_val_bf_high,
#                              plot_conj_by_val_bf_low,
#                              plot_conj_by_val_bf_paired,
#                              ncol=1, nrow=4,
#                              axis='tlbr',
#                              rel_heights=c(1, .33, .33, .33),
#                              align="v")
# 
# svg(paste0(path_out, 'fig_conj_by_val.svg'),
#     width=8, height=8)
# plot(fig_conj_by_val)
# dev.off()

## Plot cue_rc_int -------------------------------------------------------------

# apply BF function to rslts_int
rslts_cue_rc_int_bf <- rslts_cue_rc_int[, 
                          lapply(.SD, extract_bf_1samp), 
                          .SDcols=stim:rule_conj,
                          by=c('tpoint', 'time')
][, 
  melt(.SD, id.vars=c('tpoint', 'time'),
       variable.name='effect',
       value.name='BF'),
  .SDcols=tpoint:rule_conj
][, ':=' (BF_strong = ifelse(BF > 1, BF, NA),
          BF_subs = ifelse(BF > 0.5, BF, NA))]

rslts_cue_rc_int_p <- rslts_cue_rc_int[, lapply(.SD, mean), by=time, 
                         .SDcols=c('stim', 'stim_by_val', 'stim_conj',
                                   'resp', 'resp_by_val', 'resp_conj',
                                   # 'resp2', 'resp_by_val2', 
                                   # 'resp3', 'resp_by_val3',
                                   'rule', 'rule_by_val', 'rule_conj'
                                   #conj', 
                                   #'rand1', 'conj1', 
                                   # 'rand2','conj2',
                                   # 'rand22', 'conj22'
                         )]

### stimulus 
plot_int_stim <- ggplot(rslts_cue_rc_int_p, aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=stim), color ='#17becf', linewidth=1) +
  geom_line(aes(y=stim_by_val), color='black', linewidth=1, linetype=2) +
  # geom_line(aes(y=stim_by_val_resp), color='#17becf', linewidth=1, linetype=3) +
  # geom_line(aes(y=stim_conj), color='black', linewidth=1, linetype=1) +
  # 
  # customise
  scale_y_continuous(name='Beta', 
                     breaks=c(0, 0.25, 0.5, 0.75),
                     labels=c('0.0', '0.25', '0.50', '0.75'),
                     expand=expansion(mult = 0.1)) + 
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(0, 0.5)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')


plot_int_stim_bf <- ggplot(rslts_cue_rc_int_bf[effect=='stim',], 
                           aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_subs), color='#17becf', linewidth=1) +
  
  # customise
  # scale_colour_gradientn(colours = bf_palette) + 
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme()

plot_int_stim_by_val_bf <- ggplot(rslts_cue_rc_int_bf[effect=='stim_by_val',], 
                                  aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong), color='black', linewidth=1) +
  
  # customise
  # scale_colour_gradientn(colours = bf_palette) + 
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme()

fig_int_stim <- plot_grid(plot_int_stim, 
                          plot_int_stim_bf,
                          plot_int_stim_by_val_bf,
                          ncol=1, nrow=3,
                          axis='tlbr',
                          rel_heights=c(1, .33, .33),
                          align="v")

svg(paste0(path_out, 'fig_cue_rc_int_stim.svg'),
    width=8, height=8)
plot(fig_int_stim)
dev.off()

### response 
plot_int_resp <- ggplot(rslts_cue_rc_int_p, aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=resp), color ='#e377c2', linewidth=1) +
  geom_line(aes(y=resp_by_val), color='black', linewidth=1, linetype=2) +
  # geom_line(aes(y=resp_conj), color='black', linewidth=1, linetype=1) +
  
  # customise
  scale_y_continuous(name='Beta', 
                     breaks=c(0, 0.25, 0.5, 0.75),
                     labels=c('0.0', '0.25', '0.50', '0.75'),
                     expand=expansion(mult = 0.1)) + 
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(0, 0.5)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')


plot_int_resp_bf <- ggplot(rslts_cue_rc_int_bf[effect=='resp',], 
                           aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong), color='#e377c2', linewidth=1) +
  
  # customise
  # scale_colour_gradientn(colours = bf_palette) + 
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     limits=c(-7, 10)) +  
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme()

plot_int_resp_by_val_bf <- ggplot(rslts_cue_rc_int_bf[effect=='resp_by_val',],
                                  aes(x=time)) +

  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +

  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_subs), color='black', linewidth=1) +

  # customise
  # scale_colour_gradientn(colours = bf_palette) +
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     limits=c(-7, 7)) +
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme()

fig_int_resp <- plot_grid(plot_int_resp, 
                          plot_int_resp_bf,
                          plot_int_resp_by_val_bf,
                          ncol=1, nrow=3,
                          axis='tlbr',
                          rel_heights=c(1, .33, .33),
                          align="v")

svg(paste0(path_out, 'fig_cue_rc_int_resp.svg'),
    width=8, height=8)
plot(fig_int_resp)
dev.off()


### rule
plot_int_rule <- ggplot(rslts_cue_rc_int_p, aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=rule), color ='#9467bd', linewidth=1) +
  geom_line(aes(y=rule_by_val), color='black', linewidth=1, linetype=2) +
  # geom_line(aes(y=rule_conj), color='black', linewidth=1, linetype=1) +
  
  # customise
  scale_y_continuous(name='Beta', 
                     breaks=c(0, 0.25, 0.5, 0.75),
                     labels=c('0.0', '0.25', '0.50', '0.75'),
                     expand=expansion(mult = 0.1)) + 
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(0, 0.5)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')

plot_int_rule_bf <- ggplot(rslts_cue_rc_int_bf[effect=='rule',], 
                           aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong), color='#9467bd', linewidth=1) +
  
  # customise
  # scale_colour_gradientn(colours = bf_palette) + 
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme()

plot_int_rule_by_val_bf <- ggplot(rslts_cue_rc_int_bf[effect=='rule_by_val',], 
                                  aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_subs), color='black', linewidth=1) +
  
  # customise
  # scale_colour_gradientn(colours = bf_palette) + 
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme()

fig_int_rule <- plot_grid(plot_int_rule, 
                          plot_int_rule_bf,
                          plot_int_rule_by_val_bf,
                          ncol=1, nrow=3,
                          axis='tlbr',
                          rel_heights=c(1, .33, .33),
                          align="v")

svg(paste0(path_out, 'fig_cue_rc_int_rule.svg'),
    width=8, height=8)
plot(fig_int_rule)
dev.off()

### all int 
plot_int_all <- ggplot(rslts_int_p, aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=conj, color='conj'), linewidth=1) +
  geom_line(aes(y=stim, color='stimulus'), linewidth=1) +
  geom_line(aes(y=stim_by_val, color='stimulus'), linewidth=1, linetype=2) +
  geom_line(aes(y=resp, color='response'), linewidth=1) +
  geom_line(aes(y=resp_by_val, color='response'), linewidth=1, linetype=2) +
  geom_line(aes(y=rule, color='rule'), linewidth=1) +
  geom_line(aes(y=rule_by_val, color='rule'), linewidth=1, linetype=2) +
  
  # customise
  scale_y_continuous(name='Beta', 
                     breaks=c(0, 0.25, 0.5),
                     labels=c('0.0', '0.25', '0.50'),
                     # limits=c(0.375, 1),
                     expand=expansion(mult = 0.1)) + 
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  scale_color_manual(name='',
                     breaks=c('rule', 'stimulus', 'response', 'conj'),
                     values=c('rule' = '#9467bd',
                              'stimulus' = '#17becf',
                              'response' = '#e377c2',
                              'conj' = 'black'),
                     labels=c('rule', 'stimulus', 'response', 'conj')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(0, 0.5)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')

plot_int_all_bfs <- ggplot(rslts_int_bf[!(effect %in% c('rand1', 'rand2', 'conj1', 'conj2'))], aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  # geom_line(aes(y=BF_strong, color=effect), linewidth=1) +
  geom_line(aes(y=BF_strong, color='black'), linewidth=1) +
  facet_grid(rows='effect') + 
  
  # customise
  # scale_colour_gradientn(colours = bf_palette) +
  scale_color_manual(name='',
                     breaks=c('rule', 'rule_by_val',  
                              'stim', 'stim_by_val', 
                              'resp', 'resp_by_val',
                              'conj'),
                     values=c('rule' = '#9467bd',
                              'rule_by_val' = '#9467bd',
                              'stim' = '#17becf',
                              'stim_by_val' = '#17becf',
                              'resp' = '#e377c2',
                              'resp_by_val' = '#e377c2',
                              'conj' = 'black'),
                     # labels=c('rule', 'stimulus', 'response')
  ) +
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     # expand=expansion(mult=.1)) + 
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme() #+ theme(legend.position = 'bottom')


fig_all <- plot_grid(plot_int_all, 
                     plot_int_all_bfs, 
                     ncol=1, nrow=2,
                     axis='tlbr',
                     rel_heights=c(1, 1),
                     align="v")

svg(paste0(path_out, 'fig_int_all.svg'),
    width=8, height=9)
plot(fig_all)
dev.off()

png(paste0(path_out, 'fig_all.png'),
    width=8, height=8, units='in', res=400)
plot(fig_all)
dev.off()




#### Response Locked Epochs #### ===============================================

## load data
rslts_fdbc_rc_all <- fread(paste0(path_out, 'rslts_fdbc_rc_all.csv'))
rslts_fdbc_rc_int <- fread(paste0(path_out, 'rslts_fdbc_rc_int.csv'))
rslts_fdbc_rc_by_val_int <- fread(paste0(path_out, 'rslts_fdbc_rc_by_val_int.csv'))

# fix timing labels if necessary
n_tpoints = 282
ts <- seq(-1, 0.1, length.out=n_tpoints)
rslts_fdbc_rc_all$time <- rep(ts, times=27)
rslts_fdbc_rc_int$time <- rep(ts, times=27)

## Plot fdbc_rc_all ------------------------------------------------------------
# run statistics
rslts_fdbc_rc_all_bf <- rslts_fdbc_rc_all[, 
                                          lapply(.SD, extract_bf_1samp), 
                                          .SDcols=stim:conj, 
                                          by=c('tpoint', 'time')
][, 
  melt(.SD, id.vars=c('tpoint', 'time'),
       variable.name='effect',
       value.name='BF'),
  .SDcols=tpoint:conj
][, ':=' (BF_strong = ifelse(BF > 1, BF, NA),
          BF_subs = ifelse(BF > 0.5, BF, NA))]

# average data for plotting
rslts_fdbc_rc_all_p <- rslts_fdbc_rc_all[, lapply(.SD, mean), by=time, 
                         .SDcols=c('stim', 'resp', 'rule', 'val', 'conj')] 

plot_fdbc_rc_all <- ggplot(rslts_fdbc_rc_all_p, aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-1, 0.1), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=conj, color='conjunction'), linewidth=1) +
  geom_line(aes(y=stim, color='stimulus'), linewidth=1) +
  geom_line(aes(y=resp, color='response'), linewidth=1) +
  geom_line(aes(y=rule, color='rule'), linewidth=1) +
  geom_line(aes(y=val, color='value'), linewidth=1) + 
  
  # customise
  scale_y_continuous(name='Beta', 
                     breaks=c(0, 0.25, 0.5),
                     labels=c('0.0', '0.25', '0.50'),
                     # limits=c(0.375, 1),
                     expand=expansion(mult = 0.1)) + 
  scale_x_continuous(name='Time from response  (s)',
                     breaks=c(-0.9, -0.6, -0.3, 0),
                     labels=c('-0.9', '-0.6', '-0.3', '0.0')) +
  scale_color_manual(name='',
                     breaks=c('rule', 'stimulus', 'response', 'value', 'conjunction'),
                     values=c('rule' = '#9467bd',
                              'stimulus' = '#17becf',
                              'response' = '#e377c2',
                              'value' = '#ff7f0e',
                              'conjunction'  = 'black'),
                     labels=c('rule', 'stimulus', 'response', 'value', 'conjunction')) +
  geom_rangeframe(data=data.frame(x=c(-0.9, 0), y=c(0, 0.5)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')

plot_fdbc_rc_all_bfs <- ggplot(rslts_fdbc_rc_all_bf, aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-1, 0.1), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong, color=effect), linewidth=1) +
  facet_grid(rows='effect') + 
  
  # customise
  # scale_colour_gradientn(colours = bf_palette) + 
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     # expand=expansion(mult=.1)) + 
                     limits=c(-5, 10)) +  
  scale_x_continuous(name='Time from response  (s)',
                     breaks=c(-0.9, -0.6, -0.3, 0),
                     labels=c('-0.9', '-0.6', '-0.3', '0.0')) +
  scale_color_manual(name='',
                     breaks=c('rule', 'stim', 'resp', 'val', 'conj'),
                     values=c('rule' = '#9467bd',
                              'stim' = '#17becf',
                              'resp' = '#e377c2',
                              'val' = '#ff7f0e',
                              'conj'  = 'black'),
                     labels=c('rule', 'stim', 'resp', 'val', 'conj')) +
  geom_rangeframe(data=data.frame(x=c(-0.9, 0), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme()


fig_fdbc_rc_all <- plot_grid(plot_fdbc_rc_all, 
                             plot_fdbc_rc_all_bfs, 
                             ncol=1, nrow=2,
                             axis='tlbr',
                             rel_heights=c(1, 1),
                             align="v")

svg(paste0(path_out, 'fig_fdbc_rc_all.svg'),
    width=8, height=10)
plot(fig_fdbc_rc_all)
dev.off()

## Plot fdbc_rc_by_val_int -----------------------------------------------------

# run statistics
rslts_fdbc_rc_by_val_int_bf <- rslts_fdbc_rc_by_val_int[, 
                                                        lapply(.SD, extract_bf_1samp), 
                                                        .SDcols=int_rule:conj_resp,
                                                        by=c('tpoint', 'time')
][, 
  melt(.SD, id.vars=c('tpoint', 'time'),
       variable.name='effect',
       value.name='BF'),
  .SDcols=tpoint:conj_resp
][, ':=' (BF_strong = ifelse(BF > 1, BF, NA),
          BF_subs = ifelse(BF > 0.5, BF, NA))]

# average across subjects
rslts_fdbc_rc_by_val_int_p <- rslts_fdbc_rc_by_val_int[, 
                                 lapply(.SD, mean), by=c('time'), 
                                 .SDcols=c('rule', 'rule_by_val', 'conj_rule',
                                           'stim', 'stim_by_val', 'conj_stim',
                                           'resp', 'resp_by_val', 'conj_resp')]

## rule
plot_fdbc_rc_by_val_int_rule <- ggplot() + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-1, 0.1), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(data=rslts_fdbc_rc_by_val_int_p, 
            aes(x=time, y=rule), color='#9467bd',
            linewidth=1) +
  geom_line(data=rslts_fdbc_rc_by_val_int_p, 
            aes(x=time, y=rule_by_val), color='#9467bd',
            linewidth=1,
            linetype=4) +
  
  # customise
  scale_y_continuous(name='Beta', 
                     breaks=c(0, 0.25, 0.5),
                     labels=c('0.0', '0.25', '0.50'),
                     expand=expansion(mult = 0.1)) + 
  scale_x_continuous(name='Time from response  (s)',
                     breaks=c(-0.9, -0.6, -0.3, 0),
                     labels=c('-0.9', '-0.6', '-0.3', '0.0')) +
  geom_rangeframe(data=data.frame(x=c(-0.9, 0), y=c(0, 0.5)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')

plot_fdbc_rc_by_val_int_rule_bf <- ggplot(rslts_fdbc_rc_by_val_int_bf[effect=='rule'], 
                       aes(x=time), linewidth=1) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-1, 0.1), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong), color='#9467bd', linewidth=1) +
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     # expand=expansion(mult=.1)) + 
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from response  (s)',
                     breaks=c(-0.9, -0.6, -0.3, 0),
                     labels=c('-0.9', '-0.6', '-0.3', '0.0')) +
  geom_rangeframe(data=data.frame(x=c(-0.9, 0), y=c(0, 0.5)),
                  aes(x, y), size=1, color='black') +
  my_theme() 

plot_fdbc_rc_by_val_int_rule_by_val_bf <- ggplot(rslts_fdbc_rc_by_val_int_bf[effect=='rule_by_val'], 
                                          aes(x=time), linewidth=1) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-1, 0.1), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong), color='#9467bd', linewidth=1) +
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     # expand=expansion(mult=.1)) + 
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from response  (s)',
                     breaks=c(-0.9, -0.6, -0.3, 0),
                     labels=c('-0.9', '-0.6', '-0.3', '0.0')) +
  geom_rangeframe(data=data.frame(x=c(-0.9, 0), y=c(0, 0.5)),
                  aes(x, y), size=1, color='black') +
  my_theme() 


fig_fdbc_rc_by_val_int_rule <- plot_grid(plot_fdbc_rc_by_val_int_rule, 
                                 plot_fdbc_rc_by_val_int_rule_bf,
                                 plot_fdbc_rc_by_val_int_rule_by_val_bf,
                                 ncol=1, nrow=3,
                                 axis='tlbr',
                                 rel_heights=c(1, .33, .33),
                                 align="v")

svg(paste0(path_out, 'fig_fdbc_rc_by_val_int_rule.svg'),
    width=8, height=8)
plot(fig_fdbc_rc_by_val_int_rule)
dev.off()

## stimulus
plot_fdbc_rc_by_val_int_stim <- ggplot() + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-1, 0.1), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(data=rslts_fdbc_rc_by_val_int_p, 
            aes(x=time, y=stim), color='#17becf',
            linewidth=1) +
  geom_line(data=rslts_fdbc_rc_by_val_int_p, 
            aes(x=time, y=stim_by_val), color='#17becf',
            linewidth=1,
            linetype=4) +
  
  # customise
  scale_y_continuous(name='Beta', 
                     breaks=c(0, 0.25, 0.5),
                     labels=c('0.0', '0.25', '0.50'),
                     expand=expansion(mult = 0.1)) + 
  scale_x_continuous(name='Time from response  (s)',
                     breaks=c(-0.9, -0.6, -0.3, 0),
                     labels=c('-0.9', '-0.6', '-0.3', '0.0')) +
  geom_rangeframe(data=data.frame(x=c(-0.9, 0), y=c(0, 0.5)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')

plot_fdbc_rc_by_val_int_stim_bf <- ggplot(rslts_fdbc_rc_by_val_int_bf[effect=='stim'], 
                                          aes(x=time), linewidth=1) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-1, 0.1), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong), color='#17becf', linewidth=1) +
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     # expand=expansion(mult=.1)) + 
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from response  (s)',
                     breaks=c(-0.9, -0.6, -0.3, 0),
                     labels=c('-0.9', '-0.6', '-0.3', '0.0')) +
  geom_rangeframe(data=data.frame(x=c(-0.9, 0), y=c(0, 0.5)),
                  aes(x, y), size=1, color='black') +
  my_theme() 

plot_fdbc_rc_by_val_int_stim_by_val_bf <- ggplot(rslts_fdbc_rc_by_val_int_bf[effect=='stim_by_val'], 
                                                 aes(x=time), linewidth=1) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-1, 0.1), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong), color='#17becf', linewidth=1) +
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     # expand=expansion(mult=.1)) + 
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from response  (s)',
                     breaks=c(-0.9, -0.6, -0.3, 0),
                     labels=c('-0.9', '-0.6', '-0.3', '0.0')) +
  geom_rangeframe(data=data.frame(x=c(-0.9, 0), y=c(0, 0.5)),
                  aes(x, y), size=1, color='black') +
  my_theme() 


fig_fdbc_rc_by_val_int_stim <- plot_grid(plot_fdbc_rc_by_val_int_stim, 
                                         plot_fdbc_rc_by_val_int_stim_bf,
                                         plot_fdbc_rc_by_val_int_stim_by_val_bf,
                                         ncol=1, nrow=3,
                                         axis='tlbr',
                                         rel_heights=c(1, .33, .33),
                                         align="v")

svg(paste0(path_out, 'fig_fdbc_rc_by_val_int_stim.svg'),
    width=8, height=8)
plot(fig_fdbc_rc_by_val_int_stim)
dev.off()

## response
plot_fdbc_rc_by_val_int_resp <- ggplot() + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-1, 0.1), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(data=rslts_fdbc_rc_by_val_int_p, 
            aes(x=time, y=resp), color='#17becf',
            linewidth=1) +
  geom_line(data=rslts_fdbc_rc_by_val_int_p, 
            aes(x=time, y=resp_by_val), color='#17becf',
            linewidth=1,
            linetype=4) +
  
  # customise
  scale_y_continuous(name='Beta', 
                     breaks=c(0, 0.25, 0.5),
                     labels=c('0.0', '0.25', '0.50'),
                     expand=expansion(mult = 0.1)) + 
  scale_x_continuous(name='Time from response  (s)',
                     breaks=c(-0.9, -0.6, -0.3, 0),
                     labels=c('-0.9', '-0.6', '-0.3', '0.0')) +
  geom_rangeframe(data=data.frame(x=c(-0.9, 0), y=c(0, 0.5)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')

plot_fdbc_rc_by_val_int_resp_bf <- ggplot(rslts_fdbc_rc_by_val_int_bf[effect=='resp'], 
                                          aes(x=time), linewidth=1) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-1, 0.1), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong), color='#17becf', linewidth=1) +
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     # expand=expansion(mult=.1)) + 
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from response  (s)',
                     breaks=c(-0.9, -0.6, -0.3, 0),
                     labels=c('-0.9', '-0.6', '-0.3', '0.0')) +
  geom_rangeframe(data=data.frame(x=c(-0.9, 0), y=c(0, 0.5)),
                  aes(x, y), size=1, color='black') +
  my_theme() 

plot_fdbc_rc_by_val_int_resp_by_val_bf <- ggplot(rslts_fdbc_rc_by_val_int_bf[effect=='resp_by_val'], 
                                                 aes(x=time), linewidth=1) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-1, 0.1), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong), color='#17becf', linewidth=1) +
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     # expand=expansion(mult=.1)) + 
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from response  (s)',
                     breaks=c(-0.9, -0.6, -0.3, 0),
                     labels=c('-0.9', '-0.6', '-0.3', '0.0')) +
  geom_rangeframe(data=data.frame(x=c(-0.9, 0), y=c(0, 0.5)),
                  aes(x, y), size=1, color='black') +
  my_theme() 


fig_fdbc_rc_by_val_int_resp <- plot_grid(plot_fdbc_rc_by_val_int_resp, 
                                         plot_fdbc_rc_by_val_int_resp_bf,
                                         plot_fdbc_rc_by_val_int_resp_by_val_bf,
                                         ncol=1, nrow=3,
                                         axis='tlbr',
                                         rel_heights=c(1, .33, .33),
                                         align="v")

svg(paste0(path_out, 'fig_fdbc_rc_by_val_int_resp.svg'),
    width=8, height=8)
plot(fig_fdbc_rc_by_val_int_resp)
dev.off()


## Plot fdbc_rc_int -------------------------------------------------------------

# apply BF function to rslts_int
rslts_fdbc_rc_int_bf <- rslts_fdbc_rc_int[, 
                                        lapply(.SD, extract_bf_1samp), 
                                        .SDcols=stim:rule_conj,
                                        by=c('tpoint', 'time')
][, 
  melt(.SD, id.vars=c('tpoint', 'time'),
       variable.name='effect',
       value.name='BF'),
  .SDcols=tpoint:rule_conj
][, ':=' (BF_strong = ifelse(BF > 1, BF, NA),
          BF_subs = ifelse(BF > 0.5, BF, NA))]

rslts_fdbc_rc_int_p <- rslts_fdbc_rc_int[, lapply(.SD, mean), by=time, 
                                       .SDcols=c('stim', 'stim_by_val', 'stim_conj',
                                                 'resp', 'resp_by_val', 'resp_conj',
                                                 # 'resp2', 'resp_by_val2', 
                                                 # 'resp3', 'resp_by_val3',
                                                 'rule', 'rule_by_val', 'rule_conj'
                                                 #conj', 
                                                 #'rand1', 'conj1', 
                                                 # 'rand2','conj2',
                                                 # 'rand22', 'conj22'
                                       )]

### response 
plot_int_resp <- ggplot(rslts_fdbc_rc_int_p, aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-0.1, 0.2)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=resp), color ='#e377c2', linewidth=1) +
  geom_line(aes(y=resp_by_val), color='black', linewidth=1, linetype=2) +
  # geom_line(aes(y=stim_by_val_resp), color='#17becf', linewidth=1, linetype=3) +
  # geom_line(aes(y=stim_conj), color='black', linewidth=1, linetype=1) +
  # 
  # customise
  scale_y_continuous(name='Beta', 
                     breaks=c(0, 0.25, 0.5, 0.75),
                     labels=c('0.0', '0.25', '0.50', '0.75'),
                     expand=expansion(mult = 0.1)) + 
  scale_x_continuous(name='Time from response  (s)',
                     breaks=c(-0.9, -0.6, -0.3, 0),
                     labels=c('-0.9', '-0.6', '-0.3', '0.0')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(0, 0.5)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')


plot_fdbc_stim_bf <- ggplot(rslts_fdbc_rc_int_bf[effect=='stim',], 
                           aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_subs), color='#e377c2', linewidth=1) +
  
  # customise
  # scale_colour_gradientn(colours = bf_palette) + 
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from response  (s)',
                     breaks=c(-0.9, -0.6, -0.3, 0),
                     labels=c('-0.9', '-0.6', '-0.3', '0.0')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme()

plot_int_resp_by_val_bf <- ggplot(rslts_fdbc_rc_int_bf[effect=='resp_by_val',], 
                                  aes(x=time)) + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0, 0.9), y=c(0, 0)), 
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=BF), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong), color='black', linewidth=1) +
  
  # customise
  # scale_colour_gradientn(colours = bf_palette) + 
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     limits=c(-7, 7)) +  
  scale_x_continuous(name='Time from response  (s)',
                     breaks=c(-0.9, -0.6, -0.3, 0),
                     labels=c('-0.9', '-0.6', '-0.3', '0.0')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme()

fig_int_stim <- plot_grid(plot_int_stim, 
                          plot_int_stim_bf,
                          plot_int_stim_by_val_bf,
                          ncol=1, nrow=3,
                          axis='tlbr',
                          rel_heights=c(1, .33, .33),
                          align="v")

svg(paste0(path_out, 'fig_cue_rc_int_stim.svg'),
    width=8, height=8)
plot(fig_int_stim)
dev.off()