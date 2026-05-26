## Notes: This script takes results from the binary decoding analysis performed in the 
# script "eeg_mvpa.py", found at https://github.com/calebjordanstone/valDecode/tree/main/analysis, 
# and runs statistical analysis on those results and produces figures of the results. 
## Input data can be found in "decode_by_value_results_files.zip" at https://osf.io/2x3a8/files/osfstorage

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

# Load data
files_dcd_by_val <- list.files(path_out, pattern = 'dcd_by_val_cue_rc_sub')
data_dcd_by_val <- rbindlist(lapply(file.path(path_out, files_dcd_by_val), fread))

## Run analysis ----------------------------------------------------------------
n_tpoints = 282 # epochs are from -0.1 s to 1 s relative to cue onset
ts <- seq(-0.1, 1, length.out=n_tpoints) # put time samples into actual times
nsubs <- 39 

# melt data table
data_dcd_by_val_melt <- data_dcd_by_val[,
                                        melt(.SD,
                                             id.vars=c('Value', 'Comparisons', 'subID'),
                                             variable.name='tpoint',
                                             value.name='dcd_acc'),
                                        .SDcols=T0:subID
][order(Value, Comparisons, subID, tpoint)]
data_dcd_by_val_melt$t <- rep(ts, times=39*2*3) # 39 subs * 2 values (high, low)x 3 variables (rule, stim, resp)

# write function to extract BFs
extract_bf_1samp <- function(x) {
  res <- ttestBF(x, mu=0.5, nullInterval=c(0.5, Inf))
  bf <- log10(as.numeric(as.vector(res))[1])
  return(bf)
}


# apply BF function to rslts_val
rslts_by_val_bf <- data_dcd_by_val_melt[,
                                lapply(.SD, extract_bf_1samp),
                                .SDcols='dcd_acc',
                                by=c('Comparisons', 'tpoint', 't', 'Value')
][, ':=' (BF_strong = ifelse(dcd_acc > 1, dcd_acc, NA),
          BF_subs = ifelse(dcd_acc > 0.5, dcd_acc, NA))
][order(Value, Comparisons, tpoint)]

# run paired samples t-tests
rslts_by_val_bf_paired <- data.table()
for (t in seq(0, 281)) {

  tp <- paste0("T", t)

  # run test for each effect
  bf_rule <- ttestBF(data_dcd_by_val_melt[Comparisons == 'scores_cue' & tpoint == tp & Value == 'hi', dcd_acc],
                     data_dcd_by_val_melt[Comparisons == 'scores_cue' & tpoint == tp & Value == 'lo', dcd_acc],
                     mu=0, paired=T)
  bf_rule <- log10(as.numeric(as.vector(bf_rule))[1])
  bf_stim <- ttestBF(data_dcd_by_val_melt[Comparisons == 'scores_stim' & tpoint == tp & Value == 'hi', dcd_acc],
                     data_dcd_by_val_melt[Comparisons == 'scores_stim' & tpoint == tp & Value == 'lo', dcd_acc],
                     mu=0, paired=T)
  bf_stim <- log10(as.numeric(as.vector(bf_stim))[1])
  bf_resp <- ttestBF(data_dcd_by_val_melt[Comparisons == 'scores_resp' & tpoint == tp & Value == 'hi', dcd_acc],
                     data_dcd_by_val_melt[Comparisons == 'scores_resp' & tpoint == tp & Value == 'lo', dcd_acc],
                     mu=0, paired=T)
  bf_resp <- log10(as.numeric(as.vector(bf_resp))[1])

  # save output
  bf_df <- data.table(tpoint = tp,
                      t = ts[t+1],
                      rule=bf_rule,
                      rule_subs=ifelse(bf_rule > 0.5, bf_rule, NA),
                      rule_strong=ifelse(bf_rule > 1, bf_rule, NA),
                      stim=bf_stim,
                      stim_subs=ifelse(bf_stim > 0.5, bf_stim, NA),
                      stim_strong=ifelse(bf_stim > 1, bf_stim, NA),
                      resp=bf_resp,
                      resp_subs=ifelse(bf_resp > 0.5, bf_resp, NA),
                      resp_strong=ifelse(bf_resp > 1, bf_resp, NA))

  rslts_by_val_bf_paired <- rbind(rslts_by_val_bf_paired, bf_df)
}

#### Plot decoding results -----------------------------------------------------
# average across subjects
rslts_by_val_p <- data_dcd_by_val_melt[,
                                       .(dcd_acc=mean(dcd_acc)),
                                       by=c('Value', 'Comparisons', 'tpoint', 't')
                                       ][order(Value, Comparisons, tpoint)]
## Rule cue --------------------------------------------------------------------
plot_cue_by_val <- ggplot() + 
  
  # add reference lines - cue locked
  geom_line(data=data.frame(x=c(0, 0), y=c(0.45, 0.55)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(0.45, 0.55)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-0.1, 0.9), y=c(0.5, 0.5)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +

  # add data
  geom_line(data=rslts_by_val_p[Comparisons=='scores_cue'], 
            aes(x=t, y=dcd_acc, color=Value), linewidth = 1) +

  # customise
  scale_y_continuous(name='Decoding accuracy (%)', 
                     breaks=c(0.5, 0.55, 0.6),
                     labels=c('50', '55', '60'),
                     expand=expansion(mult = 0.1)) +
  scale_x_continuous(name='Time from rule cue onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  scale_color_manual(name='Reward',
                     breaks=c('hi', 'lo'),
                     values=c('hi' = '#F28E2B',
                              'lo' = '#4E79A7'),
                     labels=c('high', 'low')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(0.5, 0.6)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')

plot_bfs_cue_high <- ggplot(rslts_by_val_bf[Comparisons=='scores_cue' & Value=='hi'], 
                            aes(x=t)) + 
  
  
  # add reference lines - cue locked
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-0.1, 1), y=c(0, 0)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +

  # add data
  geom_line(aes(y=dcd_acc), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong), color='#F28E2B', linewidth=1) +
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     limits=c(-7, 8)) +  
  scale_x_continuous(name='Time from rule cue onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme()

plot_bfs_cue_low <- ggplot(rslts_by_val_bf[Comparisons=='scores_cue' & Value=='lo'], 
                            aes(x=t)) + 
  
  # add reference lines - cue locked
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-0.1, 1), y=c(0, 0)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  
  # add data
  geom_line(aes(y=dcd_acc), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong), color='#4E79A7', linewidth=1) +
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     limits=c(-7, 8)) +  
  scale_x_continuous(name='Time from rule cue onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme()

plot_bfs_cue_paired <- ggplot(rslts_by_val_bf_paired, 
                           aes(x=t)) + 
  
 
  # add reference lines - cue locked
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-0.1, 1), y=c(0, 0)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +

  # add data
  geom_line(aes(y=rule), color='grey', linewidth=1) +
  geom_line(aes(y=rule_strong), color='black', linewidth=1) +
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     limits=c(-7, 8)) +  
  scale_x_continuous(name='Time from rule cue onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme()

# save figure
fig_cue <- plot_grid(plot_cue_by_val, 
                     plot_bfs_cue_high,
                     plot_bfs_cue_low,
                     plot_bfs_cue_paired,
                     ncol=1, nrow=4,
                     axis='tlbr',
                     rel_heights=c(1, 0.33, 0.33, 0.33),
                     align="v")

svg(paste0(path_out, 'fig_dcd_by_val_rc_rule.svg'),
    width=8, height=8)
plot(fig_cue)
dev.off()

## Target position -------------------------------------------------------------
plot_stim_by_val <- ggplot() + 
  
 
  # add reference lines - cue locked
  geom_line(data=data.frame(x=c(0, 0), y=c(0.45, 0.55)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(0.45, 0.55)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-0.1, 0.9), y=c(0.5, 0.5)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +

  # add data
  geom_line(data=rslts_by_val_p[Comparisons=='scores_stim'], 
            aes(x=t, y=dcd_acc, color=Value), linewidth=1) +
  
  # customise
  scale_y_continuous(name='Decoding accuracy (%)', 
                     breaks=c(0.45, 0.5, 0.55, 0.6),
                     labels=c('45', '50', '55', '60'),
                     expand=expansion(mult = 0.1)) +
  scale_x_continuous(name='Time from rule cue onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  scale_color_manual(name='Reward',
                     breaks=c('hi', 'lo'),
                     values=c('hi' = '#F28E2B',
                              'lo' = '#4E79A7'),
                     labels=c('high', 'low')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(0.45, 0.6)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')

plot_bfs_stim_high <- ggplot(rslts_by_val_bf[Comparisons=='scores_stim' & Value=='hi'], 
                            aes(x=t)) + 
  
  # add reference lines - cue locked
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-0.1, 1), y=c(0, 0)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  
  # add data
  geom_line(aes(y=dcd_acc), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong), color='#F28E2B', linewidth=1) +
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     limits=c(-8, 10)) +  
  scale_x_continuous(name='Time from rule cue onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme()

plot_bfs_stim_low <- ggplot(rslts_by_val_bf[Comparisons=='scores_stim' & Value=='lo'], 
                           aes(x=t)) + 
  
  # add reference lines - cue locked
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-0.1, 1), y=c(0, 0)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  
  # add data
  geom_line(aes(y=dcd_acc), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong), color='#4E79A7', linewidth=1) +
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     limits=c(-8, 10)) +  
  scale_x_continuous(name='Time from rule cue onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme()

plot_bfs_stim_paired <- ggplot(rslts_by_val_bf_paired, 
                              aes(x=t)) + 
  
  # add reference lines - cue locked
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-0.1, 1), y=c(0, 0)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=stim), color='grey', linewidth=1) +
  geom_line(aes(y=stim_strong), color='black', linewidth=1) +
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-4, 0, 4),
                     limits=c(-8, 8)) +  
  scale_x_continuous(name='Time from rule cue onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-4, 4)),
                  aes(x, y), size=1, color='black') +
  my_theme()

fig_target <- plot_grid(plot_stim_by_val, 
                        plot_bfs_stim_high,
                        plot_bfs_stim_low,
                        plot_bfs_stim_paired,
                        ncol=1, nrow=4,
                        axis='tlbr',
                        rel_heights=c(1, 0.33, 0.33, 0.33),
                        align="v")

svg(paste0(path_out, 'fig_dcd_by_val_rc_target.svg'),
    width=8, height=8)
plot(fig_target)
dev.off()

## Response -------------------------------------------------------------------------
plot_resp_by_val <- ggplot() + 

  # add reference lines - cue locked
  geom_line(data=data.frame(x=c(0, 0), y=c(0.45, 0.55)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(0.45, 0.55)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-0.1, 0.9), y=c(0.5, 0.5)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(data=rslts_by_val_p[Comparisons=='scores_resp'], 
            aes(x=t, y=dcd_acc, color=Value), linewidth=1) +
  
  # customise
  scale_y_continuous(name='Decoding accuracy (%)', 
                     breaks=c(0.5, 0.60, 0.7),
                     labels=c('50', '60', '70'),
                     expand=expansion(mult = 0.1)) +
  scale_x_continuous(name='Time from rule cue onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  scale_color_manual(name='Reward',
                     breaks=c('hi', 'lo'),
                     values=c('hi' = '#F28E2B',
                              'lo' = '#4E79A7'),
                     labels=c('high', 'low')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(0.5, 0.70)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')

plot_bfs_resp_high <- ggplot(rslts_by_val_bf[Comparisons=='scores_resp' & Value=='hi'], 
                             aes(x=t)) + 
  
  # add reference lines - cue locked
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-0.1, 1), y=c(0, 0)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  # add data
  geom_line(aes(y=dcd_acc), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong), color='#F28E2B', linewidth=1) +
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-5, 0, 5),
                     limits=c(-7, 12)) +  
  scale_x_continuous(name='Time from rule cue onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-5, 5)),
                  aes(x, y), size=1, color='black') +
  my_theme()

plot_bfs_resp_low <- ggplot(rslts_by_val_bf[Comparisons=='scores_resp' & Value=='lo'], 
                            aes(x=t)) + 
  
  # add reference lines - cue locked
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-0.1, 1), y=c(0, 0)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=dcd_acc), color='grey', linewidth=1) +
  geom_line(aes(y=BF_strong), color='#4E79A7', linewidth=1) +
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-5, 0, 5),
                     limits=c(-7, 12)) +  
  scale_x_continuous(name='Time from rule cue onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-5, 5)),
                  aes(x, y), size=1, color='black') +
  my_theme()

plot_bfs_resp_paired <- ggplot(rslts_by_val_bf_paired, 
                               aes(x=t)) + 
  
  # add reference lines - cue locked
  geom_line(data=data.frame(x=c(0, 0), y=c(-4, 4)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-4, 4)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-0.1, 1), y=c(0, 0)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(aes(y=resp), color='grey', linewidth=1) +
  geom_line(aes(y=resp_strong), color='black', linewidth=1) +
  
  # customise
  scale_y_continuous(name='BF (log10)',
                     breaks=c(-5, 0, 5),
                     limits=c(-7, 12)) +  
  scale_x_continuous(name='Time from rule cue onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-5, 5)),
                  aes(x, y), size=1, color='black') +
  my_theme()

fig_resp <- plot_grid(plot_resp_by_val, 
                      plot_bfs_resp_high,
                      plot_bfs_resp_low,
                      plot_bfs_resp_paired,
                      ncol=1, nrow=4,
                      axis='tlbr',
                      rel_heights=c(1, 0.33, 0.33, 0.33),
                      align="v")

svg(paste0(path_out, 'fig_dcd_by_val_rc_resp.svg'),
    width=8, height=8)
plot(fig_resp)
dev.off()
