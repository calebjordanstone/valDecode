library(tidyverse)
library(data.table)
library(stringr)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(ggthemes)
library(cowplot)
library(BayesFactor)
source(file.path("my_theme.R"))

path_exp <- "C:/Users/cstone/OneDrive - UNSW/Documents/Projects/my_experiments/val_decode/"
path_out <- paste0(path_exp, 'output/figures/')

# import data
data_path <- "C:/Users/cstone/OneDrive - UNSW/Documents/Projects/my_experiments/val_decode/data/"
files <- list.files(data_path, pattern = '.*(beh.txt)', recursive = T)
# subs <- unique(str_split_i(files, "/", 1))
# dt <- fread(file.path(data_path, files[1]), header=T, stringsAsFactors=T)
dt <- rbindlist(lapply(file.path(data_path, files), fread), fill = T)
# setDT(data)

# wrangle data
exclude <- c('sub-12')
dt <- dt[!(Subject %in% exclude), ]
dt[, Reward := ifelse(Block < 13, 'colour-contingent reward', 'colour-invariant reward')]
dt[, Error := ifelse(Accuracy == 1, 0, 1)]
dt$Subject <- as.factor(dt$Subject)
dt$DistractorValue <- as.factor(dt$DistractorValue)
dt$ResponseRule <- as.factor(dt$ResponseRule)
dt$Reward <- as.factor(dt$Reward)

# remove missing responses
p_dat <- dt[Accuracy == 1, 
     .(MeanRT=mean(RT),
       MeanAC=mean(Accuracy),
       MeanER=mean(Error)),
     by = .(ResponseRule, DistractorValue, Reward, Subject) ## added block
][order(Subject, Reward, DistractorValue, ResponseRule)] ## added block

# get grand averages
p_dat_av <- p_dat[, .(gavRT=mean(MeanRT),
                      gavAC=mean(MeanAC),
                      gavER=mean(MeanER)),
                  by=.(ResponseRule, DistractorValue, Reward)]

# plot data
rt <- ggplot(data=p_dat,
             aes(x=ResponseRule,
                 y=MeanRT,
                 color=DistractorValue)) + 

  # add individual data
  geom_point(alpha=0.25,
             position=position_jitterdodge(dodge.width=0.5,
                                           jitter.width=0.1, 
                                           seed=1234)) + 
  geom_tufteboxplot(median.type='line',
                    width=3,
                    voffset=0.01,
                    hoffset=0,
                    position=position_nudge(x=c(-0.25, 0.25))) + 
  
  # add average data 
  geom_point(data=p_dat_av,
             aes(x=ResponseRule,
                 y=gavRT,
                 color=DistractorValue),
             fill='white',
             stroke=1.25,
             shape=23,
             size=3,
             position=position_dodge(width=0.5)) +
               
  # geom_line(data=p_dat_av[Reward=='1-12'],
  #           aes(x=ResponseRule,
  #               y=gavRT,
  #               group=DistractorValue),
  #           position=position_dodge(width=0.5)) +
  
  facet_wrap('Reward') + 

  # customise
  scale_y_continuous(name='Reaction time (s)',
                     breaks=c(0.3, 0.4, 0.5, 0.6),
                     labels=c('0.3', '0.4', '0.5', '0.6'),
                     #limits=c(0.2, 0.7)
                     ) +
  scale_x_discrete(name='Rule') + 
  geom_rangeframe(data=data.frame(x=c(1, 2), y=c(0.3, 0.6)),
                  aes(x, y), size=1, color='black') +
  scale_color_manual(name='Distractor value',
                     breaks=c('high', 'low'),
                     values=c('high' = '#F28E2B',
                              'low' = '#4E79A7'),
                     labels=c('high', 'low')) +
  my_theme() + 
  theme(legend.position = 'top')

# plot data
ac <- ggplot(data=p_dat,
             aes(x=ResponseRule,
                 y=MeanER,
                 color=DistractorValue)) + 
  
  # add individual data
  geom_point(alpha=0.25,
             position=position_jitterdodge(dodge.width=0.5,
                                           jitter.width=0.1, 
                                           seed=1234)) + 
  geom_tufteboxplot(median.type='line',
                    width=3,
                    voffset=0.01,
                    hoffset=0,
                    position=position_nudge(x=c(-0.25, 0))) + 
  
  # add average data 
  geom_point(data=p_dat_av,
             aes(x=ResponseRule,
                 y=gavER,
                 color=DistractorValue),
             fill='white',
             alpha=1,
             size=3,
             stroke=1.25,
             shape=23,
             position=position_dodge(width=0.5)) +
  
  facet_wrap('Reward') +

  # customise
  scale_y_continuous(name='Error rate (%)',
                     breaks=c(0, 0.10, 0.2, 0.3),
                     labels=c('0', '10', '20', '30'),
                     # limits=c(0, 0.15)
                     ) +
  scale_x_discrete(name='Rule') + 
  geom_rangeframe(data=data.frame(x=c(1, 2), y=c(0, 0.3)),
                  aes(x, y), size=1, color='black') +
  scale_color_manual(name='Reward',
                     breaks=c('high', 'low'),
                     values=c('high' = '#F28E2B',
                              'low' = '#4E79A7'),
                     labels=c('high', 'low')) +
  my_theme() + 
  theme(legend.position = 'top')


fig <- ggdraw() +
  draw_plot(rt, x = 0, y = 0.5, width = 1, height = 0.5) +
  draw_plot(ac, x = 0, y = 0, width = 1, height = 0.5) 

# fig <- ggdraw() +
#   draw_plot(rt, x = 0, y = 0.5, width = 1, height = 0.5) +
#   draw_plot(ac, x = 0, y = 0, width = 1, height = 0.5) 

# save figure
svg("fig_beh.svg",
    width=8, height=6)
plot(fig)
dev.off()

## run statistics
rt_aov <- anovaBF(MeanRT ~ ResponseRule*DistractorValue*Reward + Subject, 
                  whichRandom = 'Subject', 
                  whichModels = 'withmain',
                  data=p_dat)

er_aov <- anovaBF(MeanER ~ ResponseRule*DistractorValue + Subject, 
                  whichRandom = 'Subject', 
                  whichModels = 'all',
                  data=p_dat[Reward=='13-20'])


### VMAC effect ================================================================
p_dat_wide <- dcast.data.table(p_dat, 
                               Subject + Reward  ~ ResponseRule + DistractorValue,
                               value.var = 'MeanRT')

p_vmac <- p_dat_wide[, .(vmac_away = A_high - A_low,
                         vmac_toward = T_high - T_low),
                     by=c('Subject', 'Reward')]

# find subjects that show both VMAC effects
# p_vmac <- p_vmac[(Reward == '1-12') & (vmac_away < 0 & vmac_toward > 0), ]
# vmac_subs <- c('sub-01', 'sub-04', 'sub-05', 'sub-06', 
#                'sub-09', 'sub-10', 'sub-13', 'sub-20', 
#                'sub-21', 'sub-28')

p_vmac_long <- p_vmac[, melt(.SD, 
                           id.vars=c('Subject', 'Reward'), 
                           variable.name='rule',
                           value.name="vmac_effect"),
                      .SDcols=Subject:vmac_toward
][order(Subject, rule)]

p_vmac_long_gav <- p_vmac_long[, 
                               .(mean_vmac_effect = mean(vmac_effect)), 
                               by=c('Reward', 'rule')]

plot_vmac_rc <- ggplot() + 
  
  # add zero line
  geom_line(data=data.frame(x=c('vmac_away', 'vmac_toward'), y=c(0, 0)), 
            aes(x, y, group=1), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(data=p_vmac_long[Reward=='colour-contingent reward'],
            aes(x=rule, y=vmac_effect, group=Subject),
            alpha=0.1,
            position=position_jitter(width=0.1, seed=1234)) +
  geom_point(data=p_vmac_long[Reward=='colour-contingent reward'],
             aes(x=rule, y=vmac_effect, color=rule),
             alpha=0.25,
             position=position_jitter(width=0.1, seed=1234)) +
  geom_line(data=p_vmac_long_gav[Reward=='colour-contingent reward'],
            aes(x=rule, y=mean_vmac_effect, group=1),
            linetype=2,
            size=1) +
  geom_point(data=p_vmac_long_gav[Reward=='colour-contingent reward'],
             aes(x=rule, y=mean_vmac_effect, color=rule),
             fill='white',
             size=3,
             alpha=1,
             stroke=1.25,
             shape=23) +
  geom_tufteboxplot(median.type='line',
                    width=3,
                    voffset=0.01,
                    hoffset=0,
                    position=position_nudge(x=c(-0.2, 0.2))) + 
  # facet_wrap('Block', nrow=3) +
  
  # customise
  scale_x_discrete(name='Rule',
                   labels=c('A', 'T')) +
  scale_y_continuous(name='VMAC effect (ms) (high - low)',
                     breaks=c(-0.1, -0.05, 0, 0.05),
                     labels=c('-100', '-50', '0', '50'),
                     limits=c(-0.1, 0.07)) + 
  scale_color_manual(name='rule',
                     breaks=c('vmac_toward', 'vmac_away'),
                     values=c('vmac_toward' = '#B07AA1',
                              'vmac_away' = '#a4a5d5'),
                     labels=c('high', 'low')) +
  geom_rangeframe(data=data.frame(x=c('vmac_away', 'vmac_toward'), y=c(-0.1, 0.05)),
                  aes(x, y), size=1, color='black') +
  my_theme() #+ scale_color_tableau()

plot_vmac_ex <- ggplot() + 
  
  # add zero line
  geom_line(data=data.frame(x=c('vmac_away', 'vmac_toward'), y=c(0, 0)), 
            aes(x, y, group=1), linetype = 3, linewidth=0.8, alpha = 0.4) +
  
  # add data
  geom_line(data=p_vmac_long[Reward=='colour-invariant reward'],
            aes(x=rule, y=vmac_effect, group=Subject),
            alpha=0.1,
            position=position_jitter(width=0.1, seed=1234)) +
  geom_point(data=p_vmac_long[Reward=='colour-invariant reward'],
             aes(x=rule, y=vmac_effect, color=rule),
             alpha=0.25,
             position=position_jitter(width=0.1, seed=1234)) +
  geom_line(data=p_vmac_long_gav[Reward=='colour-invariant reward'],
            aes(x=rule, y=mean_vmac_effect, group=1),
            linetype=2,
            size=1) +
  geom_point(data=p_vmac_long_gav[Reward=='colour-invariant reward'],
             aes(x=rule, y=mean_vmac_effect, color=rule),
             fill='white',
             size=3,
             alpha=1,
             stroke=1.25,
             shape=23) +
  geom_tufteboxplot(median.type='line',
                    width=3,
                    voffset=0.01,
                    hoffset=0,
                    position=position_nudge(x=c(-0.2, 0.2))) + 
  # facet_wrap('Block', nrow=2) +
  
  # customise
  scale_x_discrete(name='Rule',
                   labels=c('A', 'T')) +
  scale_y_continuous(name='VMAC effect (ms) (high - low)',
                     breaks=c(-0.1, -0.05, 0, 0.05),
                     labels=c('-100', '-50', '0', '50'),
                     limits=c(-0.1, 0.07)) + 
  scale_color_manual(name='rule',
                     breaks=c('vmac_toward', 'vmac_away'),
                     values=c('vmac_toward' = '#B07AA1',
                              'vmac_away' = '#a4a5d5'),
                     labels=c('high', 'low')) +
  geom_rangeframe(data=data.frame(x=c('vmac_away', 'vmac_toward'), y=c(-0.1, 0.05)),
                  aes(x, y), size=1, color='black') +
  my_theme()

fig_vmac <- ggdraw() +
  draw_plot(plot_vmac_rc, x = 0, y = 0, width = 0.5, height = 1) +
  draw_plot(plot_vmac_ex, x = 0.5, y = 0, width = 0.5, height = 1) 

svg(paste0(path_out, "fig_vmac_ex_by_block.svg"),
    width=8, height=8)
plot(plot_vmac_ex)
dev.off()


## run statistics
vmac_aov <- anovaBF(vmac_effect ~ Reward*rule + Subject, 
                  whichRandom = 'Subject', 
                  whichModels = 'all',
                  data=p_vmac_long)


