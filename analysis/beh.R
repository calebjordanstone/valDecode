## Notes: This script analyses the raw behavioural data and produces the corresponding figures.
## Input data can be found in "raw_behavioural_data.csv" at https://osf.io/2x3a8/files/osfstorage

## Load libraries
library(tidyverse)
library(data.table)
library(stringr)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(ggthemes)
library(cowplot)
library(BayesFactor)
library(rstatix)
library(lme4)
library(lmerTest)
source(file.path("my_theme.R"))
options(scipen=999)

## Set paths
# path_exp <- ""
# path_out <- ""
# path_data <- ""

# Load behavioural data 
dt <- fread('raw_behavioural_data.csv')

## Wrangle data
dt[Response != 999, mean(Accuracy), by=Subject]
exclude <- c('sub-12') # accuracy < 50%
dt <- dt[!(Subject %in% exclude), ]
dt[, RewardPhase := ifelse(Block < 13, 'Reward', 'Extinction')]
dt[, Error := ifelse(Accuracy == 1, 0, 1)]
dt$Subject <- as.factor(dt$Subject)
dt$DistractorValue <- as.factor(dt$DistractorValue)
dt$ResponseRule <- as.factor(dt$ResponseRule)
dt$RewardPhase <- as.factor(dt$RewardPhase)

# calculate % of trials with missing responses
dt[RT == 999, .N]/nrow(dt)

# calculate % of trials with RTs faster than 150 ms
dt[RT < .150, .N]/nrow(dt)

## Remove missing responses and fast responses, create averages per Subject
p_dat <- dt[RT != 999 & RT >= .150, 
     .(MeanRT=mean(RT),
       MeanER=mean(Error)),
     by = .(Subject, RewardPhase, DistractorValue, ResponseRule) 
][order(Subject, RewardPhase, DistractorValue, ResponseRule)] 

## Analyse data ----------------------------------------------------------------
# Reaction time ----------------------------------------------------------------
# check assumptions
p_dat[, ZMeanRT := scale(MeanRT), by = c('RewardPhase', 'DistractorValue', 'ResponseRule')]
p_dat[ZMeanRT >= 3]
p_dat %>% group_by(RewardPhase, DistractorValue, ResponseRule) %>% identify_outliers(MeanRT)
p_dat %>% group_by(RewardPhase, DistractorValue, ResponseRule) %>% shapiro_test(MeanRT)
ggqqplot(p_dat, 'MeanRT', ggtheme = theme_bw()) + 
  facet_grid(rows=vars(DistractorValue, ResponseRule), cols=vars(RewardPhase), labeller = "label_both")

# peform statistical tests
aov_rt <- anova_test(data = p_dat, 
                      dv = MeanRT, 
                      wid = Subject, 
                      within = c('RewardPhase', 'DistractorValue', 'ResponseRule'),
                      effect = 'pes')
get_anova_table(aov_rt)

aov_rt_2way <- p_dat %>% 
  group_by(RewardPhase) %>%
  anova_test(dv=MeanRT, 
             wid=Subject, 
             within=c('DistractorValue', 'ResponseRule'), 
             effect='pes') %>% 
  adjust_pvalue(method='bonferroni')
get_anova_table(aov_rt_2way)

paired_ttest_rt <- p_dat %>% 
  filter(RewardPhase=='Reward') %>% 
  group_by(ResponseRule) %>% 
  pairwise_t_test(MeanRT ~ DistractorValue, paired=T)
paired_ttest_rt

cohends_d_rt <- p_dat %>% 
  filter(RewardPhase=='Reward') %>% 
  group_by(ResponseRule) %>% 
  cohens_d(MeanRT ~ DistractorValue, paired=T)
cohends_d_rt

# get means
p_dat_av_rt <- p_dat[, .(gavRT=mean(MeanRT),
                        seRT=sd(MeanRT)/sqrt(39)), 
                  by=.(ResponseRule, DistractorValue, RewardPhase)]

p_dat[, .(mean=mean(MeanRT),
          sd=sd(MeanRT))
      , by=c('RewardPhase', 'ResponseRule')]

p_dat[,
      .(mean=mean(MeanRT),
        sd=sd(MeanRT)), 
      by=c('RewardPhase', 'DistractorValue', 'ResponseRule')]

# Error ------------------------------------------------------------------------
# check assumptions
p_dat[, ZMeanER := scale(MeanER), by = c('RewardPhase', 'DistractorValue', 'ResponseRule')]
p_dat[ZMeanER >= 3]
outliers <- c('sub-16', 'sub-19', 'sub-40') # > 3 SDs above the mean
p_dat %>% group_by(RewardPhase, DistractorValue, ResponseRule) %>% identify_outliers(MeanER)
p_dat %>% group_by(RewardPhase, DistractorValue, ResponseRule) %>% shapiro_test(MeanER)
ggqqplot(p_dat, 'MeanER', ggtheme = theme_bw()) + 
  facet_grid(rows=vars(DistractorValue, ResponseRule), cols=vars(RewardPhase), labeller = "label_both")

# peform statistical tests
aov_er <- anova_test(data = p_dat[!(Subject %in% outliers), ], 
                     dv = MeanER, 
                     wid = Subject, 
                     within = c('RewardPhase', 'DistractorValue', 'ResponseRule'),
                     effect = 'pes')
get_anova_table(aov_er)

aov_er_2way <- p_dat[!(Subject %in% outliers), ] %>%
  group_by(RewardPhase) %>%
  anova_test(dv=MeanER,
             wid=Subject,
             within=c('DistractorValue', 'ResponseRule'),
             effect='pes') %>%
  adjust_pvalue(method='bonferroni')
get_anova_table(aov_er_2way)

# get means
p_dat_av_er <- p_dat[!(Subject %in% outliers), 
                  .(gavER=mean(MeanER),
                    seER=sd(MeanER/sqrt(36))), # after exclusions
                  by=.(ResponseRule, DistractorValue, RewardPhase)]

p_dat[!(Subject %in% outliers),
      .(mean=mean(MeanER),
        sd=sd(MeanER)), 
      by=c('RewardPhase', 'DistractorValue', 'ResponseRule')]

## Plot data ------------------------------------------------------------------- ### NEED TO REDO FIGURES WITH FAST RESPONSES EXCLUDED
rt <- ggplot() + 

  # add average data 
  geom_pointrange(data=p_dat_av_rt,
                  aes(x=ResponseRule,
                      y=gavRT,
                      ymin=gavRT-seRT,
                      ymax=gavRT+seRT,
                      color=DistractorValue),
                  linewidth=1,
                  position=position_dodge(width=0.5)) +
  geom_point(data=p_dat_av_rt,
             aes(x=ResponseRule,
                 y=gavRT,
                 color=DistractorValue),
             fill='white',
             stroke=1.25,
             shape=23,
             size=3,
             position=position_dodge(width=0.5)) +
               
  facet_wrap('RewardPhase') + 

  # customise
  scale_y_continuous(name='Reaction time (s)',
                     breaks=c(0.35, 0.40, 0.45),
                     labels=c('0.35', '0.40', '0.45')) +
  scale_x_discrete(name='Rule') + 
  geom_rangeframe(data=data.frame(x=c(1, 2), y=c(0.35, 0.45)),
                  aes(x, y), size=1, color='black') +
  scale_color_manual(name='Distractor value',
                     breaks=c('high', 'low'),
                     values=c('high' = '#F28E2B',
                              'low' = '#4E79A7'),
                     labels=c('high', 'low')) +
  my_theme() + 
  theme(legend.position = 'top')

# plot data
ac <- ggplot() + 
  
  # add average data
  geom_pointrange(data=p_dat_av_er,
                  aes(x=ResponseRule,
                      y=gavER,
                      ymin=gavER-seER,
                      ymax=gavER+seER,
                      color=DistractorValue),
                  linewidth=1,
                  position=position_dodge(width=0.5)) +
  geom_point(data=p_dat_av_er,
             aes(x=ResponseRule,
                 y=gavER,
                 color=DistractorValue),
             fill='white',
             alpha=1,
             size=3,
             stroke=1.25,
             shape=23,
             position=position_dodge(width=0.5)) +
  
  facet_wrap('RewardPhase') +

  # customise
  scale_y_continuous(name='Error rate (%)',
                     breaks=c(0.04, 0.08, 0.12),
                     labels=c('4', '8', '12')) +
  scale_x_discrete(name='Rule') + 
  geom_rangeframe(data=data.frame(x=c(1, 2), y=c(0.04, 0.12)),
                  aes(x, y), size=1, color='black') +
  scale_color_manual(name='Reward',
                     breaks=c('high', 'low'),
                     values=c('high' = '#F28E2B',
                              'low' = '#4E79A7'),
                     labels=c('high', 'low')) +
  my_theme() + 
  theme(legend.position = 'top')


fig <- plot_grid(rt,
                 ac,
                 ncol=1, nrow=2,
                 rel_heights=c(1, 1),
                 align="hv")

# save figure
svg("fig_beh_means.svg",
    width=6, height=8)
plot(fig)
dev.off()


### VMAC effect ================================================================
## Reaction time ---------------------------------------------------------------
p_dat_wide_rt <- dcast.data.table(p_dat, 
                                  Subject + RewardPhase ~ ResponseRule + DistractorValue, 
                                  value.var = 'MeanRT')

p_vmac_rt <- p_dat_wide_rt[, .(vmac_away = A_high - A_low,
                               vmac_toward = T_high - T_low),
                           by=c('Subject', 'RewardPhase')] 


p_vmac_long_rt <- p_vmac_rt[, melt(.SD, 
                                   id.vars=c('Subject', 'RewardPhase'),  
                                   variable.name='rule',
                                   value.name="vmac_effect"),
                            .SDcols=Subject:vmac_toward
][order(Subject, rule, RewardPhase)] 
p_vmac_long_rt_gav <- p_vmac_long_rt[, 
                                     .(mean_vmac_effect = mean(vmac_effect)), 
                                     by=c('RewardPhase', 'rule')] 

jitter <- position_jitter(width = 0.1, height = 0, seed=1234)
plot_vmac_rt <- ggplot() + 
  
  # add zero line
  geom_line(data=data.frame(x=c('vmac_away', 'vmac_toward'), y=c(0, 0)), 
            aes(x, y, group=1), linetype = 3, linewidth=0.8, alpha = 1) +
  
  # add data
  geom_line(data=p_vmac_long_rt,
            aes(x=rule, y=vmac_effect, group=Subject),
            alpha=0.1,
            position=jitter) +
  geom_point(data=p_vmac_long_rt,
             aes(x=rule, y=vmac_effect, color=rule),
             alpha=0.25,
             position=jitter) +
  geom_line(data=p_vmac_long_rt_gav,
            aes(x=rule, y=mean_vmac_effect, group=1),
            linetype=2,
            size=1) +
  geom_point(data=p_vmac_long_rt_gav,
             aes(x=rule, y=mean_vmac_effect, color=rule),
             fill='white',
             size=4,
             alpha=1,
             stroke=1.25,
             shape=23) +
  geom_tufteboxplot(median.type='line',
                    width=3,
                    voffset=0.01,
                    hoffset=0,
                    position=position_nudge(x=c(-0.2, 0.2))) + 
  
  facet_wrap('RewardPhase', nrow=1) +
  
  # customise
  scale_x_discrete(name='Rule',
                   labels=c('A', 'T')) +
  scale_y_continuous(name='VMAC effect (ms) (high - low)',
                     breaks=c(-0.1, -0.05, 0, 0.05, 0.1),
                     labels=c('-100', '-50', '0', '50', '100'),
                     limits=c(-0.110, 0.110)) +
  scale_color_manual(name='rule',
                     breaks=c('vmac_toward', 'vmac_away'),
                     values=c('vmac_toward' = '#B07AA1',
                              'vmac_away' = '#a4a5d5'),
                     labels=c('high', 'low')) +
  geom_rangeframe(data=data.frame(x=c('vmac_away', 'vmac_toward'), y=c(-0.1, 0.1)),
                  aes(x, y), size=1, color='black') +
  my_theme()


## Error rate ------------------------------------------------------------------
p_dat_wide_er <- dcast.data.table(p_dat[!(Subject %in% outliers),], 
                               Subject + RewardPhase  ~ ResponseRule + DistractorValue,
                               value.var = 'MeanER')

p_vmac_er <- p_dat_wide_er[, .(vmac_away = A_high - A_low,
                               vmac_toward = T_high - T_low),
                     by=c('Subject', 'RewardPhase')]


p_vmac_long_er <- p_vmac_er[, melt(.SD, 
                           id.vars=c('Subject', 'RewardPhase'), 
                           variable.name='rule',
                           value.name="vmac_effect"),
                      .SDcols=Subject:vmac_toward
][order(Subject, rule)]

p_vmac_long_er_gav <- p_vmac_long_er[, 
                               .(mean_vmac_effect = mean(vmac_effect)), 
                               by=c('RewardPhase', 'rule')]

plot_vmac_er <- ggplot() + 
  
  # add zero line
  geom_line(data=data.frame(x=c('vmac_away', 'vmac_toward'), y=c(0, 0)), 
            aes(x, y, group=1), linetype = 3, linewidth=0.8, alpha = 1) +
  
  # add data
  geom_line(data=p_vmac_long_er,
            aes(x=rule, y=vmac_effect, group=Subject),
            alpha=0.1,
            position=position_jitter(width=0.1, seed=1234)) +
  geom_point(data=p_vmac_long_er,
             aes(x=rule, y=vmac_effect, color=rule),
             alpha=0.25,
             position=position_jitter(width=0.1, seed=1234)) +
  geom_line(data=p_vmac_long_er_gav,
            aes(x=rule, y=mean_vmac_effect, group=1),
            linetype=2,
            size=1) +
  geom_point(data=p_vmac_long_er_gav,
             aes(x=rule, y=mean_vmac_effect, color=rule),
             fill='white',
             size=4,
             alpha=1,
             stroke=1.25,
             shape=23) +
  geom_tufteboxplot(median.type='line',
                    width=3,
                    voffset=0.01,
                    hoffset=0,
                    position=position_nudge(x=c(-0.2, 0.2))) + 
  facet_wrap('RewardPhase', nrow=1) +
  
  # customise
  scale_x_discrete(name='Rule',
                   labels=c('A', 'T')) +
  scale_y_continuous(name='VMAC effect (%) (high - low)',
                     breaks=c(-0.1, 0, 0.1, 0.2),
                     labels=c('-10', '0', '10', 20),
                     limits=c(-0.125, 0.2)) + 
  scale_color_manual(name='rule',
                     breaks=c('vmac_toward', 'vmac_away'),
                     values=c('vmac_toward' = '#B07AA1',
                              'vmac_away' = '#a4a5d5'),
                     labels=c('high', 'low')) +
  geom_rangeframe(data=data.frame(x=c('vmac_away', 'vmac_toward'), y=c(-0.1, 0.2)),
                  aes(x, y), size=1, color='black') +
  my_theme()


fig_vmac <- plot_grid(
  plot_vmac_rt, 
  plot_vmac_er,
  ncol=1, nrow=2,
  axis='tlbr',
  rel_heights=c(1, 1),
  align="hv")

svg(paste0(path_out, "fig_beh_vmac.svg"),
    width=7, height=8)
plot(fig_vmac)
dev.off()


