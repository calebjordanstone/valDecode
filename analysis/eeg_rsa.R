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

# set data paths
path_exp <- "C:/Users/cstone/OneDrive - UNSW/Documents/Projects/my_experiments/val_decode/"
path_out <- paste0(path_exp, 'output/')
path_data <- paste0(path_exp, 'data/')

# load data

files_beh <- list.files(path_data, pattern = '.*(beh.txt)', recursive = T)
data_beh <- fread(file.path(path_data, files_beh[1]), stringsAsFactors = T)
files_dfun <- list.files(path_out, pattern = 'dfun_cue_rc_avg')
data_dfun <- fread(file.path(path_out, files_dfun[1]), stringsAsFactors = T)
# data <- rbindlist(lapply(file.path(data_path, files), fread))

## Run analysis ----------------------------------------------------------------
n_tpoints = 282 # epochs are from -0.1 s to 1 s relative to cue onset
ts <- seq(-0.1, 1, length.out=n_tpoints) # put time samples into actual times

# define model RDMs
vec_stim <- c(1, 0, 1, 0, 1, 0, 1, 0,
              0, 1, 0, 1, 0, 1, 0, 1,
              1, 0, 1, 0, 1, 0, 1, 0,
              0, 1, 0, 1, 0, 1, 0, 1,
              1, 0, 1, 0, 1, 0, 1, 0,
              0, 1, 0, 1, 0, 1, 0, 1,
              1, 0, 1, 0, 1, 0, 1, 0,
              0, 1, 0, 1, 0, 1, 0, 1)
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
mat_conj <- diag(1, 8, 8)

# wrangle data
rslts <- data.table()
for (sub in unique(data_dfun[, subID])) {
  
  # get condition RTs
  data_beh_sub <- data_beh[Subject == sub]
  data_beh_sub <- data_beh_sub[Accuracy == 1 & Block < 13
  ][, zRT := scale(RT)]
  data_beh_sub_av <- data_beh_sub[, 
    # (Accuracy == 1) & (Block < 13), 
    .(ZMeanRT=mean(zRT)),
    by = .(DistractorValue, ResponseRule, TargetPosition)
    ][order(DistractorValue, -ResponseRule, TargetPosition)]
  data_beh_sub_av[, class := as.integer(paste0(seq(1, 8), '10'))]
  
  # put decoding into long format
  data_dfun_long <- transpose(data_dfun[, dfun_110:dfun_810, ])
  data_dfun_long[, class := as.integer(paste0(seq(1, 8), '10'))]
  data_dfun_long <- melt(data_dfun_long, id.vars='class', 
                         variable.name='trial_by_tpoint', 
                         value.name='dfun')
  data_dfun_ids = data.table( # create data table of ID variables
    y = rep(data_dfun[, y], each=8),
    tpoint = rep(data_dfun[, tpoint], each=8),
    subID = rep(data_dfun[, subID], each=8)) 
  data_dfun_long <- cbind(data_dfun_ids, data_dfun_long) # add ID variables back to data
  setorder(data_dfun_long, y, tpoint, trial_by_tpoint, class) # make sure things are ordered correctly
  
  # add in RDMs vectors for each trial type
  data_dfun_long[, ':=' 
      (stim = case_when(
        unique(y) %in% c(110, 310, 510, 710) ~ mat_stim[, 1],
        T ~ mat_stim[, 2]),
       resp = case_when(
        unique(y) %in% c(110, 410, 510, 810) ~ mat_resp[, 1],
        T ~ mat_resp[, 2]),
       rule = case_when(
        unique(y) %in% c(110, 210, 510, 610) ~ mat_rule[, 1],
        T ~ mat_rule[, 3]),
       val = case_when(
        unique(y) %in% c(110, 210, 310, 410) ~ mat_val[, 1],
        T ~ mat_val[, 5]),
       conj = case_when(
        y == class ~ 1,
        T ~ 0)), 
      by = trial_by_tpoint]
  
  # add in RT nuisance vector
  data_dfun_long <- data_dfun_long[data_beh_sub_av[, .(ZMeanRT, class)], on='class']
  
  # loop through time points and run analysis
  for (t in seq(0, n_tpoints - 1)) { # subtract 1 to account for Python indexing starting at 0
    
    # subset data per time point
    data_dfun_t <- data_dfun_long[tpoint == t, ]
    
    # average over trials??
    # t_dat <- t_dat[, .(dfun=mean(dfun)), by=.(class, y)]
    # t_dat$stim <- stim_vec
    # t_dat$resp <- resp_vec
    # t_dat$rule <- rule_vec
    # t_dat$val <- val_vec
    
    # compute model
    # mdl <- lm(dfun ~ stim + resp + rule + val, data = t_dat)
    mdl_t <- lsfit(data_dfun_t[, .(stim, resp, rule, val, conj, ZMeanRT)], 
                   data_dfun_t[, dfun])
    
    # add betas to data table
    rslts_t <- data.table(
      subID = sub,
      time = ts[t+1],
      int = mdl_t$coefficients['Intercept'],
      stim = mdl_t$coefficients['stim'],
      resp = mdl_t$coefficients['resp'],
      rule = mdl_t$coefficients['rule'],
      val = mdl_t$coefficients['val'],
      conj = mdl_t$coefficients['conj'],
      rt = mdl_t$coefficients['ZMeanRT'])
    
    # append data from current time point to overall data table
    rslts <- rbind(rslts, rslts_t)
  }
}
  
# plot results
ggplot(data=rslts) +
  geom_line(aes(time, stim), color='red') +
  geom_line(aes(time, resp), color='blue') +
  geom_line(aes(time, rule), color='green') +
  geom_line(aes(time, val), color='purple') + 
  geom_line(aes(time, conj), color='black') +
  geom_line(aes(time, rt), color='black', linetype =2) 
  

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

# plot 
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


