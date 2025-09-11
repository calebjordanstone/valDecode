library(tidyverse)
library(data.table)
library(stringr)
library(ggplot2)
library(viridis)
library(magrittr)
library(ggpubr)
library(ggthemes)
library(cowplot)

# set data path
data_path <- "C:/Users/cstone/OneDrive - UNSW/Documents/Projects/my_experiments/val_decode/output/"
# load data
files <- list.files(data_path, pattern = 'dfun_cue_rc_avg')
# data <- fread(file.path(data_path, files), stringsAsFactors = T)
data <- rbindlist(lapply(file.path(data_path, files), fread))

# create model RDM vectors
stim_vec <- c(1, 0, 1, 0, 1, 0, 1, 0)
resp_vec <- c(1, 0, 0, 1, 1, 0, 0, 1)
rule_vec <- c(1, 1, 0, 0, 1, 1, 0, 0)
val_vec <- c(1, 1, 1, 1, 0, 0, 0, 0)

dt_all <- data.table()
for (sub in unique(data[, subID])) {
  
  # store model RDM vectors in data table
  dt <- data.table(
    class = as.integer(paste0(seq(1, 8), '10')),
    stim = stim_vec,
    resp = resp_vec,
    rule = rule_vec,
    val = val_vec
  )
  # wrangle decoding data into long format
  dat <- transpose(data[, dfun_110:dfun_810, ])
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
                           val=comp[4])
      DT <- rbind(DT, row_dt)
    }  
  }
  DT[, n_match := rowSums(.SD), .SDcols = c("stim", "resp", "rule", "val")] 
  # add number of feature matches to main data table
  dat <- dat[DT[, .(y, class, n_match)], on=c('y', 'class')]
  dat$n_match <- as.factor(dat$n_match)
  # append output
  dt_all <- rbind(dt_all, dat)
}

# create data table for plotting
p_dat <- dat[, .(meandf=mean(dfun)), by=.(tpoint, n_match, subID)
][, .(meanDF=mean(meandf),
      seDF=sd(meandf)/sqrt(length(unique(data$subID)))),
  by=.(tpoint, n_match)]
p_dat$t <- rep(seq(from=-0.1, to=1, length.out=282), times=4)


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
                color=n_match),
            linewidth=1) +
  geom_ribbon(data=p_dat,
              aes(t,
                  y=meanDF,
                  ymin=meanDF - seDF,
                  ymax=meanDF + seDF,
                  fill=n_match),
              colour=NA,
              alpha=0.2,
              show.legend=F) +
  # customise
  scale_y_continuous(name='Decision function', 
                     breaks=c(-1.8, -1.5, -1.2, - 0.9, -0.6, -0.3),
                     labels=c('-1.8', '-1.5', '-1.2', '-0.9', '-0.6', '-0.3'),
                     # limits=c(0.375, 1),
                     expand=expansion(mult = 0.1)) + 
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  scale_color_manual(name='Number of matching variables',
                     breaks=c('1', '2', '3', '4'),
                     values=c('1' = '#fdb42f', 
                              '2' = '#ed7953',
                              '3' = '#9c179e',
                              '4' = '#5c01a6'),
                     labels=c('1', '2', '3', '4')) +
  scale_fill_manual(name='',
                    breaks=c('1', '2', '3', '4'),
                    values=c('1' = '#fdb42f', 
                              '2' = '#ed7953',
                              '3' = '#9c179e',
                              '4' = '#5c01a6')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-0.3, -1.8)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')


png(paste0(data_path, 'plt_cue_rc_avg_decFun.png'),
    width=8, height=4, units='in', res=400)
plot(p)
dev.off()




# stim_vec <- c(1, 0, 1, 0, 1, 0, 1, 0,
#               0, 1, 0, 1, 0, 1, 0, 1,
#               1, 0, 1, 0, 1, 0, 1, 0,
#               0, 1, 0, 1, 0, 1, 0, 1,
#               1, 0, 1, 0, 1, 0, 1, 0,
#               0, 1, 0, 1, 0, 1, 0, 1,
#               1, 0, 1, 0, 1, 0, 1, 0,
#               0, 1, 0, 1, 0, 1, 0, 1)
# 
# # matrix(stim_vec, 8)
# resp_vec <- c(1, 0, 0, 1, 1, 0, 0, 1, 
#               0, 1, 1, 0, 0, 1, 1, 0,
#               0, 1, 1, 0, 0, 1, 1, 0,
#               1, 0, 0, 1, 1, 0, 0, 1,
#               1, 0, 0, 1, 1, 0, 0, 1, 
#               0, 1, 1, 0, 0, 1, 1, 0,
#               0, 1, 1, 0, 0, 1, 1, 0,
#               1, 0, 0, 1, 1, 0, 0, 1)
# # matrix(resp_vec, 8, 8)
# rule_vec <- c(1, 1, 0, 0, 1, 1, 0, 0,
#               1, 1, 0, 0, 1, 1, 0, 0,
#               0, 0, 1, 1, 0, 0, 1, 1,
#               0, 0, 1, 1, 0, 0, 1, 1,
#               1, 1, 0, 0, 1, 1, 0, 0,
#               1, 1, 0, 0, 1, 1, 0, 0,
#               0, 0, 1, 1, 0, 0, 1, 1,
#               0, 0, 1, 1, 0, 0, 1, 1)
# # matrix(rule_vec, 8, 8)
# val_vec <- c(1, 1, 1, 1, 0, 0, 0, 0,
#              1, 1, 1, 1, 0, 0, 0, 0,
#              1, 1, 1, 1, 0, 0, 0, 0,
#              1, 1, 1, 1, 0, 0, 0, 0,
#              0, 0, 0, 0, 1, 1, 1, 1,
#              0, 0, 0, 0, 1, 1, 1, 1,
#              0, 0, 0, 0, 1, 1, 1, 1,
#              0, 0, 0, 0, 1, 1, 1, 1)
# # matrix(val_vec, 8)
# conj_vec <- as.vector(diag(1, 8, 8))
# # diag(1, 8, 8)
# 
# (mat <- matrix(c(stim_vec, resp_vec, rule_vec, val_vec, conj_vec), nrow=5, byrow=T))

