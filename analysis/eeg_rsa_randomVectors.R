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
source(file.path("my_theme.R"))

# set data paths
path_exp <- "C:/Users/cstone/OneDrive - UNSW/Documents/Projects/my_experiments/val_decode/"
path_out <- paste0(path_exp, 'output/')
path_data <- paste0(path_exp, 'data/')

# load data
files_dfun <- list.files(path_out, pattern = 'dfun_cue_rc_avg_etc_sub') ################ cue-locked, reward contingency blocks - change here for different epochs
data_dfun <- rbindlist(lapply(file.path(path_out, files_dfun), fread))
## Run analysis ----------------------------------------------------------------
n_tpoints = 282 # epochs are from -0.1 s to 1 s relative to cue onset
n_iter <- 1000
n_subs <- 39
ts <- seq(-0.1, 1, length.out=n_tpoints) # put time samples into actual times

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

### Generate the random vectors
## I want a symmetric 8x8 (n=64) matrix with all on-diagonal elements equal to 1 and a total of 32 1s and 32 0s
## 32 - 8 = 24 off-diagonal 1s, 12 in each of the lower and upper triangles of the matrix
## Each triangle of the matrix contains 28 elements, so I begin by generating a vector with 12 of 28 elements as 1s 
x <- c(rep(1, times=12), rep(0, times=16))
## randomly shuffle the vector
x_samp <- sample(x)
## Add first iteration to list
cols <- list(x_samp)
## Create a loop where I shuffle the vector, and then add it to the list of shuffled vectors only if it doesn't match any of the previous. Do this until 1000 vectors.
n <- 1
while (n < n_iter) {
  x_samp <- sample(x)
  comp_vect <- vector(mode='logical', length=n_iter)
  for (col in seq(1, length(cols))) {
    comp_vect[col] <- identical(cols[col], x_samp) 
  }
  if (any(comp_vect)) { 
      next
  } else {
    cols <- append(cols, list(x_samp))
    n <- n + 1
  }
}
# save random vectors
dt <- data.table(do.call(cbind, cols))
write_csv(dt, paste0(path_out, 'random_vec.csv'))

# loop through participants
dt_len <- n_iter*n_tpoints*n_subs
rslts_all <- data.table(rand_vect=integer(length=dt_len),
                        subID = character(length=dt_len),
                        tpoint = integer(length=dt_len),
                        time = numeric(length=dt_len),
                        rand = numeric(length=dt_len))
row <- 1
for (sub in unique(data_dfun[, subID])) {
  
  # update progress 
  print(paste0('Analysing ', sub))
  
  # initializes the progress bar
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = n_iter, # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar
  
  # ## Wrangling data ##
  # # get condition RTs
  # data_beh_sub <- data_beh[Subject == sub & Accuracy == 1 & Block < 13] ## change this if looking at extinction blocks
  # data_beh_sub_av <- data_beh_sub[,
  #                                 .(MeanRT=mean(RT)),
  #                                 by = .(DistractorValue, ResponseRule, TargetPosition)
  # ][order(DistractorValue, -ResponseRule, TargetPosition)]
  # data_beh_sub_av[, ':=' (zMeanRT = scale(MeanRT),
  #                         class = as.integer(paste0(seq(1, 8), '10')))]
  # 
  # put data_dfun into long format
  data_dfun_sub <- data_dfun[subID == sub]
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
  
  # find out how many rows of each class label there are. Note that there should be the same number for all class labels because I equalised the events counts in the MVPA script
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
  
  # loop through random vectors
  for (rand_vect_no in seq(1, n_iter)) {
    
    # set probress bar to current state
    setTxtProgressBar(pb, rand_vect_no)
    
    # get vector
    rand_vect <- cols[rand_vect_no]
    # generate an 8x8 identify matrix
    mat_rand <- diag(1, 8)
    # fill in upper triangle with x_samp
    u_ind <- upper.tri(mat_rand)
    mat_rand[u_ind] <- rand_vect[[1]]
    # symmetrize the matrix using the matrix transpose
    l_ind <- lower.tri(mat_rand)
    mat_rand[l_ind] <- t(mat_rand)[l_ind]
    # check is symmetric
    if (!isSymmetric(mat_rand)) {
      print("Matrix not symmetric!")
      break
    }
  
   # add in random vector 
    rand_vec <-  c(rep(mat_rand[, 1], times=repeats), # 110
                   rep(mat_rand[, 2], times=repeats), # 210
                   rep(mat_rand[, 3], times=repeats), # 310
                   rep(mat_rand[, 4], times=repeats), # 410
                   rep(mat_rand[, 5], times=repeats), # 510
                   rep(mat_rand[, 6], times=repeats), # 610
                   rep(mat_rand[, 7], times=repeats), # 710
                   rep(mat_rand[, 8], times=repeats)) # 810
    data_dfun_long[, rand := rand_vec]
    
    # 1) Loop through time points and run analysis for all data conditions
    for (t in seq(0, n_tpoints - 1)) { # subtract 1 to account for Python indexing starting at 0
      
      # subset data per time point
      data_dfun_t <- data_dfun_long[tpoint == t, ]
      
      # compute model
      # mdl <- lm(dfun ~ stim + resp + rule + val, data = t_dat)
      mdl_t <- lsfit(x=data_dfun_t[, .(stim, resp, rule, val, conj, rand)],
                     y=data_dfun_t[, dfun])
      
      # add betas to data table
      rslts_t <- data.table(
        rand_vect = rand_vect_no,
        subID = sub,
        tpoint=t,
        time = ts[t+1],
        rand = mdl_t$coefficients['rand'])
      
      # append data from current time point to overall data table
      set(rslts_all, 
          i=as.integer(row), 
          j=c('rand_vect', 'subID', 'tpoint', 'time', 'rand'), 
          rslts_t)
      row <- row + 1
    }
  }
}
# close the progress bar
close(pb) 

write_csv(rslts_all, paste0(path_out, 'rslts_random_vectors.csv'))

## Run cluster-based permutation test ------------------------------------------
exclude <- c('sub-12') # 'sub-24' 'sub-07', 'sub-18', 'sub-32'
critical_t <- 2.023 # 38 degrees of freedom
## load data
rslts_cue_rc_by_val_int <- fread(paste0(path_out, 'rslts_cue_rc_by_val_int.csv'))

# write function to extract BFs
extract_t_1samp <- function(x) {
  res <- t.test(x, mu=0, alternative='two.sided')
  t <- as.numeric(res$statistic)
  return(t)
}

# run t-test on empirical data
rslts_cue_rc_by_val_int_t <- rslts_cue_rc_by_val_int[, 
                        lapply(.SD, extract_t_1samp), 
                        .SDcols=c('rule_by_val', 'resp_by_val', 'stim_by_val'),
                        by=c('tpoint', 'time')
][, 
  melt(.SD, id.vars=c('tpoint', 'time'),
        variable.name='effect',
        value.name='t'),
   .SDcols=tpoint:stim_by_val
][, ':=' (abv_thshld = ifelse(abs(t) > critical_t, t, NA))]

# plot t-values 
ggplot(data=rslts_cue_rc_by_val_int_t, 
       aes(x=time, y=t)) + 
  geom_hline(yintercept = 0, linetype=3) +
  geom_hline(yintercept = critical_t, linetype=2) +
  geom_hline(yintercept = -critical_t, linetype=2) +
  geom_line() +
  facet_wrap("effect")

# sum clusters
running_sim <- 0
summer <- function(x) {
  if (is.na(x)) {running_sum <<- 0} 
  else {
    running_sum <<- x + running_sum
  }
  return(running_sum)
}
rslts_cue_rc_by_val_int_t[, 
                          cluster_sums := summer(abv_thshld), 
                          by=1:nrow(rslts_cue_rc_by_val_int_t)]

# find largest absolute cluseter for each effect
rslts_cue_rc_by_val_int_t[, max(abs(cluster_sums)), by='effect']

# run t-tests on random data
rslts_all_t <- rslts_all[, 
                         lapply(.SD, extract_t_1samp), 
                         .SDcols='rand',
                         by=c('rand_vect', 'tpoint')
][, ':=' (abv_thshld = ifelse(abs(rand) > critical_t, rand, NA))]
# sum clusters
rslts_all_t[, 
            cluster_sums := summer(abv_thshld), 
            by=1:nrow(rslts_all_t)]
# generate the permutation distribution
permutation_distribution <- rslts_all_t[, 
                                        .(max_cluster_val=max(abs(cluster_sums))), 
                                        by='rand_vect']
# find p-values of clusters
(sum(permutation_distribution$max_cluster_val > 16.41693)/1000) # rule_by_val interaction
(sum(permutation_distribution$max_cluster_val > 21.05875)/1000) # resp_by_val interaction
(sum(permutation_distribution$max_cluster_val > 75.22756)/1000) # stim_by_val interaction

## plot interactions
# average across subjects
rslts_cue_rc_by_val_int_p <- rslts_cue_rc_by_val_int[!(subID %in% exclude), 
                                                     lapply(.SD, mean), by=c('time'), 
                                                     .SDcols=c('rule_by_val',
                                                               'stim_by_val',
                                                               'resp_by_val' )]
# add significance line
rslts_cue_rc_by_val_int_p[, stim_by_val_cluster := ifelse(time >= 0.610 & time <= 0.703, -0.08, NA)]
  
plot_cue_rc_by_val_int <- ggplot() + 
  
  # add reference lines
  geom_line(data=data.frame(x=c(0, 0), y=c(-0.075, 0.075)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(0.5, 0.5), y=c(-0.075, 0.075)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  geom_line(data=data.frame(x=c(-0.1, 1), y=c(0, 0)),
            aes(x, y), linetype = 3, linewidth=0.8, alpha = 0.4) +
  # add data
  geom_line(data=rslts_cue_rc_by_val_int_p,
            aes(x=time, y=rule_by_val), color='#9467bd',
            linewidth=1,
            linetype=1) +

  geom_line(data=rslts_cue_rc_by_val_int_p,
            aes(x=time, y=stim_by_val), color='#17becf',
            linewidth=1,
            linetype=1) +

  geom_line(data=rslts_cue_rc_by_val_int_p,
            aes(x=time, y=resp_by_val), color='#e377c2',
            linewidth=1,
            linetype=1) +
  
  # add significance line
  geom_line(data=rslts_cue_rc_by_val_int_p,
            aes(x=time, y=stim_by_val_cluster), color='#17becf',
            linewidth=2,
            linetype=1) +
 
  # customise
  scale_y_continuous(name='Beta',
                     breaks=c(-0.1, 0, 0.1),
                     labels=c('-0.1', '0.0', '0.1'),
                     # limits=c(0.375, 1),
                     expand=expansion(mult = 0.1)) +
  scale_x_continuous(name='Time from rule onset (s)',
                     breaks=c(0, 0.3, 0.6, 0.9),
                     labels=c('0.0', '0.3', '0.6', '0.9')) +
  geom_rangeframe(data=data.frame(x=c(0, 0.9), y=c(-0.1, 0.1)),
                  aes(x, y), size=1, color='black') +
  my_theme() + theme(legend.position = 'top')

svg(paste0(path_out, 'fig_cue_rc_int_stim_permutations.svg'),
    width=8, height=4)
plot(plot_cue_rc_by_val_int)
dev.off()


