library(tidyverse)
library(data.table)
library(stringr)
library(ggplot2)
library(magrittr)
library(randtests)

# import data
data_path <- "C:/Users/cstone/OneDrive - UNSW/Documents/Projects/my_experiments/val_decode/data/"
files <- list.files(data_path, pattern = '.*(beh.txt)', recursive = T)
subs <- unique(str_split_i(files, "/", 1))
# data <- read.table(file.path(data_path, files), header=T)
dt <- rbindlist(lapply(file.path(data_path, files), fread), fill = TRUE)
# setDT(data)
unique(data[, StimulusColour])

# check for randomness of trial type sequence
dt[, TType := case_when((StimulusValue == 'high' & ResponseRule == 'T' & StimulusPosition == 'left') ~ 1,
                        (StimulusValue == 'high' & ResponseRule == 'T' & StimulusPosition == 'right') ~ 2,
                        (StimulusValue == 'high' & ResponseRule == 'A' & StimulusPosition == 'left') ~ 3,
                        (StimulusValue == 'high' & ResponseRule == 'A' & StimulusPosition == 'right') ~ 4,
                        (StimulusValue == 'low' & ResponseRule == 'T' & StimulusPosition == 'left') ~ 5,
                        (StimulusValue == 'low' & ResponseRule == 'T' & StimulusPosition == 'right') ~ 6,
                        (StimulusValue == 'low' & ResponseRule == 'A' & StimulusPosition == 'left') ~ 7,
                        (StimulusValue == 'low' & ResponseRule == 'A' & StimulusPosition == 'right') ~ 8
                        )]
randtests::runs.test(dt[Subject=='sub-02', TType])


dat <- dt[Subject=='sub-01' & RT <= 1,
            .(MeanRT = mean(RT), 
              MeanAcc = mean(Accuracy),
              .N), 
            by=.(StimulusColour, StimulusValue, StimulusPosition, ResponseRule)][
       order(StimulusValue, StimulusPosition, ResponseRule)]

dt[RT <= 1,
     .(MeanRT = mean(RT), 
       MeanAcc = mean(Accuracy),
       .N), 
     by=.(StimulusValue, ResponseRule)][
       order(StimulusValue, ResponseRule)] %>% 
  ggplot() +
  geom_point(aes(x=ResponseRule, y=MeanAcc, color=StimulusValue)) + 
  scale_y_continuous(limits = c(0, 1))



data[Block > 12,
     .N,
     by=.(StimulusColour, StimulusValue, StimulusPosition, ResponseRule)][
       order(StimulusValue, StimulusPosition, ResponseRule)]




# check timing in log files
data_path <- "C:/Users/cstone/OneDrive - UNSW/Documents/Projects/my_experiments/val_decode/data/"
files <- list.files(data_path, pattern = '.*(log.txt)', recursive = T)
subs <- unique(str_split_i(files, "/", 1))
data <- read.table(file.path(data_path, files), sep='\t', header=F, quote = "", row.names = NULL, stringsAsFactors = FALSE)
setDT(data)
data <- data[, -c('V2')]
data <- data[V3 %in% c('START_CUE', 'START_SOA', 'START_TAR', 'START_RES', 'START_FDB', 'RESPONSE'), ]
diffs <- diff(data$V1, 1)
data[2:nrow(data), diff := diffs]
data$V3 <- as.factor(data$V3)
ggplot(data, aes(diff, V3)) + geom_boxplot()
ggplot(data = data[V3 == 'START_FDB' & diff < 0.1, ], aes(diff)) + geom_boxplot()

`%ni%` <- Negate(`%in%`)
data[V3 %ni% c('START_CUE', 'RESPONSE'), ]
ggplot(data[V3 %ni% c('START_CUE', 'RESPONSE'), ], aes(diff, V3)) + geom_boxplot()

