import mne
import json
import re
import os
import h5py as hp
import polars as pl
import numpy as np 
import scipy as sp 
import sklearn as sk
import seaborn as sb
import matplotlib.pyplot as plt
from mne.decoding import SlidingEstimator, cross_val_multiscore
from sklearn import svm, model_selection
from pathlib import Path
from eeg_fasterAlgorithm import *
%matplotlib qt

# load data
DATAPATH = "C:/Users/cstone/OneDrive - UNSW/Documents/Projects/my_experiments/val_decode/data/" 
EPOCHPATH = 'C:/Users/cstone/OneDrive - UNSW/Documents/Projects/my_experiments/val_decode/preprocessed_data/'
SAVEPATH = 'C:/Users/cstone/OneDrive - UNSW/Documents/Projects/my_experiments/val_decode/output/'
extension = 'cue_rc_avg' # change extension to get epochs with different preprocessing stages
epoch_paths = sorted(Path(EPOCHPATH).glob(f'sub*/*{extension}.fif')) 
epoch_counts = pl.read_csv(EPOCHPATH + f'epochs_{extension}.txt')

# set constants for analysis
freqs = np.geomspace(2, 35, 30)
n_cycles = 3 #np.linspace(3, 10, 30)
analysis_window = [-0.1, 1] # [-1, 0.1] for fdbc
# define bands
delta = freqs < 4
theta = (freqs >= 4) & (freqs < 8)
alpha = (freqs >= 8) & (freqs < 13) 
beta = (freqs >= 13) & (freqs < 30)
gamma = freqs >= 30
# create classifier
svc = svm.LinearSVC(class_weight='balanced', max_iter=2000)
temp_decode = SlidingEstimator(svc, scoring="balanced_accuracy") # extend pipeline over time
skf = model_selection.StratifiedKFold(n_splits=4, shuffle=True) # define cross-validation

# create column names
cols = ["dfun_110", 
        "dfun_210", 
        "dfun_310", 
        "dfun_410", 
        "dfun_510",
        "dfun_610",
        "dfun_710",
        "dfun_810"]

# loop through participants
for path in epoch_paths[0:1]:

    # load data
    subID = path.stem.split('_')[0]
    epochs = mne.read_epochs(path) 

    # compute tfr
    power = epochs.compute_tfr(
        method="morlet",
        freqs=freqs,
        n_cycles=n_cycles,
        average=False)

    # average over frequency bands
    power_data = power.get_data() 
    times = (power.times >= analysis_window[0]) & (power.times <= analysis_window[1])
    power_data = power_data[:, :, :, times] # trim data to times of interest
    power_ind_freqs = [power_data[:, :, band, :].mean(2) 
                        for band in [delta, theta, alpha, beta, gamma]] # average over all freqncies within a band
    
    # normalise data
    z_scrd_lst = []
    for band in power_ind_freqs:
        z_scrd = [sp.stats.zscore(band[:, :, t], axis=1) 
                    for t in range(0, band.shape[2])] # z-score across electodes for each epoch and time point, separately for each freqency band
        z_scrd_lst.append(np.moveaxis(np.array(z_scrd), 0, -1))
    X = np.concatenate(z_scrd_lst, axis=1)

    # reset indicies of class comparisons for pre-trial period
    power.selection = np.arange(0, len(power)) 
    
    ## Run multiclass decoding -------------------------------------------------------
    # create empty data frame to save results
    df_decfun = pl.DataFrame(schema={"dfun_110": float, 
                                     "dfun_210": float,
                                     "dfun_310": float, 
                                     "dfun_410": float,
                                     "dfun_510": float, 
                                     "dfun_610": float,
                                     "dfun_710": float, 
                                     "dfun_810": float,
                                     "y": int,
                                     "tpoint":int,
                                     "score":float,
                                     "subID":str})
    
    # get indicies of classes
    y = power.events[:, 2]

    # run model
    splits = list(skf.split(X, y))
    for train, test in splits:

        for t in range(X.shape[-1]):
            
            svc.fit(X[:, :, t][train], y[train])
            score = svc.score(X[:, :, t][test], y[test])
            #preds = svc.predict(X[:, :, t][test])
            dfun = svc.decision_function(X[:, :, t][test])

            df = pl.DataFrame(dfun, schema=cols)
            df = df.with_columns([(pl.Series(y[test], dtype=int).alias('y')),
                                  (pl.lit(t, dtype=int).alias('tpoint')),
                                  (pl.lit(score, dtype=float).alias('score')),
                                  (pl.lit(subID, dtype=str).alias('subID'))])
            
            # save output to main dataframe
            df_decfun = pl.concat([df_decfun, df])

    # save csv 
    df_decfun.write_csv(SAVEPATH + f'dfun_{extension}_{subID}.csv')

    ## Run binary decoding -----------------------------------------------------------
    # create empty data frame to save results
    df_decacc = pl.DataFrame()
    
    # get indicies of classes
    epochs_to = power['to'].selection # response rule
    epochs_hi = power['hi'].selection # value
    epochs_sle = power['le'].selection # stimulus location
    epochs_rle = power[['to/le', 'aw/ri']].selection # correct response  

    ## evaluate models
    # cue
    y = np.zeros(len(power))
    y[epochs_to] = 1
    scores_cue = cross_val_multiscore(temp_decode, X, y, cv=skf)
    # value
    y = np.zeros(len(power))
    y[epochs_hi] = 1
    scores_val = cross_val_multiscore(temp_decode, X, y, cv=skf)
    # stim position
    y = np.zeros(len(power))
    y[epochs_sle] = 1
    scores_stim = cross_val_multiscore(temp_decode, X, y, cv=skf)
    # correct response
    y = np.zeros(len(power))
    y[epochs_rle] = 1
    scores_resp = cross_val_multiscore(temp_decode, X, y, cv=skf)

    # save output
    avs = [np.array(comp).mean(0)[np.newaxis, :] 
           for comp in [scores_cue, scores_val, scores_stim, scores_resp]]
    columns = [f'T{t}' for t in np.arange(0, len(epochs.times[times]))]
    df = pl.DataFrame(np.concatenate(avs), schema=columns)     
    comparisons = [
        'scores_cue', 
        'scores_val',
        'scores_stim',
        'scores_resp'] 
    df = df.with_columns([(pl.Series(comparisons).alias('Comparisons')),
                          (pl.lit(subID).alias('subID'))])
    df_decacc = pl.concat([df_decacc, df])

    # save csv
    df_decacc.write_csv(SAVEPATH + f'dcd_{extension}_{subID}.csv')

    ## Get behavioural data -------------------------------------------------------
    beh_path =  sorted(Path(DATAPATH).glob(f'**/{subID}*beh.txt')) 
    beh = pl.read_csv(beh_path[0], separator='\t')
    beh = beh.with_columns(
        pl.when(pl.col('Accuracy') == 1).then(0).otherwise(1).alias('Error')
        ).filter(pl.col('Accuracy') != 999)
    dat_av = beh.group_by('ResponseRule', 'DistractorValue'
                          ).agg(MeanRT=pl.col('RT').mean(),
                                MeanEr=pl.col('Error').mean())
    ## Create plot ----------------------------------------------------------------------
    fig, axs = plt.subplot_mosaic(
        [["a", "b", "c"],
         ["d", "d", "d"],
         ["e", "e", "e"]],
         layout='constrained')
    # response time
    axs['a'].plot([0, 1], 
            [dat_av.filter((pl.col('DistractorValue')=='low') & (pl.col('ResponseRule')=='A')).select('MeanRT').item(),
             dat_av.filter((pl.col('DistractorValue')=='low') & (pl.col('ResponseRule')=='T')).select('MeanRT').item()],
             color='blue', label='low', marker='o')
    axs['a'].plot([0, 1], 
            [dat_av.filter((pl.col('DistractorValue')=='high') & (pl.col('ResponseRule')=='A')).select('MeanRT').item(),
             dat_av.filter((pl.col('DistractorValue')=='high') & (pl.col('ResponseRule')=='T')).select('MeanRT').item()],
             color='orange', label='high', marker='o')
    axs['a'].set_xticks(ticks=[0, 1], 
                  labels=['A', 'T'])
    axs['a'].set_xlabel('Response Rule')
    axs['a'].set_ylabel('Response time (s)')
    axs['a'].legend()
    # error rate
    axs['b'].plot([0, 1], 
            [dat_av.filter((pl.col('DistractorValue')=='low') & (pl.col('ResponseRule')=='A')).select('MeanEr').item(),
             dat_av.filter((pl.col('DistractorValue')=='low') & (pl.col('ResponseRule')=='T')).select('MeanEr').item()],
             color='blue', label='low', marker='o')
    axs['b'].plot([0, 1], 
            [dat_av.filter((pl.col('DistractorValue')=='high') & (pl.col('ResponseRule')=='A')).select('MeanEr').item(),
             dat_av.filter((pl.col('DistractorValue')=='high') & (pl.col('ResponseRule')=='T')).select('MeanEr').item()],
             color='orange', label='high', marker='o')
    axs['b'].set_xticks(ticks=[0, 1], 
                  labels=['A', 'T'])
    axs['b'].set_xlabel('Response Rule')
    axs['b'].set_ylabel('Error rate')
    axs['b'].legend()
    # epoch count
    axs['c'].bar(x=epoch_counts.filter(pl.col('subID')==subID).columns[2:10], 
                 height=epoch_counts.filter(pl.col('subID')==subID).select(epoch_counts.columns[2:10]).row(0))
    axs['c'].bar_label(axs['c'].containers[0], 
                       labels=epoch_counts.filter(pl.col('subID')==subID).select(epoch_counts.columns[2:10]).row(0),
                       label_type='center')
    axs['c'].tick_params(axis='x', labelrotation=90)
    axs['c'].legend()
    # multi-class
    dec_mean = df_decfun.group_by('tpoint').agg(pl.col("score").mean()).sort('tpoint')
    axs['d'].hlines(0.125, xmin=-0.1, xmax=1, color='grey', linestyle='--', alpha=0.5)
    axs['d'].plot(power.times[times], dec_mean['score'], label='trial_type')
    axs['d'].set_ylabel('Decoding accuracy %')
    axs['d'].set_yticks(ticks=[0.1, 0.15, 0.2, 0.25, 0.3], labels=[10, 15, 20, 25, 30])
    axs['d'].set_xlabel('Time from cue onset (s)')
    axs['d'].set_xticks([0, 0.3, 0.6, 0.9])
    axs['d'].legend()
    # binary
    axs['e'].hlines(.5, xmin=-0.1, xmax=1, color='grey', linestyle='--', alpha=0.5)
    axs['e'].plot(power.times[times], scores_cue.mean(0), label = 'Cue')
    axs['e'].plot(power.times[times], scores_val.mean(0), label = 'Value')
    axs['e'].plot(power.times[times], scores_stim.mean(0), label = 'Stimulus')
    axs['e'].plot(power.times[times], scores_resp.mean(0), label = 'Response')
    # axs['e'].plot(power.times[times], df_decacc.filter(pl.col('Comparisons')=='scores_cue').row(0)[0:-2], label = 'Cue')
    # axs['e'].plot(power.times[times], df_decacc.filter(pl.col('Comparisons')=='scores_val').row(0)[0:-2], label = 'Value')
    # axs['e'].plot(power.times[times], df_decacc.filter(pl.col('Comparisons')=='scores_stim').row(0)[0:-2], label = 'Stimulus')
    # axs['e'].plot(power.times[times], df_decacc.filter(pl.col('Comparisons')=='scores_resp').row(0)[0:-2], label = 'Response')
    axs['e'].set_ylabel('Decoding accuracy %')
    axs['e'].set_yticks(ticks=[0.5, 0.6, 0.7], labels=[50, 60, 70])
    axs['e'].set_xlabel('Time from cue onset (s)')
    axs['e'].set_xticks([0, 0.3, 0.6, 0.9])
    axs['e'].legend()
    # save
    plt.gcf().set_size_inches(10, 10)
    fig.savefig(SAVEPATH + f'plt_{extension}_{subID}.png', dpi=400, )



# fig, ax = plt.subplots()
# # ax.vlines(0, ymin=, ymax=0.75, color='grey', linestyle='--', alpha=0.5)
# ax.hlines(0.125, xmin=-0.1, xmax=1, color='grey', linestyle='--', alpha=0.5)
# ax.plot(power.times[times], dec_mean['score'], label='trial_type')
# ax.set_ylabel('Decoding accuracy %')
# ax.set_yticks(ticks=[0.1, 0.15, 0.2, 0.25, 0.3], labels=[10, 15, 20, 25, 30])
# ax.set_xlabel('Time from cue onset (s)')
# ax.set_xticks([0, 0.3, 0.6, 0.9])
# fig.legend(loc=[0.25, 0.6])

# fig, ax = plt.subplots()
# ax.vlines([0.0], ymin=0.45, ymax=0.75, color='grey', linestyle='--', alpha=0.5)
# ax.hlines(.5, xmin=-0.1, xmax=1, color='grey', linestyle='--', alpha=0.5)
# ax.plot(power.times[times], scores_cue.mean(0), label = 'Cue')
# ax.plot(power.times[times], scores_val.mean(0), label = 'Value')
# ax.plot(power.times[times], scores_stim.mean(0), label = 'Stimulus')
# ax.plot(power.times[times], scores_resp.mean(0), label = 'Response')
# ax.set_ylabel('Decoding accuracy %')
# ax.set_yticks(ticks=[0.5, 0.6, 0.7], labels=[50, 60, 70])
# ax.set_xlabel('Time from cue onset (s)')
# ax.set_xticks([0, 0.3, 0.6, 0.9])
# # ax.set_xlabel('Time from response (s)')
# # ax.set_xticks([-1.6, -1.2, -0.8, -0.4, 0])
# fig.legend(loc=[0.25, 0.6])


dat = pl.read_csv(SAVEPATH + f'dfun_cue_rc_avg_sub-01.csv')