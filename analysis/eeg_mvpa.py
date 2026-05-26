## Notes: This script takes preprocessed, epoched EEG data and raw behavioural data and performs the following:
# 1) Drops bad epochs and those that are excluded in the behavioural analysis, equalises the event counts 
# per trial/epoch type, and creates averages of 4 epochs of the same trial/epoch type to increase SNR in the decoding analyses
# 2) Multi-class decoding to extract decision-functions per trial,  time point, and subject
# 3) Binary decoding separaetly for high- and low-value trials per time point and subject
## Input data can be found in "preprocessed_EEG_data" and "raw_behavioural_data.csv", at https://osf.io/2x3a8/files/osfstorage
## Output data can be found in "decode_by_value_results_files.zip" and "multiclass_decoding_results_files.zip", found at the above link. 

import mne
import re
import polars as pl
import numpy as np 
import scipy as sp 
from mne.decoding import SlidingEstimator, cross_val_multiscore
from sklearn import svm, model_selection
from pathlib import Path
%matplotlib qt

# load data
EPOCHPATH = ''
SAVEPATH = ''
extension = 'cue'
epoch_paths = sorted(Path(EPOCHPATH).glob(f'sub*/*{extension}.fif')) 
beh_all = pl.read_csv('raw_behavioural_data.csv', separator=',')

## set constants for analysis
freqs = np.geomspace(2, 35, 30)
n_cycles = 3 
analysis_window = [-0.1, 1] 
# define bands
delta = freqs < 4
theta = (freqs >= 4) & (freqs < 8)
alpha = (freqs >= 8) & (freqs < 13) 
beta = (freqs >= 13) & (freqs < 30)
gamma = freqs >= 30
# create classifier
svc = svm.LinearSVC(max_iter=2000) 
temp_decode = SlidingEstimator(svc, scoring="accuracy") # extend pipeline over time
skf = model_selection.StratifiedKFold(n_splits=4, shuffle=True) # define cross-validation
# create column names for output DF
cols = ["dfun_110", 
        "dfun_210", 
        "dfun_310", 
        "dfun_410", 
        "dfun_510",
        "dfun_610",
        "dfun_710",
        "dfun_810"]

## loop through participants for analysis
for path in epoch_paths:

    ## load, clean and subset data
    subID = path.stem.split('_')[0]
    if subID == 'sub-12': ## exclude sub-12
        continue
    epochs = mne.read_epochs(path) 
    # load behavioural data  
    beh = beh_all.filter(pl.col("Subject") == subID)
    # deal with missing trials for sub-03
    if subID == 'sub-03':
        beh = beh.filter(~pl.col('RunningTrialNo').is_in([361, 362, 363]))
        beh = beh.with_columns(pl.Series(np.arange(0, len(beh))).alias("RunningTrialNo2"))
    else: # re-set running trial number to account for python indexing starting at 0
        beh = beh.with_columns((pl.col("RunningTrialNo") - 1).alias("RunningTrialNo2")) 
    # extract trial number of trials to exlcude
    filt_idx = beh.filter((pl.col('Accuracy') != 1) | 
                          (pl.col('RT') < 0.15)
                          ).select('RunningTrialNo2').to_series().to_list()
    # load bad epochs txt file
    bad_epochs_path = sorted(Path(EPOCHPATH).glob(f'**/{subID}*bad_epochs.txt')) 
    with open(bad_epochs_path[0], 'r') as file:
        bads_str = file.readlines()
    bads = re.findall(r"-?\d*\.?\d+", bads_str[0])
    bads = list(map(int, bads)) # convert to list of ints
    # combine bad epochs with list of trials to exclude, remove duplicates
    exclude = list(set(filt_idx + bads)) 
    # drop epochs 
    epochs.drop(exclude)
    # remove corresponding epochs from the behavioural data file
    beh_cleaned = beh.filter(~pl.col('RunningTrialNo2').is_in(exclude))
    # add new column to behavioural data file with new index
    beh_cleaned = beh_cleaned.with_columns(pl.Series('NewTrialIndex', np.arange(0, len(beh_cleaned))))
    # subset data
    beh_rc = beh_cleaned.filter(pl.col('Block') < 13) # reward contingency phase
    beh_ex = beh_cleaned.filter(pl.col('Block') >= 13) # extinction phase
    rc_idx = beh_rc.select('NewTrialIndex').to_series().to_list()
    ex_idx = beh_ex.select('NewTrialIndex').to_series().to_list()
    epochs_rc = epochs[rc_idx]
    epochs_ex = epochs[ex_idx]

    # equalise event counts
    epochs_rc, rc_idx_drpd = epochs_rc.equalize_event_counts(method='random', random_state=int('1234' + subID.split('-')[1])) 
    epochs_ex, ex_idx_drpd = epochs_ex.equalize_event_counts(method='random', random_state=int('1234' + subID.split('-')[1]))   
    # remove the equalised event count dropped epochs from the behavioural data
    beh_rc = beh_rc.with_columns(pl.Series('NewTrialIndex', np.arange(0, len(beh_rc)))
                                 ).filter(~pl.col("NewTrialIndex").is_in(rc_idx_drpd))
    beh_ex = beh_ex.with_columns(pl.Series('NewTrialIndex', np.arange(0, len(beh_ex)))
                                ).filter(~pl.col("NewTrialIndex").is_in(ex_idx_drpd))
    # add final reset index 
    beh_rc = beh_rc.with_columns(pl.Series('NewTrialIndex', np.arange(0, len(beh_rc))))
    beh_ex = beh_ex.with_columns(pl.Series('NewTrialIndex', np.arange(0, len(beh_ex))))
    # add new columns to behavioural datafile for later
    beh_rc = beh_rc.with_columns(event_type=pl.lit(999), 
                                 event_group=pl.lit(999))
    beh_ex = beh_ex.with_columns(event_type=pl.lit(999), 
                                 event_group=pl.lit(999))

    # create averaged epochs
    phase = "ex" #NOTE: set this to be either "rc" (reward contingency) or "ex" (extinction phase); and comment out relevant line below
    #epochs_av = epochs_rc.copy() 
    epochs_av = epochs_ex.copy()
    n_trials = 4 # number of trials to average
    epochs_av.selection = np.arange(0, len(epochs_av)) # renumber epochs to start at 0
    eeg_events_array = [] # create empty list for new events array
    epoch_array = [] # create empty list to store averaged epochs
    df_epoch_array = pl.DataFrame(schema={"event_type": float,
                                          "event_group": float,
                                          "RT": float})
    for event_type in epochs_av.event_id.keys():
        idxs = epochs_av[event_type].selection # find indicies of epochs that belong to the event type
        beh_rc[idxs, 'event_type'] = epochs_av.event_id[event_type]
        n_epochs = int(np.ceil(len(idxs) / n_trials))
        for epoch in range(n_epochs):
            eeg_events_array.append(epochs_av.event_id[event_type]) # add correct number of events to events array
        group = 0
        while len(idxs) > n_trials: # loop through epochs to extract as many averages as we can 
            this_selection = np.random.choice(idxs, 
                                              size=n_trials, 
                                              replace=False) # radomly select epochs of the same type to average
            av_epoch = epochs_av[this_selection].average(method='mean').get_data() # average epochs
            epoch_array.append(av_epoch[np.newaxis, :]) # save to list
            bool_array = list(map(lambda x: x not in this_selection, idxs)) # update idxs list to remove the epochs we just averaged together
            idxs = idxs[bool_array]
            # update behavioural datafiles
            beh_rc[this_selection, 'event_group'] = group
            df_epoch_array = pl.concat([df_epoch_array,
                                        beh_rc[this_selection].mean()['event_type', 'event_group', "RT"]
                                        ])
            group += 1
        # average remaining epochs
        av_epoch = epochs_av[idxs].average(method='mean').get_data() # average remaining epochs together
        epoch_array.append(av_epoch[np.newaxis, :])
        beh_rc[idxs, 'event_group'] = group
        df_epoch_array = pl.concat([df_epoch_array,
                            beh_rc[idxs].mean()['event_type', 'event_group', "RT"]
                            ])
    epoch_array = np.concatenate(epoch_array, axis=0)
    dim1 = np.linspace(0, 
                    (np.abs(epochs_av.tmin) + epochs_av.tmax)*1000*len(epoch_array), 
                    len(eeg_events_array), 
                    endpoint=False, 
                    dtype=int)
    dim2 = np.zeros(len(epoch_array))
    eeg_events_array = np.stack([dim1, dim2, eeg_events_array], 
                                axis=1)
    info = mne.create_info(epochs_av.info.ch_names[0:64], 
                            epochs_av.info['sfreq'], 
                            ch_types='eeg')
    epoch_grp = mne.EpochsArray(data=epoch_array, 
                                info=info, 
                                events=eeg_events_array.astype(int), 
                                tmin=epochs_av.tmin, 
                                event_id=epochs_av.event_id)

    # save new behavioural data files
    #beh_rc.write_csv(SAVEPATH + f'beh_rc_{subID}.csv')
    beh_ex.write_csv(SAVEPATH + f'beh_ex_{subID}.csv') # Note: change this depending on what phase data you are looking at 

    ## begin decoding analysis 
    # compute tfr
    power = epoch_grp.compute_tfr(
        method="morlet",
        freqs=freqs,
        n_cycles=n_cycles,
        average=False)

    # average over frequency bands
    power_data = power.get_data() 
    times = (power.times >= analysis_window[0]) & (power.times <= analysis_window[1])
    power_data = power_data[..., times] # trim data to times of interest
    power_ind_freqs = [power_data[:, :, band, :].mean(2) 
                        for band in [delta, theta, alpha, beta, gamma]] # average over all freqncies within a band
    
    # normalise data
    z_scrd_lst = []
    for band in power_ind_freqs:
        z_scrd = [sp.stats.zscore(band[:, :, t], axis=1) 
                    for t in range(0, band.shape[2])] # z-score across electodes for each epoch and time point, separately for each freqency band
        z_scrd_lst.append(np.moveaxis(np.array(z_scrd), 0, -1))
    
    # get data
    X = np.concatenate(z_scrd_lst, axis=1)
    # get indicies of classes
    power.selection = np.arange(0, len(power))
    y = power.events[:, 2]

    # Run multiclass decoding -------------------------------------------------------
    # create empty data frame to save results
    df_decfun = pl.DataFrame(schema={"dfun_110": float, 
                                     "dfun_210": float,
                                     "dfun_310": float, 
                                     "dfun_410": float,
                                     "dfun_510": float, 
                                     "dfun_610": float,
                                     "dfun_710": float, 
                                     "dfun_810": float,
                                     "event_type": float,
                                     "event_group": float,
                                     "RT": float,
                                     "y": int,
                                     "tpoint":int,
                                     "score":float,
                                     "subID":str,
                                     "cv_split":float})
    
    # run model
    for split in np.arange(1, 9):
        # generate new cv folds
        splits = list(skf.split(X, y))
        for train, test in splits:
            # run per time sample
            for t in range(X.shape[-1]):
                # fit model
                svc.fit(X[:, :, t][train], y[train])
                score = svc.score(X[:, :, t][test], y[test])
                dfun = svc.decision_function(X[:, :, t][test]) 
                # put results into dataframe
                df = pl.DataFrame(dfun, schema=cols)
                df = df.with_columns([(pl.Series(df_epoch_array[test]['event_type']).alias('event_type')),
                                      (pl.Series(df_epoch_array[test]['event_group']).alias('event_group')),
                                      (pl.Series(df_epoch_array[test]['RT']).alias('RT')),
                                      (pl.Series(y[test], dtype=int).alias('y')),
                                      (pl.lit(t, dtype=int).alias('tpoint')),
                                      (pl.lit(score, dtype=float).alias('score')),
                                      (pl.lit(subID, dtype=str).alias('subID')),
                                      (pl.lit(split, dtype=float).alias('cv_split'))])
                # save output to main dataframe
                df_decfun = pl.concat([df_decfun, df])

    # save csv 
    df_decfun.group_by(["event_type", "event_group", "y", "tpoint", "subID"]).mean() # average over 8 iterations of CV
    df_decfun.write_csv(SAVEPATH + f'dfun_{extension}_{phase}_{subID}.csv')

    ## Run binary decoding by value ---------------------------------------------------------
    # create empty data frame to save results
    df_decacc = pl.DataFrame()
    
    for value in ['hi', 'lo']:

        # get indicies of classes
        epochs_val = power[value].selection # value
        epochs_to = power[f'{value}/to'].selection # response rule
        epochs_sle = power[f'{value}/le'].selection # target location
        epochs_rle = power[[f'{value}/to/le', f'{value}/aw/ri']].selection # correct response   

        ## run models
        X_val = X[epochs_val]
        # rule cue
        y_val = np.isin(epochs_val, epochs_to).astype(int)
        scores_cue = cross_val_multiscore(temp_decode, X_val, y_val, cv=skf)
        # scores_cue_tg = cross_val_multiscore(temp_gen, X_temp, y, cv=skf)
        # target location
        y_val = np.isin(epochs_val, epochs_sle).astype(int)
        scores_stim = cross_val_multiscore(temp_decode, X_val, y_val, cv=skf)
        # correct response
        y_val = np.isin(epochs_val, epochs_rle).astype(int)
        scores_resp = cross_val_multiscore(temp_decode, X_val, y_val, cv=skf)

        # save output
        avs = [np.array(comp).mean(0)[np.newaxis, :] 
            for comp in [scores_cue, scores_stim, scores_resp]]
        columns = [f'T{t}' for t in np.arange(0, len(epochs.times[times]))]
        df = pl.DataFrame(np.concatenate(avs), schema=columns)     
        comparisons = [
            'scores_cue', 
            'scores_stim',
            'scores_resp'] 
        df = df.with_columns([(pl.lit(value).alias('Value')),
                                (pl.Series(comparisons).alias('Comparisons')),
                                (pl.lit(subID).alias('subID'))])
        df_decacc = pl.concat([df_decacc, df])

    # save csv
    df_decacc.write_csv(SAVEPATH + f'dcd_by_val_{extension}_{phase}_{subID}.csv')
    