import polars as pl
import mne
import json
import numpy as np 
import scipy as sp 
import sklearn as sk
import re
import os
from sklearn import preprocessing, pipeline, model_selection, discriminant_analysis
from pathlib import Path
from eeg_fasterAlgorithm import *
import matplotlib.pyplot as plt
%matplotlib qt

## set data paths
DATAPATH = "C:/Users/cstone/OneDrive - UNSW/Documents/Projects/my_experiments/val_decode/data/" 
SAVEPATH = "C:/Users/cstone/OneDrive - UNSW/Documents/Projects/my_experiments/val_decode/preprocessed_data_allTrials/"
srcDataEEG = sorted(Path(DATAPATH).glob('**/*.bdf'))
srcDataBeh = sorted(Path(DATAPATH).glob('**/*beh.txt'))
srcDataFrms = sorted(Path(DATAPATH).glob('**/*frms.txt'))

for path in srcDataEEG[39:40]:

    # extract some BIDS info
    subID, task, modality = path.stem.split('_')

    # load data
    raw = mne.io.read_raw_bdf(
        path,
        preload=True)
    
    # load behavioural data file
    beh_path =  sorted(Path(DATAPATH).glob(f'**/{subID}*beh.txt')) 
    beh = pl.read_csv(beh_path[0], separator='\t')
    # idx = beh.filter((pl.col('Accuracy') != 1) #| (pl.col('RT') < 0.15)
    #                  ).select('RunningTrialNo').to_numpy()
    # remove trials missed in the EEG recording for this subject 
    if subID == 'sub-03':
        beh = beh.filter(~pl.col('RunningTrialNo').is_in([361, 362, 363]))
    
    # set montage
    raw.set_montage(
        'biosemi64', 
        on_missing='warn')

    # add EOG channels
    info = mne.create_info( 
        ch_names=['hEOG','vEOG'], 
        sfreq=raw.info['sfreq'], 
        ch_types='eog'
        )
    RH, LH, LV, UV = mne.pick_channels(
        raw.ch_names, 
        ['EXG5', 'EXG6', 'EXG7', 'EXG8']
        )
    hEOG = raw[LH][0] - raw[RH][0]
    vEOG = raw[UV][0] - raw[LV][0]
    newEOG = mne.io.RawArray(
        np.concatenate([hEOG, vEOG]), 
        info=info)
    raw.add_channels(
        add_list=[newEOG], 
        force_update_info=True)
    raw.drop_channels(
        ch_names=['EXG1', 'EXG2', 'EXG3', 'EXG4', 
                'EXG5', 'EXG6', 'EXG7', 'EXG8'])

    # find events
    eeg_events = mne.find_events(raw, 
        initial_event=True, 
        consecutive=True, 
        output='onset', 
        shortest_event=1)
    
    # # modify events to eliminate errors 
    # for i, j in enumerate(eeg_events[:, 2]): 
    #     if j in [150, 190, 250, 290, 350, 390, 450, 490, 550, 590, 650, 690, 750, 790, 850, 890]: # errors
    #         # if (eeg_events[i - 4, 2] in [110, 210, 310, 410, 510, 610, 710, 810]):
    #         #     eeg_events[i - 4, 2] = 999 #error
    #         if (eeg_events[i - 3, 2] in [110, 210, 310, 410, 510, 610, 710, 810]):
    #             eeg_events[i - 3, 2] = 999 #error or  miss

    # # check this worked
    # sum(eeg_events[:, 2] == 999) == len(idx) ## this might not work if exlcude RTs < 0.15 s

    # create event dictionary
    event_dict = {
            "hi/to/le/iti": 100,
            "hi/to/le/cue": 110,
            "hi/to/le/soa": 120,
            "hi/to/le/tar": 130,
            "hi/to/le/fdbc": 140,
            "hi/to/le/fdbi": 150,
            "hi/to/le/fdbm": 190,
            "hi/to/ri/iti": 200,
            "hi/to/ri/cue": 210,
            "hi/to/ri/soa": 220,
            "hi/to/ri/tar": 230,
            "hi/to/ri/fdbc": 240,
            "hi/to/ri/fdbi": 250,
            "hi/to/ri/fdbm": 290,
            "hi/aw/le/iti": 300,
            "hi/aw/le/cue": 310,
            "hi/aw/le/soa": 320,
            "hi/aw/le/tar": 330,
            "hi/aw/le/fdbc": 340,
            "hi/aw/le/fdbi": 350,
            "hi/aw/le/fdbm": 390,
            "hi/aw/ri/iti": 400,
            "hi/aw/ri/cue": 410,
            "hi/aw/ri/soa": 420,
            "hi/aw/ri/tar": 430,
            "hi/aw/ri/fdbc": 440,
            "hi/aw/ri/fdbi": 450,
            "hi/aw/ri/fdbm": 490,
            "lo/to/le/iti": 500,
            "lo/to/le/cue": 510,
            "lo/to/le/soa": 520,
            "lo/to/le/tar": 530,
            "lo/to/le/fdbc": 540,
            "lo/to/le/fdbi": 550,
            "lo/to/le/fdbm": 590,
            "lo/to/ri/iti": 600,
            "lo/to/ri/cue": 610,
            "lo/to/ri/soa": 620,
            "lo/to/ri/tar": 630,
            "lo/to/ri/fdbc": 640,
            "lo/to/ri/fdbi": 650,
            "lo/to/ri/fdbm": 690,
            "lo/aw/le/iti": 700,
            "lo/aw/le/cue": 710,
            "lo/aw/le/soa": 720,
            "lo/aw/le/tar": 730,
            "lo/aw/le/fdbc": 740,
            "lo/aw/le/fdbi": 750,
            "lo/aw/le/fdbm": 790,
            "lo/aw/ri/iti": 800,
            "lo/aw/ri/cue": 810,
            "lo/aw/ri/soa": 820,
            "lo/aw/ri/tar": 830,
            "lo/aw/ri/fdbc": 840,
            "lo/aw/ri/fdbi": 850,
            "lo/aw/ri/fdbm": 890
        }

    # # plot events
    # mne.viz.plot_events(eeg_events, 
    #                     event_id=event_dict,
    #                     on_missing='warn',
    #                     sfreq=raw.info['sfreq'])

    # # check event counts
    # for key, val in event_dict.items():
    #     ev_mask = eeg_events[:, 2] == val
    #     count = ev_mask.sum()
    #     print(f'{key}: {count}')

    # mark breaks in data collection
    break_annots = mne.preprocessing.annotate_break(
        raw=raw,
        events=eeg_events,
        min_break_duration=5,  # consider segments of at least 5 s duration
        t_start_after_previous=2,  # start annotation 3 s after end of previous one
        t_stop_before_next=2,  # stop annotation 3 s before beginning of next one
    ) 
    raw.set_annotations(break_annots)

    # plot raw data
    # raw.plot()

    ## Start actual pre-processing ---------------------------------------------------------

    # filter EEG data
    filt_h = raw.copy().filter(
        l_freq=None, 
        h_freq=40, 
        picks='eeg')

    # identify bad chans
    filt_h.info['bads'] += faster_bad_channels(filt_h, 
                                            use_metrics=['correlation',
                                                         'variance',
                                                         'hurst'])

    # check filtered data to make sure no bad channels were missed and mark bad sections
    # filt_h.plot()

    # handle bad channels
    intrp = filt_h.copy().interpolate_bads()   

    # re-reference to average 
    avRef = intrp.set_eeg_reference()

    # create two copies of data, one for ICA and one for continued preprocessing
    ica_data = avRef.copy().filter(l_freq=1.0, h_freq=None) # use unfiltered data to apply higher high-pass filter
    filt_l = avRef.copy().filter(l_freq=0.1, h_freq=None, picks='eeg')

    # run ICA
    ICA = mne.preprocessing.ICA() 
    ICA.fit(ica_data, decim=8) # speed up processing

    # check ICA components on filtered data
    # ICA.plot_sources(filt_l, show_scrollbars=False)
    # ICA.plot_components()
    # automatically find the ICs that best match the EOG signal
    eog_indices, eog_scores = ICA.find_bads_eog(filt_l)
    print(f'EOG indicies: {eog_indices}')
    ICA.exclude = eog_indices

    # apply ICA 
    ICA.apply(filt_l)
    # filt_l.plot()

    # # crop data into reward and extinction segments
    # block13_samp = eeg_events[:, 2] == 913 # block 13
    # block13_time = eeg_events[block13_samp][0, 0]/raw.info['sfreq']
    # filt_rc = filt_l.copy().crop(tmin=0, tmax=block13_time, include_tmax=False)
    # filt_ex = filt_l.copy().crop(tmin=block13_time, tmax=None, include_tmax=True)

    # # create dictionary for epoching
    # seg_dict = {
    #     'rc': filt_rc,
    #     'ex': filt_ex}

    # # create epochs from data segments
    # for segment in seg_dict.keys():

    # cycle through different triggers
    for trigger in ['cue', 'fdbc']:
        
        # set epoch constants
        if trigger == 'cue':
            tmin = -1
            tmax = 2.7
            baseline = (-0.2, 0)
        else:
            tmin = -2.7
            tmax = 1
            baseline = (-2.7, -2.5)

        # create epochs
        epochs = mne.Epochs(
            raw=filt_l,
            events=eeg_events,
            event_id=event_dict,
            tmin=tmin,
            tmax=tmax,
            baseline=baseline,
            decim=filt_l.info['sfreq']/256,
            preload=True, 
            reject=None, 
            flat=None,
            reject_by_annotation=False,
            on_missing='warn')
        epochs_trig = epochs[trigger]
        # epochs_trig.selection = np.arange(0, len(epochs))

        # check the number of epochs matches the behavioural data
        if len(epochs_trig) != len(beh):
            print("Number of epochs does not match number of behavioural trials")
            break

        # remove bad epochs
        bads_epochs = faster_bad_epochs(epochs_trig)
        #epochs_trig.drop(bads_epochs)

        # save epochs
        epoch_dict = {
        'subID': subID,
        'hi_to_le': len(epochs_trig['hi/to/le']),
        'hi_to_ri': len(epochs_trig['hi/to/ri']),
        'hi_aw_le': len(epochs_trig['hi/aw/le']),
        'hi_aw_ri': len(epochs_trig['hi/aw/ri']),
        'lo_to_le': len(epochs_trig['lo/to/le']),
        'lo_to_ri': len(epochs_trig['lo/to/ri']),
        'lo_aw_le': len(epochs_trig['lo/aw/le']),
        'lo_aw_ri': len(epochs_trig['lo/aw/ri']),
        'n_bd_chns': len(filt_h.info['bads'])}
        df = pl.from_dict(epoch_dict)
        if not os.path.exists(SAVEPATH + f'epochs_{trigger}.txt'):
            df.write_csv(SAVEPATH + f'epochs_{trigger}.txt')
        else:
            df_ld = pl.read_csv(SAVEPATH + f'epochs_{trigger}.txt')
            df_ld.vstack(df, in_place=True)
            df_ld.write_csv(SAVEPATH + f'epochs_{trigger}.txt')
        # save cue epoch files
        if not os.path.exists(SAVEPATH + subID):
            os.mkdir(SAVEPATH + subID)
        FILENAME = f'/{subID}_{task}_{modality}_epochs_{trigger}.fif'
        epochs_trig.save(SAVEPATH + subID + FILENAME)
        with open(SAVEPATH + subID + f"/{subID}_bad_epochs.txt", "w") as output:
            output.write(str(bads_epochs))
