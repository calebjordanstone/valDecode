import polars as pl
import mne
import json
import numpy as np 
import scipy as sp 
import sklearn as sk
from sklearn import preprocessing, pipeline, model_selection, discriminant_analysis
from pathlib import Path
import matplotlib.pyplot as plt
%matplotlib qt


## set data paths
DATAPATH = Path("C:/Users/cstone/OneDrive - UNSW/Documents/Projects/my_experiments/val_decode/data/") 
srcDataEEG = sorted(DATAPATH.glob('**/*.bdf'))
srcDataBeh = sorted(DATAPATH.glob('**/*beh.txt'))
srcDataFrms = sorted(DATAPATH.glob('**/*frms.txt'))

# extract some BIDS info
task, modality, _, subID = srcDataEEG[0].stem.split('_')
modality = modality.lower()
subID = f"sub-{re.search(r'\d+', subID)[0].zfill(2)}" # extract subject number from string, add leading zero if < 10

# load data
raw = mne.io.read_raw_bdf(
    srcDataEEG[0],
    preload=True)

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
vEOG = raw[LV][0] - raw[UV][0]
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
event_dict = {
        "hi/to/le/iti": 100,
        "hi/to/le/cue": 110,
        "hi/to/le/soa": 120,
        "hi/to/le/tar": 130,
        "hi/to/le/rsc": 140,
        "hi/to/le/fdbc": 150,
        "hi/to/le/fdbi": 160,
        "hi/to/le/fdbm": 199,
        "hi/to/ri/iti": 200,
        "hi/to/ri/cue": 210,
        "hi/to/ri/soa": 220,
        "hi/to/ri/tar": 230,
        "hi/to/ri/rsc": 240,
        "hi/to/ri/fdbc": 250,
        "hi/to/ri/fdbi": 260,
        "hi/to/ri/fdbm": 299,
        "hi/aw/le/iti": 300,
        "hi/aw/le/cue": 310,
        "hi/aw/le/soa": 320,
        "hi/aw/le/tar": 330,
        "hi/aw/le/rsc": 340,
        "hi/aw/le/fdbc": 350,
        "hi/aw/le/fdbi": 360,
        "hi/aw/le/fdbm": 399,
        "hi/aw/ri/iti": 400,
        "hi/aw/ri/cue": 410,
        "hi/aw/ri/soa": 420,
        "hi/aw/ri/tar": 430,
        "hi/aw/ri/rsc": 440,
        "hi/aw/ri/fdbc": 450,
        "hi/aw/ri/fdbi": 460,
        "hi/aw/ri/fdbm": 499,
        "lo/to/le/iti": 500,
        "lo/to/le/cue": 510,
        "lo/to/le/soa": 520,
        "lo/to/le/tar": 530,
        "lo/to/le/rsc": 540,
        "lo/to/le/fdbc": 550,
        "lo/to/le/fdbi": 560,
        "lo/to/le/fdbm": 599,
        "lo/to/ri/iti": 600,
        "lo/to/ri/cue": 610,
        "lo/to/ri/soa": 620,
        "lo/to/ri/tar": 630,
        "lo/to/ri/rsc": 640,
        "lo/to/ri/fdbc": 650,
        "lo/to/ri/fdbi": 660,
        "lo/to/ri/fdbm": 699,
        "lo/aw/le/iti": 700,
        "lo/aw/le/cue": 710,
        "lo/aw/le/soa": 720,
        "lo/aw/le/tar": 730,
        "lo/aw/le/rsc": 740,
        "lo/aw/le/fdbc": 750,
        "lo/aw/le/fdbi": 760,
        "lo/aw/le/fdbm": 799,
        "lo/aw/ri/iti": 800,
        "lo/aw/ri/cue": 810,
        "lo/aw/ri/soa": 820,
        "lo/aw/ri/tar": 830,
        "lo/aw/ri/rsc": 840,
        "lo/aw/ri/fdbc": 850,
        "lo/aw/ri/fdbi": 860,
        "lo/aw/ri/fdbm": 899
    }

# plot events
mne.viz.plot_events(eeg_events, 
                    event_id=event_dict,
                    sfreq=raw.info['sfreq'])

# check event counts
for key, val in event_dict.items():
    ev_mask = eeg_events[:, 2] == val
    count = ev_mask.sum()
    print(f'{key}: {count}')

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
raw.plot()

# filter EEG data
filt = raw.copy()
filt.filter(
    l_freq=0.1, 
    h_freq=49, 
    picks='eeg')

# filter EOG data
filt.filter(
    l_freq=1,
    h_freq=10,
    picks='eog')

# interpolate bads
filt.info['bads'] += mne.preprocessing.find_bad_channels_lof(filt, 
                                                                return_scores=False, 
                                                                threshold=3) 
filt = filt.interpolate_bads()   

# re-reference to average 
avRef = filt.copy().set_eeg_reference()

# remove blinks
ica_data = raw.copy().filter(l_freq=1.0, h_freq=None) # use unfiltered data to apply higher high-pass filter
ICA = mne.preprocessing.ICA() 
ICA.fit(ica_data, decim=4) # speed up processing
ICA.plot_sources(avRef, show_scrollbars=False)
eog_indices, eog_scores = ICA.find_bads_eog(avRef) # automatically find the ICs that best match the EOG signal
ICA.exclude = eog_indices
ICA.apply(avRef)
avRef.plot()

# create epochs
epochs = mne.Epochs(
    raw=avRef,
    events=eeg_events,
    event_id=event_dict,
    tmin=-0.2, 
    tmax=1.7, 
    baseline=(-0.2, 0), 
    decim=raw.info['sfreq']/256,
    preload=True, 
    reject=None, 
    flat=None,
    reject_by_annotation=False,
    on_missing='warn')


## Check ERPs -------------------------------------------
picks = ['O1', 'O2', 'Oz', 'Iz']
epochs["cue"].plot_image(picks=picks, combine="mean")
tar_high = epochs["hi/cue"].average(picks=picks)
tar_low= epochs["lo/cue"].average(picks=picks)
mne.viz.plot_compare_evokeds([tar_high, tar_low], 
                             picks=picks, 
                             combine='mean')

picks = ['Fpz', 'Cz', 'CPz', 'Pz']
epochs["tar"].plot_image(picks=picks, combine="mean")
cue_to = epochs["to/tar"].average()
cue_aw= epochs["aw/tar"].average()
mne.viz.plot_compare_evokeds([cue_to, cue_aw], 
                             picks=picks,
                             combine='mean')

evks = epochs.average(by_event_type=True)
mne.viz.plot_compare_evokeds([tar_high, tar_low], picks=picks)

picks=['P2', 'P4', 'PO4', 'PO8', 'O2']
picks = ['O1', 'O2', 'Oz', 'Iz']
cue_hi_le= epochs["hi/le/cue"].average()
cue_hi_ri= epochs["hi/ri/cue"].average()
cue_lo_le= epochs["lo/le/cue"].average()
cue_lo_ri= epochs["lo/ri/cue"].average()
mne.viz.plot_compare_evokeds([cue_hi_le, cue_hi_ri, cue_lo_le, cue_lo_ri], 
                             picks=picks,
                             combine='mean')

biosemi_montage = mne.channels.make_standard_montage("biosemi64")
biosemi_montage.plot()  


## Check decoding ---------------------------------------------
# get indicies of classes
epochs_cue = epochs['cue']
epochs_cue.selection = np.arange(0, len(epochs_cue))
# response rule
epochs_To = epochs_cue['to'].selection
epochs_Aw = epochs_cue['aw'].selection
# value
epochs_Hi = epochs_cue['hi'].selection
epochs_Lo = epochs_cue['lo'].selection
# stimulus location
epochs_SLe = epochs_cue['le'].selection
epochs_SRi = epochs_cue['ri'].selection
# correct response 
epochs_RLe = epochs_cue[['to/le', 'aw/ri']].selection
epochs_RRi = epochs_cue[['to/ri', 'aw/le']].selection

# make pipeline for analysis
pipeline = sk.pipeline.make_pipeline(
            sk.preprocessing.StandardScaler(),
            sk.discriminant_analysis.LinearDiscriminantAnalysis())

# extend pipeline over time
temp_decode = mne.decoding.SlidingEstimator(pipeline, scoring="accuracy") 

# define cross-validation
skf = sk.model_selection.StratifiedKFold(n_splits=4, shuffle=True)

## evaluate models
X = epochs_cue.get_data()[:, 0:64, :]
# cue
y = np.zeros(len(epochs_cue))
y[epochs_To] = 1
scores_cue = mne.decoding.cross_val_multiscore(temp_decode, X, y, cv=skf, n_jobs=None)
# value
y = np.zeros(len(epochs_cue))
y[epochs_Hi] = 1
scores_val = mne.decoding.cross_val_multiscore(temp_decode, X, y, cv=skf, n_jobs=None)
# stim position
y = np.zeros(len(epochs_cue))
y[epochs_SLe] = 1
scores_stim = mne.decoding.cross_val_multiscore(temp_decode, X, y, cv=skf, n_jobs=None)
# correct response
y = np.zeros(len(epochs_cue))
y[epochs_RLe] = 1
scores_resp = mne.decoding.cross_val_multiscore(temp_decode, X, y, cv=skf, n_jobs=None)


fig, ax = plt.subplots()
# ax.vlines([0.3, 0.7], ymin=0.45, ymax=0.75, color='grey', linestyle='--', alpha=0.5)
ax.vlines([0.0, 0.5], ymin=0.45, ymax=0.75, color='grey', linestyle='--', alpha=0.5)
ax.hlines(.5, xmin=-0.2, xmax=1.7, color='grey', linestyle='--', alpha=0.5)
ax.plot(epochs_cue.times, scores_cue.mean(0), label = 'Cue')
ax.plot(epochs_cue.times, scores_val.mean(0), label = 'Value')
ax.plot(epochs_cue.times, scores_stim.mean(0), label = 'Stimulus')
ax.plot(epochs_cue.times, scores_resp.mean(0), label = 'Response')
ax.set_ylabel('Decoding accuracy %')
ax.set_yticks(ticks=[0.5, 0.6, 0.7], labels=[50, 60, 70])
ax.set_xlabel('Time from cue onset (s)')
ax.set_xticks([0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6])
fig.legend(loc=[0.75, 0.6])


skf = sk.model_selection.StratifiedKFold(n_splits=5, shuffle=True)
# splits = skf.split(X[:, :, 90], y)
scores = sk.model_selection.cross_val_score(pipeline, X[:, :, 90], y, cv=skf)

# RSA multi-class approach
epochs_cue = epochs['cue']
epochs_cue.selection = np.arange(0, len(epochs_cue))

y = np.zeros(len(epochs_cue))
for event_type in epochs_cue.event_id.keys():
    idxs = epochs_cue[event_type].selection
    y[idxs] = epochs_cue.event_id[event_type]
X = epochs_cue.get_data()[:, 0:64, :]


x = X[:, :, 90]
skf = sk.model_selection.StratifiedKFold(n_splits=4, shuffle=True)
splits = list(skf.split(X[:, :, 90], y))
cv_lda = sk.discriminant_analysis.LinearDiscriminantAnalysis()

cv_lda.fit(x[splits[0][0]], y[splits[0][0]])
cv_lda.score(x[splits[0][1]], y[splits[0][1]])
dfun = cv_lda.decision_function(x[splits[0][1]])
cv_lda.predict(x[splits[0][1]])
pprob = cv_lda.predict_proba(x[splits[0][1]])
plogprob = cv_lda.predict_log_proba(x[splits[0][1]])






with open(srcDataFrms[0]) as f:
    frms = json.load(f)

REFRATE = 120

## check dropped frames
thold = 1/REFRATE + 0.002
frms_all = []
for block in frms.keys():
    for trial in frms[block].keys():
        frms_all.append(frms[block][trial][:])
frms_all = np.concat(frms_all)
dropped_frames = sum(frms_all > thold)
dropped_frames_pcnt = np.round(dropped_frames/len(frms_all), 2)
plt.plot(frms_all, marker = 'o', ms = 0.5, ls = '')
plt.hlines(thold, xmin=0, xmax=len(frms_all), color = 'black', ls = '--')
plt.hlines(1/REFRATE, xmin=0, xmax=len(frms_all), color = 'black', )
plt.hlines((1/REFRATE)*2, xmin=0, xmax=len(frms_all), color = 'black')
plt.title(f'Dropped frames: {dropped_frames} of {len(frms_all)} ({dropped_frames_pcnt}%)')
plt.ylabel('Frame duration (s)')
plt.xlabel('Trial')

## check behavioural performance
data = pl.read_csv(srcDataBeh[2], separator="\t")
# check accuracy and response time
data.filter(
    pl.col('RT') <= 1
    ).group_by(
    "ResponseRule", "StimulusValue", 'StimulusPosition'
    ).agg(
    pl.col('RT').mean().alias('Mean RT'),
    pl.col('Accuracy').mean().alias('Mean Acc'),
    )
# check points and reward total
data.select('Points', 'Cents').sum()
print(f'Reward: ${np.round(data[-1, 'CumulativeCents']/100, 2)}')
    