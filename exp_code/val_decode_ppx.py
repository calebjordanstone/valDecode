from datetime import datetime
from psychopy import core, event, visual, logging, gui, event, monitors, data 
from pathlib import Path
from propixx_functions import *
import polars as pl
import numpy as np
import matplotlib.pyplot as plt
import os
import itertools
import json
from pypixxlib import _libdpx as dp
logging.console.setLevel(logging.WARNING)
## ======================================================================
## Get experiment and monitor information 
## ======================================================================
expInfo = {
    'Monitor Name': 'test',
    'Monitor Refresh Rate': '120',
    'Subject Number': '',
    'Screen Number': '1',
    'Session': ['main'],
    'Full Screen': True}

dlg = gui.DlgFromDict(
    expInfo, 
    title = 'Experinent Information')

if not dlg.OK:
    core.quit()

EXPERIMENT = 'valDecode'
DATETIME = datetime.now().strftime("%y%m%d%H%M") 
SUBID = 'sub-' + f'{int(expInfo["Subject Number"])}'.zfill(2)
REFRATE = int(expInfo['Monitor Refresh Rate'])
SCREEN = int(expInfo['Screen Number'])
FULLSCREEN = expInfo['Full Screen']

# configure monitor
if expInfo['Monitor Name'] in monitors.getAllMonitors():
    MONITOR = monitors.Monitor(expInfo['Monitor Name'])

else:
    monInfo = {
        'Monitor y (pxl)': '1080',
        'Monitor x (pxl)': '1920',
        'Monitor Width (cm)': '',
        'Monitor Distance (cm)': ''}

    dlg = gui.DlgFromDict(
        monInfo, 
        title = 'Monitor Information')

    if dlg.OK:
        MONITOR = monitors.Monitor(expInfo['Monitor Name'])
        MONITOR.setDistance(int(monInfo['Monitor Distance (cm)']))
        MONITOR.setWidth(float(monInfo['Monitor Width (cm)']))
        MONITOR.setSizePix((int(monInfo['Monitor x (pxl)']), 
                            int(monInfo['Monitor y (pxl)'])))
        MONITOR.saveMon()
    else:
        gui.criticalDlg('Something went wrong. Aborting')
        core.quit()

## ======================================================================
## Create data paths
## ======================================================================
# DATAPATH = 'C:/Users/cstone/OneDrive - UNSW/Documents/Projects/my_experiments/val_decode/data/'
DATAPATH = '/home/experimenter/Experiments/val_decode/data/'
os.mkdir(DATAPATH + SUBID)
folders = ['/beh/', '/eeg/']
for folder in folders:
    os.mkdir(DATAPATH + SUBID + folder)
FILENAME = f'{SUBID}_task-{EXPERIMENT}_beh.txt'
LOGFILENAME = f'{SUBID}_task-{EXPERIMENT}_log.txt'
FRMSFILENAME = f'{SUBID}_task-{EXPERIMENT}_frms.txt'
FILEPATH = DATAPATH + SUBID + folders[0] + FILENAME
LOGFILEPATH = DATAPATH + SUBID + folders[0] + LOGFILENAME
FRMSFILEPATH = DATAPATH + SUBID + folders[0] + FRMSFILENAME
logging.LogFile(LOGFILEPATH)
## ======================================================================
## Create window, stimuli, and other experiment features
## ======================================================================
# create window
win = visual.Window(
        screen = SCREEN, 
        monitor = MONITOR,
        size = MONITOR.getSizePix(),
        fullscr=FULLSCREEN,
        pos = [0, 0],
        color = [0, 0, 0], # black
        units = 'pix', 
        colorSpace = 'rgb255',
        blendMode = 'avg')
win.refreshThreshold = 1/REFRATE + 0.002

# set mouse to invisible
mouse = event.Mouse(visible=False)

# create clock
clock = core.Clock()

# define stimulus positions
size_params = { # common parameters used to calculate stimulus size in pixels
    'distance': MONITOR.getDistance(), 
    'screen_res': MONITOR.getSizePix(), 
    'screen_width': MONITOR.getWidth()
}
y_pos = dva_to_pix(2, **size_params) # stmuli to appear 2 degrees down from centre
x_pos = dva_to_pix(5, **size_params) # stmuli to appear 5 degrees to left/right of centre
CENTRE = [0, 0]
LEFT = [-x_pos, -y_pos]
RIGHT = [x_pos, -y_pos]
TLC = [-win.size[0]/2, win.size[1]/2] # top left corer

# define colours
BLUE =   [37, 141, 165] #[0, 140, 255] ## check these are okay
ORANGE = [193, 95, 30] #[255, 140, 0] ## check these are okay
WHITE = [255, 255, 255]

# Aassign high and low value to different colours
if int(expInfo["Subject Number"]) % 2 == 0: # if even, assign high value stimulus to colour blue
    HIGH = BLUE
    LOW = ORANGE
    sub_col_assign_high = 'BLUE'
    sub_col_assign_low = 'ORANGE'
else:
    HIGH = ORANGE
    LOW = BLUE
    sub_col_assign_high = 'ORANGE'
    sub_col_assign_low = 'BLUE'

# define rulesc
CON = 'T' # respond toward the stimulus
INCON = 'A' # respond away from the stimulus

# define keys===========================================================================
RESPKEYS = ['c', 'm']
QUITKEYS = ['escape']
CONKEYS = ['space']

# define number of trials/blocks
NTRIALS = 72
NBLOCKS = 20
NBLOCKSREWARD = 12

# define points
POINTS_HIGH = 50
POINTS_LOW = 5

# calculate total possible points
total_points = ((NBLOCKSREWARD*NTRIALS/2*POINTS_HIGH) + # 12 blocks with half of trials == high reward
                (NBLOCKSREWARD*NTRIALS/2*POINTS_LOW) + # 12 blocks with half of trials == low reward
                ((NBLOCKS-NBLOCKSREWARD)*NTRIALS*POINTS_LOW)) # 8 blocks with all trials == low reward
total_reward = 1500 # $15, or 1500 cents
cents_per_point = total_reward / total_points

# define timing (multiply all by 4 to account for increase in refrate from PROPixx)
ITI_RANGE = [int(0.8*REFRATE)*4, int(1*REFRATE)*4] 
CUE_DUR = int(0.3*REFRATE)*4
SOA_DUR = int(0.2*REFRATE)*4
TAR_DUR = int(10*REFRATE)*4 #int(0.2*REFRATE)*4
RES_DUR = int(1*REFRATE)*4
FDB_DUR = int(0.5*REFRATE)*4
FDB_DUR_LNG = int(1.5*REFRATE)*4

# create stimuli
stim_params = { # common parameters used across stimuli
    'win': win, 
    'units': 'pix', 
    'opacity': 1,
    'contrast': 1,
    'colorSpace': 'rgb255'}
crc_stim_rad = dva_to_pix(dva=1.5, **size_params)
crc_stim = visual.Circle(
    radius = crc_stim_rad,
    edges = 100,
    lineWidth = 0,
    **stim_params)
txt_stim_height = dva_to_pix(dva=1, **size_params)
txt_stim = visual.TextStim(
    pos=CENTRE,
    color=WHITE,
    height=txt_stim_height,
    wrapWidth=MONITOR.getSizePix()[0]/2*0.8, # 80% of width of screen
    **stim_params)
fix_stim_height = dva_to_pix(dva=1, **size_params)
fix_stim = visual.TextStim(
    text= "+",
    pos=CENTRE,
    color=WHITE,
    height=fix_stim_height,
    **stim_params)
cue_stim_height = dva_to_pix(dva=1.5, **size_params)
cue_stim = visual.TextStim(
    pos=CENTRE,
    color=WHITE,
    height=cue_stim_height,
    **stim_params)
trig_stim = visual.Line(
    pos=TLC,
    start=TLC,
    end=[TLC[0]+1, TLC[1]],
    interpolate = False,
    **stim_params)
stims = [crc_stim, txt_stim, fix_stim, cue_stim] # create list of all stimuli to use later 

# create experiment handler to save output
experimentDict = {
    'Experiment': EXPERIMENT,
    'Date': DATETIME,
    'Refrate': REFRATE,
    'MonitorSize' : MONITOR.getSizePix(),
    'Subject': SUBID,
    'SubHighValColAssign': sub_col_assign_high}

exp = data.ExperimentHandler(name=EXPERIMENT,
                             extraInfo=experimentDict)
exp.dataNames = []                     

# write instruction text
instructText = {

'intro_1': '''
    Welcome to our experiment! \n\n\n\
    On every trial, you will see a letter followed by a coloured circle. \n
    Depending on the letter and the location of the circle, you will need to press either the "C" or the "M" key. \n\n\n
    Press space to continue. 
    ''',

'intro_2': '''
    If you see the letter "T", press the response key that is on the SAME side as the circle.\n
    "T" stands for responding "toward" the circle.\n
    So, if you see the letter "T", and the circle is on the LEFT side of the screen, you would press the "C" key.\n
    But if you see the letter "T", and the circle is on the RIGHT side, you would press the "M" key. \n\n\n
    Press space to continue. 
    ''',

'intro_3': '''
    If you see the letter "A", press the response key that is on the OPPOSITE side to the circle.\n
    "A" stands for responding "away" from the circle. \n
    So, if you see the letter "A", and the circle is on the LEFT side of the screen, you would press the "M" key. \n
    But if you see the letter "A", and the circle is on the RIGHT side, you would press the "C" key. \n\n\n
    Press space to continue. 
    ''',

'intro_4': f'''
    The circle will be either ORANGE or BLUE. \n
    If the circle is {sub_col_assign_high}, you can earn {POINTS_HIGH} points for a correct response. \n
    If the circle is {sub_col_assign_low}, you can earn {POINTS_LOW} points for a correct response. \n
    These points will be converted into additional reimbursement at the end of the experiment. \n\n\n
    Press space to continue.
    ''', 

'extinction': f'''
    From now on, all correct responses will earn you {POINTS_LOW} points. \n\n\n 
    Please keep your eyes on the fixation cross throughout each trial. \n\n\n
    Press space to begin Block {NBLOCKSREWARD + 1}.
    '''
}
## ======================================================================
## Initialise PROPixx, adjust stimulus parameters, define triggers
## ======================================================================
# establish connection to hardware
dp.DPxOpen()
isReady = dp.DPxIsReady()
if isReady:
    dp.DPxSetPPxDlpSeqPgrm('QUAD4X') # set to 4x refresh rate
    dp.DPxEnableDoutPixelMode() # enable pixel mode for triggers
    dp.DPxEnablePPxRearProjection() # enable rear projection to reverse display
    dp.DPxWriteRegCache()
else:
    print('Warning! DPx call failed, check connection to hardware')
    core.quit()

# rescale stimuli to half size to account for resolution drop
for stim in stims:
    stim.size = stim.size/2 

# create new positions to display stimuli in each quadrant
CENTRE_QUAD = reformat_for_propixx(win, CENTRE) 
LEFT_QUAD = reformat_for_propixx(win, LEFT)
RIGHT_QUAD = reformat_for_propixx(win, RIGHT)
TLC_QUAD = reformat_for_propixx(win, TLC)

# define trigger values
'''
Block number triggers:
block number. e.g., 1 = block 1

Trial number triggers:
1 to nTrials. e.g., 1 = trial 1

Trial structure triggers:
Stim value: H = high, L = low
Rule cue: T = toward (congruent), A = away (incongruent)
Stim location: L = left, R = right
HTL: 1xx
HTR: 2xx
HAL: 3xx
HAR: 4xx
LTL: 5xx
LTR: 6xx
LAL: 7xx
LAR: 8xx

ITI: x00 # inter-trial interval onset
CUE: x10 # rule cue onsest
SOA: x20 # stim-onset asynchrony onset
TAR: x30 # target onset
RSC: x40 # response cue onset
RES: x50 # correct response
RES: x60 # incorrect response
RES: x99 # missed response
'''

def find_trigger_prefix(trial_info):

    if (trial_info[0] == HIGH) & (trial_info[1] == CON) & (trial_info[2] == LEFT):
        pfx = 100
    elif (trial_info[0] == HIGH) & (trial_info[1] == CON) & (trial_info[2] == RIGHT):
        pfx = 200
    elif (trial_info[0] == HIGH) & (trial_info[1] == INCON) & (trial_info[2] == LEFT):
        pfx = 300
    elif (trial_info[0] == HIGH) & (trial_info[1] == INCON) & (trial_info[2] == RIGHT):
        pfx = 400
    elif (trial_info[0] == LOW) & (trial_info[1] == CON) & (trial_info[2] == LEFT):
        pfx = 500
    elif (trial_info[0] == LOW) & (trial_info[1] == CON) & (trial_info[2] == RIGHT):
        pfx = 600
    elif (trial_info[0] == LOW) & (trial_info[1] == INCON) & (trial_info[2] == LEFT):
        pfx = 700
    elif (trial_info[0] == LOW) & (trial_info[1] == INCON) & (trial_info[2] == RIGHT):
        pfx = 800

    return pfx
## ======================================================================
## Create trial structure
## ======================================================================
trl_stc = np.repeat(
    np.asarray(
        tuple(
            itertools.product(
                [HIGH, LOW], # valuE   
                [CON, INCON], # rule
                [LEFT, RIGHT])), # location  
            dtype = 'object'), 
    repeats=9, # number of each trial type per block
    axis=0)
## ======================================================================
## Present stimuli
## ======================================================================
## Present instructions -------------------------------------------------------------- START INSTRUCTIONS
event.clearEvents()
logging.warning('INSTRUCTIONS_1')
logging.flush()
txt_stim.text = instructText['intro_1']
instr_idx = 0
while True:

    # keep track of quadrants
    quad_idx = (instr_idx % 4)

    # draw
    txt_stim.pos = CENTRE_QUAD[quad_idx]
    txt_stim.draw()

    # collect user input to exit 
    pressed = event.getKeys(keyList = CONKEYS)
    if pressed: 
        break

    # flip window once fourth quadrant is drawn
    if quad_idx == 3: 
        win.flip()

    # udpate frame
    instr_idx += 1 

event.clearEvents()
logging.warning('INSTRUCTIONS_2')
logging.flush()
txt_stim.text = instructText['intro_2']
instr_idx = 0
while True:

    # keep track of quadrants
    quad_idx = (instr_idx % 4)

    # draw
    txt_stim.pos = CENTRE_QUAD[quad_idx]
    txt_stim.draw()

    # collect user input to exit 
    pressed = event.getKeys(keyList = CONKEYS)
    if pressed:
        break

    # flip window once fourth quadrant is drawn
    if quad_idx == 3: 
        win.flip()

    # udpate frame
    instr_idx += 1 

event.clearEvents()
logging.warning('INSTRUCTIONS_3')
logging.flush()
txt_stim.text = instructText['intro_3']
instr_idx = 0
while True:

    # keep track of quadrants
    quad_idx = (instr_idx % 4)

    # draw
    txt_stim.pos = CENTRE_QUAD[quad_idx]
    txt_stim.draw()

    # collect user input to exit 
    pressed = event.getKeys(keyList = CONKEYS)
    if pressed:
        break

    # flip window once fourth quadrant is drawn
    if quad_idx == 3: 
        win.flip()

    # udpate frame
    instr_idx += 1 


event.clearEvents()
logging.warning('INSTRUCTIONS_4')
logging.flush()
txt_stim.text = instructText['intro_4']
instr_idx = 0
while True:

    # keep track of quadrants
    quad_idx = (instr_idx % 4)

    # draw
    txt_stim.pos = CENTRE_QUAD[quad_idx]
    txt_stim.draw()

    # collect user input to exit 
    pressed = event.getKeys(keyList = CONKEYS)
    if pressed:
        break

    # flip window once fourth quadrant is drawn
    if quad_idx == 3: 
        win.flip()

    # udpate frame
    instr_idx += 1 

## Start experiment ------------------------------------------------------------------ EXP ONSET
logging.warning('START_EXP')
logging.flush()
framesPerBlock = {}
runningTrialNo = 1
cumulative_points = 0
cumulative_cents = 0
for block in range(1, NBLOCKS + 1): #------------------------------------------------- BLOCK ONSET

    # present instructions 
    if block == NBLOCKSREWARD + 1:
        txt_stim.text = instructText['extinction']
    else:
        txt_stim.text = f'This is Block {block}. \n\n Please keep your eyes on the fixation cross throughout each trial. \n\n Press space to begin.' 
    
    txt_idx = 0
    trg_rgb = dp.DPxTriggerToRGB(900 + block) # convert to RGB
    while True:

        # keep track of quadrants
        quad_idx = (txt_idx % 4)

        # draw trigger stimulus
        if txt_idx < 4:
            trig_stim.lineColor = trg_rgb
            trig_stim.start = TLC_QUAD[quad_idx]
            trig_stim.end= [TLC_QUAD[quad_idx][0] + 1, TLC_QUAD[quad_idx][1]]
            trig_stim.draw()

        # draw
        txt_stim.pos = CENTRE_QUAD[quad_idx]
        txt_stim.draw()

        # collect user input to exit 
        pressed = event.getKeys(keyList = CONKEYS)
        if pressed:
            break

        # flip window once fourth quadrant is drawn
        if quad_idx == 3: 
            win.flip()

        # udpate frame
        txt_idx += 1 
    
    # randomise trial order at start of each block 
    trl_strc = trl_stc.copy()
    while True:
        np.random.shuffle(trl_strc)
        repeats = 0
        switch = 0
        for i in range(0, len(trl_strc)-1):
            if trl_strc[i][2] == trl_strc[i+1][2]:
                repeats += 1
            elif trl_strc[i][2] != trl_strc[i+1][2]:
                switch += 1
        if np.isin(repeats - switch, [-1, 1]) == True:
            break
        else: 
            continue 
    
    # logging 
    logging.warning(f'START_BLOCK_{block}')
    logging.flush()
    # turn on recording of frame intervals
    win.recordFrameIntervals = True
    framesPerTrial = {}
    # start presenting trials ---------------------------------------------------------- TRIAL ONSET
    for trial in range(0, NTRIALS):
        
        # send trial number trigger ----------------------------------------------------TRG ONSET
        trg_rgb = dp.DPxTriggerToRGB(trial + 1)
        for frame in range(0, 4): 

            if frame == 0:
                logging.warning('TRL_TRG')
                logging.flush()

            # keep track of quadrants
            quad_idx = (frame % 4)

            # draw trigger stimulus
            trig_stim.lineColor = trg_rgb
            trig_stim.start = TLC_QUAD[quad_idx]
            trig_stim.end= [TLC_QUAD[quad_idx][0] + 1, TLC_QUAD[quad_idx][1]]
            trig_stim.draw()

            # flip window once fourth quadrant is drawn
            if quad_idx == 3: 
                win.flip()
        
        # set stimulus properties for this trial 
        crc_stim.fillColor = trl_strc[trial][0]
        crc_stim.pos= trl_strc[trial][2]
        cue_stim.text = trl_strc[trial][1]
        ITI_DUR = np.random.randint(ITI_RANGE[0], ITI_RANGE[1])
        
        # reset frame interval counting
        win.frameClock.reset()
        win.frameIntervals = []

        # find starting trigger value for this trial
        trg_val = find_trigger_prefix(trl_strc[trial]) 
        trg_rgb = dp.DPxTriggerToRGB(trg_val) # convert to RGB

        # housekeeping
        event.clearEvents()
        logging.warning(f'START_TRIAL_{trial}')
        logging.flush()

        # start stimulus presentation -------------------------------------------------- ITI ONSET
        for frame in range(0, ITI_DUR): 

            if frame == 0:
                logging.warning('START_ISI')
                logging.flush()

            # keep track of quadrants
            quad_idx = (frame % 4)

            # draw trigger stimulus
            if frame < 4:
                trig_stim.lineColor = trg_rgb
                trig_stim.start = TLC_QUAD[quad_idx]
                trig_stim.end= [TLC_QUAD[quad_idx][0] + 1, TLC_QUAD[quad_idx][1]]
                trig_stim.draw()

            # draw fixation
            fix_stim.pos = CENTRE_QUAD[quad_idx]
            fix_stim.draw()

            # flip window once fourth quadrant is drawn
            if quad_idx == 3: 
                win.flip()
        
        # update trigger value
        trg_val += 10
        trg_rgb = dp.DPxTriggerToRGB(trg_val)
        for frame in range(0, CUE_DUR): # ----------------------------------------------- CUE ONSET

            if frame == 0:
                logging.warning('START_CUE')
                logging.flush()

            # keep track of quadrants
            quad_idx = (frame % 4)

            # draw trigger stimulus
            if frame < 4:
                trig_stim.lineColor = trg_rgb
                trig_stim.start = TLC_QUAD[quad_idx]
                trig_stim.end= [TLC_QUAD[quad_idx][0] + 1, TLC_QUAD[quad_idx][1]]
                trig_stim.draw()

            # draw cue
            cue_stim.pos = CENTRE_QUAD[quad_idx]
            cue_stim.draw()

            # flip window once fourth quadrant is drawn
            if quad_idx == 3: 
                win.flip()

        # update trigger value
        trg_val += 10
        trg_rgb = dp.DPxTriggerToRGB(trg_val)
        for frame in range(0, SOA_DUR): # ------------------------------------------------ SOA ONSET

            if frame == 0:
                logging.warning('START_SOA')
                logging.flush()
            
        
            # keep track of quadrants
            quad_idx = (frame % 4)

            # draw trigger stimulus
            if frame < 4:
                trig_stim.lineColor = trg_rgb
                trig_stim.start = TLC_QUAD[quad_idx]
                trig_stim.end= [TLC_QUAD[quad_idx][0] + 1, TLC_QUAD[quad_idx][1]]
                trig_stim.draw()

            # draw fixation
            fix_stim.pos = CENTRE_QUAD[quad_idx]
            fix_stim.draw()

            # flip window once fourth quadrant is drawn
            if quad_idx == 3: 
                win.flip()
        
        # reset things before target display
        event.clearEvents()
        clock.reset()
        # update trigger value
        trg_val += 10
        trg_rgb = dp.DPxTriggerToRGB(trg_val)
        for frame in range(0, TAR_DUR): # ------------------------------------------------ TAR ONSET

            if frame == 0:
                logging.warning('START_TAR')
                logging.flush()

            # keep track of quadrants
            quad_idx = (frame % 4)

            # draw trigger stimulus
            if frame < 4:
                trig_stim.lineColor = trg_rgb
                trig_stim.start = TLC_QUAD[quad_idx]
                trig_stim.end= [TLC_QUAD[quad_idx][0] + 1, TLC_QUAD[quad_idx][1]]
                trig_stim.draw()

            # draw stimuli
            fix_stim.pos = CENTRE_QUAD[quad_idx]
            if trl_strc[trial][2] == LEFT:
                crc_stim.pos = LEFT_QUAD[quad_idx]
            elif trl_strc[trial][2] == RIGHT:
                crc_stim.pos = RIGHT_QUAD[quad_idx]
            fix_stim.draw()
            crc_stim.draw()

            # collect user input 
            early_pressed = event.getKeys(keyList = RESPKEYS, timeStamped = clock)
            if early_pressed:
                logging.warning('RESPONSE')
                logging.flush()
                response = early_pressed[0][0]
                rt = early_pressed[0][1]
                break

            # flip window once fourth quadrant is drawn
            if quad_idx == 3: 
                win.flip()

        # update trigger value
        trg_val += 10
        trg_rgb = dp.DPxTriggerToRGB(trg_val)
        for frame in range(0, RES_DUR): # ------------------------------------------------ RESP CUE ONSET

            if early_pressed: # skip response duration if responded during targed presentation
                break 

            if frame == 0:
                logging.warning('START_RES')
                logging.flush()

            # keep track of quadrants
            quad_idx = (frame % 4)

            # draw trigger stimulus
            if frame < 4:
                trig_stim.lineColor = trg_rgb
                trig_stim.start = TLC_QUAD[quad_idx]
                trig_stim.end= [TLC_QUAD[quad_idx][0] + 1, TLC_QUAD[quad_idx][1]]
                trig_stim.draw()
            
            # draw fixation
            fix_stim.pos = CENTRE_QUAD[quad_idx]
            fix_stim.draw()

            # collect response input
            quitPressed = event.getKeys(keyList = QUITKEYS)
            pressed = event.getKeys(keyList = RESPKEYS, timeStamped = clock)
            if quitPressed: # exit task 
                dp.DPxClose() 
                core.quit()
            elif pressed:
                logging.warning('RESPONSE')
                logging.flush()
                response = pressed[0][0]
                rt = pressed[0][1]
                break

            # flip window once fourth quadrant is drawn
            if quad_idx == 3: 
                win.flip()

        # calculate correct response
        if trl_strc[trial][1] == CON:
            if trl_strc[trial][2] == LEFT:
                cor_response = 'c'
            elif trl_strc[trial][2] == RIGHT:
                cor_response = 'm'
        elif trl_strc[trial][1] == INCON:
            if trl_strc[trial][2] == LEFT:
                cor_response = 'm'
            elif trl_strc[trial][2] == RIGHT:
                cor_response = 'c'

        # calculate response accuracy and points if a response is made
        if (pressed) or (early_pressed):  
            if cor_response == response:
                acc = 1
            elif cor_response != response:
                acc = 0
            # update points, tiggers, and feedback display 
            if acc == 0:
                points = 0
                trg_val += 20
                if early_pressed:
                    txt_stim.text = 'Incorrect! \n No points. \n Slow down!'
                elif pressed: 
                    txt_stim.text = 'Incorrect! \n No points.'
            elif acc == 1:
                if block <= NBLOCKSREWARD:
                    if trl_strc[trial][0] == HIGH: # high value reward
                        points = POINTS_HIGH
                    elif trl_strc[trial][0] == LOW: # low value reward
                        points = POINTS_LOW
                elif block > NBLOCKSREWARD:
                    points = POINTS_LOW
                txt_stim.text = f'+ {points} points!'
                trg_val += 10   
        # if no response made, assign missing values etc.     
        elif (not pressed) & (not early_pressed):
            response = 999
            rt = 999
            acc = 999
            points = 0
            trg_val += 59
            txt_stim.text = 'Too slow! No points'
        
        trg_rgb = dp.DPxTriggerToRGB(trg_val)
        if acc != 1:
            fdb_dur = FDB_DUR_LNG
        elif acc == 1:
            fdb_dur = FDB_DUR
        for frame in range(0, fdb_dur): # --------------------------------------------------- FDB ONSET

            if frame == 0:
                logging.warning('START_FDB')
                logging.flush()
        
            # keep track of quadrants
            quad_idx = (frame % 4)

            # draw trigger stimulus
            if frame < 4:
                trig_stim.lineColor = trg_rgb
                trig_stim.start = TLC_QUAD[quad_idx]
                trig_stim.end= [TLC_QUAD[quad_idx][0] + 1, TLC_QUAD[quad_idx][1]]
                trig_stim.draw()

            # draw stimulus
            txt_stim.draw()
            
            # flip window once fourth quadrant is drawn
            if quad_idx == 3: 
                win.flip()

        ## recover some trial information
        # get string values for stimulus colour and value
        if sub_col_assign_high == 'BLUE':
            if trl_strc[trial][0] == BLUE:
                stimColour = 'blue'
                stimVal = 'high'
            elif trl_strc[trial][0] == ORANGE:
                stimColour = 'orange'
                stimVal = 'low'
        elif sub_col_assign_high == 'ORANGE':
            if trl_strc[trial][0] == BLUE:
                stimColour = 'blue'
                stimVal = 'low'
            elif trl_strc[trial][0] == ORANGE:
                stimColour = 'orange'
                stimVal = 'high'
        # get string value for  stimulus position
        if trl_strc[trial][2] == LEFT:
            stimPos = 'left'
        elif trl_strc[trial][2] == RIGHT:
            stimPos = 'right'
        
        # calculate reward
        cents_this_trial = points*cents_per_point
        cumulative_cents += cents_this_trial
        cumulative_points += points

        # update experiment handler data to save
        exp.addData('Trial', trial + 1)
        exp.addData('RunningTrialNo', runningTrialNo)
        exp.addData('Block', block)
        exp.addData('StimulusRGB', trl_strc[trial][0])
        exp.addData('StimulusColour', stimColour)
        exp.addData('StimulusValue', stimVal)
        exp.addData('StimulusXY', trl_strc[trial][2])
        exp.addData('StimulusPosition', stimPos)
        exp.addData('ResponseRule', trl_strc[trial][1])
        exp.addData('CorrectResponse', cor_response)
        exp.addData('Response', response)
        exp.addData('RT', rt)
        exp.addData('Accuracy', acc)
        exp.addData('Points', points)
        exp.addData('CumulativePoints', cumulative_points)
        exp.addData('Cents', cents_this_trial)
        exp.addData('CumulativeCents', cumulative_cents)

        exp.nextEntry() # move to next line in data output
        runningTrialNo += 1
        framesPerTrial[f'Trial_{trial + 1}'] = win.frameIntervals

    # take a break and save data at the end of the block --------------------------------------------------
    win.recordFrameIntervals = False
    exp.saveAsWideText(
        fileName = FILEPATH, 
        appendFile=None,
        fileCollisionMethod='overwrite')
    framesPerBlock[f'Block_{block}'] = framesPerTrial
    with open(FRMSFILEPATH, 'w') as file:
        file.write(json.dumps(framesPerBlock)) 

    # present end of block text
    if block < NBLOCKS:
        txt_stim.text = f'''
        End of Block {block}. Take a break. \n\n 
        You've earnt {cumulative_points} points! That's an extra ${np.round(cumulative_cents/100, 2)}. \n\n
        Press space when you're ready to continue.''' 
    elif block == NBLOCKS:
        txt_stim.text = f'''
        End of Block {block}. You're all done! \n\n 
        You've earnt {cumulative_points} points! That's an extra ${np.round(cumulative_cents/100, 2)}. \n\n
        Press space and contact the experimenter.''' 
    txt_idx = 0
    while True:

        # keep track of quadrants
        quad_idx = (txt_idx % 4)

        # draw
        txt_stim.pos = CENTRE_QUAD[quad_idx]
        txt_stim.draw()

        # collect user input to exit 
        pressed = event.getKeys(keyList = CONKEYS)
        if pressed:
            break

        # flip window once fourth quadrant is drawn
        if quad_idx == 3: 
            win.flip()

        # udpate frame
        txt_idx += 1 
    
# end experiment and shut everything down ----------------------------------------------------
logging.warning('END_EXPERIMENT')
logging.flush()
event.clearEvents()
dp.DPxSetPPxDlpSeqPgrm('RGB')
dp.DPxDisableDoutPixelMode()
dp.DPxWriteRegCache()
dp.DPxClose()
win.close()

# check data --------------------------------------------------------------------------------
## check dropped frames
# thold = 1/REFRATE + 0.002
thold = win.refreshThreshold
frms_all = []
with open(FRMSFILEPATH) as file:
    frms = json.loads(file.read())
for block in frms.keys():
    for trial in frms[block].keys():
        frms_all.append(frms[block][trial][:])
frms_all = np.concatenate(frms_all)
dropped_frames = sum(frms_all > thold)
dropped_frames_pcnt = np.round(dropped_frames/len(frms_all), 2)
plt.plot(frms_all, marker = 'o', ms = 0.5, ls = '')
plt.hlines(thold, xmin=0, xmax=len(frms_all), color = 'black', ls = '--')
plt.hlines(1/REFRATE, xmin=0, xmax=len(frms_all), color = 'black', )
plt.hlines((1/REFRATE)*2, xmin=0, xmax=len(frms_all), color = 'black')
plt.title(f'Dropped frames: {dropped_frames} of {len(frms_all)} ({dropped_frames_pcnt}%)')
plt.ylabel('Frame duration (s)')
plt.xlabel('Frame')
plt.show()
print(f'Dropped frames: {dropped_frames} of {len(frms_all)} ({dropped_frames_pcnt}%)')

## check behavioural performance
data = pl.read_csv(FILEPATH, sep="\t")
# check accuracy and response time
data.filter(
    (pl.col('RT') <= 1) & (pl.col('RT') >= 0.2)
    ).groupby(
    ["ResponseRule", "StimulusValue", 'StimulusPosition']
    ).agg(
    [pl.col('RT').mean().alias('Mean RT'),
    pl.col('Accuracy').mean().alias('Mean Acc')],
    )
# check points and reward total
data.select(['Points', 'Cents']).sum()
reward = np.round(data[-1, 'CumulativeCents']/100, 2)
print(f'Reward: ${reward}')

# quit
core.quit()