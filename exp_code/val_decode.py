from datetime import datetime
from psychopy import core, event, visual, logging, gui, event, monitors, data # tools, parallel, misc
from pathlib import Path
import numpy as np
import os
import itertools
import json
logging.console.setLevel(logging.WARNING)
## ======================================================================
## Get experiment and monitor information 
## ======================================================================
expInfo = {
    'Monitor Name': '',
    'Monitor Refresh Rate': '',
    'Subject Number': '',
    'Screen Number': '1',
    'Session': 'main',
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
SESSION = expInfo['Session']
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
# DATAPATH = 'C:\\Users\\caleb\\OneDrive - UNSW\\Documents\\Projects\\my_experiments\\val_decode\\data\\'  ## need to change this to local path
DATAPATH = 'C:\\Users\\cstone\\OneDrive - UNSW\\Documents\\Projects\\my_experiments\\val_decode\\data\\'
os.mkdir(DATAPATH + SUBID)
folders = ['\\beh\\'] # '/eeg/'
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
## Create window, stimuli, and other experiment objects
## ======================================================================
# create window
win = visual.Window(
        screen = SCREEN, 
        monitor = MONITOR,
        size = MONITOR.getSizePix(),
        fullscr=FULLSCREEN,
        pos = [0, 0], 
        color = [0, 0, 0], # black
        units = 'deg', # need to work out conversion to pix
        colorSpace = 'rgb255',
        blendMode = 'avg')
win.refreshThreshold = 1/REFRATE + 0.002

# set mouse to invisible
mouse = event.Mouse(visible=False)

# create clock
clock = core.Clock()

# define stimulus positions
CENTRE = [0, 0]
LEFT = [-5, -1]
RIGHT = [5, -1]

# define colours
BLUE =   [37, 141, 165] 
ORANGE = [193, 95, 30]
WHITE = [255, 255, 255]

# assign high and low value to different colours 
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

# define rules
CON = 'T' # respond toward the stimulus
INCON = 'A' # respond away from the stimulus

# define keys
RESPKEYS = ['c', 'm']
QUITKEYS = ['escape', 'q']
CONKEYS = ['space']

# define points
POINTS_HIGH = 50
POINTS_LOW = 5

# define number of trials/blocks
NTRIALS = 8 # 72
NBLOCKS = 20
NBLOCKSREWARD = 2 # 12

# define timing
ITI_RANGE = [int(0.8*REFRATE), int(1*REFRATE)]
CUE_DUR = int(0.3*REFRATE)
SOA_DUR = int(0.2*REFRATE)
TAR_DUR = int(0.2*REFRATE)
RES_DUR = int(1*REFRATE)
FDB_DUR = int(0.5*REFRATE)

# create stimuli
stim_params = { # common parameters used across stimuli
    'win': win, 
    'units': 'deg', ## set to pix - need to figure out conversion for propixx
    'opacity': 1,
    'contrast': 1,
    'colorSpace': 'rgb255'} 
crc_stim = visual.Circle(
    radius = 1,
    edges = 300,
    lineWidth = 0,
    **stim_params)
txt_stim = visual.TextStim(
    pos=CENTRE,
    color=WHITE,
    height=0.8,
    wrapWidth=25, #MONITOR.getSizePix()[0]*0.8,
    **stim_params)
fix_stim = visual.TextStim(
    text= "+",
    pos=CENTRE,
    color=WHITE,
    height=1,
    **stim_params)
cue_stim = visual.TextStim(
    pos=CENTRE,
    color=WHITE,
    height=1,
    **stim_params)
stims = [crc_stim, txt_stim, fix_stim, cue_stim] # create list of all stimuli to use later 

# create experiment handler to save output
experimentDict = {
    'Experiment': EXPERIMENT,
    'Date': DATETIME,
    'Refrate': REFRATE,
    'MonitorSize' : MONITOR.getSizePix(),
    'Session': SESSION,
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
    Depending on the letter and the location of the circle, you will need to press either the "C" or the "M" key. \n
    If you see the letter "T", press the response key that is on the SAME side as the circle. "T" stands for responding "toward" the circle.\n
    So, if you see the letter "T", and the circle is on the LEFT side of the screen, you would press the "C" key. \n
    But if you see the letter "T", and the circle is on the RIGHT side, you would press the "M" key. \n\n
    If you see the letter "A", press the response key that is on the OPPOSITE side to the circle. "A" stands for responding "away" the circle. \n
    So, if you see the letter "A", and the circle is on the LEFT side of the screen, you would press the "M" key. \n
    But if you see the letter "A", and the circle is on the RIGHT side, you would press the "C" key. \n\n\n
    Press space to continue. 
    ''',

'intro_2': f'''
    The circle will be either ORANGE or BLUE. \n
    If the circle is {sub_col_assign_high}, you can earn {POINTS_HIGH} points for a correct response. \n
    If the circle is {sub_col_assign_low}, you can earn {POINTS_LOW} points for a correct response. \n\n\n
    Press space to continue.
    ''', 

'extinction': f'''
    From now on, all correct responses will earn you {POINTS_LOW} points. \n\n 
    Press space to begin block {NBLOCKSREWARD + 1}
    '''
}
## ======================================================================
## Create trial structure
## ======================================================================
# create trial structure
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
txt_stim.draw()
win.flip()
while True:
    pressed = event.getKeys(keyList = CONKEYS)
    if pressed:
        break

event.clearEvents()
logging.warning('INSTRUCTIONS_2')
logging.flush()
txt_stim.text = instructText['intro_2']
txt_stim.draw()
win.flip()
event.clearEvents()
while True:
    pressed = event.getKeys(keyList = CONKEYS)
    if pressed:
        break

## Start experiment ------------------------------------------------------------------ EXP ONSET
logging.warning('START_EXP')
logging.flush()
framesPerBlock = {}
runningTrialNo = 1
cumulative_points = 0
for block in range(1, NBLOCKS + 1):

     # present instructions 
    if block == NBLOCKSREWARD + 1:
        txt_stim.text = instructText['extinction']
    else:
        txt_stim.text = f'This is block {block}. \n\n Press space to continue.' 
    txt_stim.draw()
    win.flip()
    event.clearEvents()
    while True:
        pressed = event.getKeys(keyList = CONKEYS)
        if pressed:
            break
    
    # logging 
    logging.warning(f'START_BLOCK{block}')
    logging.flush()

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

    # turn on recording of frame intervals
    win.recordFrameIntervals = True
    framesPerTrial = {}
    # start presenting trials --------------------------------------------------------- TRIAL ONSET
    for trial in range(0, NTRIALS):
        
        # housekeeping
        event.clearEvents()
        logging.warning(f'START_TRIAL{trial + 1}')
        logging.flush()
    
        # set stimulus properties for this trial 
        crc_stim.fillColor = trl_strc[trial][0]
        crc_stim.pos= trl_strc[trial][2]
        cue_stim.text = trl_strc[trial][1]
        ITI_DUR = np.random.randint(ITI_RANGE[0], ITI_RANGE[1])
        
        # reset frame interval counting
        win.frameClock.reset()
        win.frameIntervals = []

        # start stimulus presentation ------------------------------------------------- ITI ONSET
        for frame in range(0, ITI_DUR):

            if frame == 0:
                logging.warning('START_ISI')
                logging.flush()

            # present stimuli
            fix_stim.draw()
            win.flip()
        
        for frame in range(0, CUE_DUR): #---------------------------------------------- CUE ONSET

            if frame == 0:
                logging.warning('START_CUE')
                logging.flush()

            # present stimuli
            cue_stim.draw()
            win.flip()
        
        for frame in range(0, SOA_DUR): # --------------------------------------------- SOA ONSET

            if frame == 0:
                logging.warning('START_SOA')
                logging.flush()
            
            # present stimuli
            fix_stim.draw()
            win.flip()
        
        for frame in range(0, TAR_DUR): # --------------------------------------------- TAR ONSET

            if frame == 0:
                logging.warning('START_TAR')
                logging.flush()

            # present stimuli
            fix_stim.draw()
            crc_stim.draw()
            win.flip()
        
        # reset things before response response 
        event.clearEvents()
        clock.reset()
        for frame in range(0, RES_DUR): # --------------------------------------------- RESP CUE ONSET

            if frame == 0:
                logging.warning('START_RES')
                logging.flush()

            # present stimuli
            fix_stim.draw()

            # collect response input
            quitPressed = event.getKeys(keyList = QUITKEYS)
            pressed = event.getKeys(keyList = RESPKEYS, timeStamped = clock)
            if quitPressed: # exit task 
                core.quit()
            elif pressed:
                logging.warning('RESPONSE')
                logging.flush()
                rt = pressed[0][1]
                if block <= NBLOCKSREWARD: # reward contingency blocks
                    if trl_strc[trial][2] == LEFT: # stimulus on the left side of screen
                        if (trl_strc[trial][1] == CON) & (pressed[0][0] == 'c'): # congruent rule, correct response
                            acc = 1
                            if trl_strc[trial][0] == HIGH: # high value reward
                                txt_stim.text = f'+ {POINTS_HIGH} points!'
                                points = POINTS_HIGH
                            elif trl_strc[trial][0] == LOW: # low value reward
                                txt_stim.text = f'+ {POINTS_LOW} points!'
                                points = POINTS_LOW
                        elif (trl_strc[trial][1] == CON) & (pressed[0][0] == 'm'): # congruent rule, incorrect response
                            acc = 0
                            txt_stim.text = 'Incorrect!'
                            points = 0
                        elif (trl_strc[trial][1] == INCON) & (pressed[0][0] == 'm'): # congruent rule, correct response
                            acc = 1
                            if trl_strc[trial][0] == HIGH: # high value reward
                                txt_stim.text = f'+ {POINTS_HIGH} points!'
                                points = POINTS_HIGH
                            elif trl_strc[trial][0] == LOW: # low value reward
                                txt_stim.text = f'+ {POINTS_LOW} points!' 
                                points = POINTS_LOW
                        elif (trl_strc[trial][1] == INCON) & (pressed[0][0] == 'c'):
                            acc = 0
                            txt_stim.text = 'Incorrect!'
                            points = 0
                    elif trl_strc[trial][2] == RIGHT: # stimulus on the right side of screen
                        if (trl_strc[trial][1] == CON) & (pressed[0][0] == 'm'): # congruent rule, correct response
                            acc = 1
                            if trl_strc[trial][0] == HIGH: # high value reward
                                txt_stim.text = f'+ {POINTS_HIGH} points!'
                                points = POINTS_HIGH
                            elif trl_strc[trial][0] == LOW: # high value reward
                                txt_stim.text = f'+ {POINTS_LOW} points!'
                                points = POINTS_LOW
                        elif (trl_strc[trial][1] == CON) & (pressed[0][0] == 'c'): # congruent rule, incorrect response
                            acc = 0
                            txt_stim.text = 'Incorrect!'
                            points = 0
                        elif (trl_strc[trial][1] == INCON) & (pressed[0][0] == 'c'): # congruent rule, correct response
                            acc = 1
                            if trl_strc[trial][0] == HIGH: # high value reward
                                txt_stim.text = f'+ {POINTS_HIGH} points!'
                                points = POINTS_HIGH
                            elif trl_strc[trial][0] == LOW: # high value reward
                                txt_stim.text = f'+ {POINTS_LOW} points!'
                                points = POINTS_LOW
                        elif (trl_strc[trial][1] == INCON) & (pressed[0][0] == 'm'):
                            acc = 0
                            txt_stim.text = 'Incorrect!'
                            points = 0
                    break
                elif block > NBLOCKSREWARD: # extinction blocks
                    if trl_strc[trial][2] == LEFT: # stimulus on the left side of screen
                        if (trl_strc[trial][1] == CON) & (pressed[0][0] == 'c'): # congruent rule, correct response
                            acc = 1
                            txt_stim.text = f'+ {POINTS_LOW} points!'
                            points = POINTS_LOW
                        elif (trl_strc[trial][1] == CON) & (pressed[0][0] == 'm'): # congruent rule, incorrect response
                            acc = 0
                            txt_stim.text = 'Incorrect!'
                            points = 0
                        elif (trl_strc[trial][1] == INCON) & (pressed[0][0] == 'm'): # congruent rule, correct response
                            acc = 1
                            txt_stim.text = f'+ {POINTS_LOW} points!'
                            points = POINTS_LOW
                        elif (trl_strc[trial][1] == INCON) & (pressed[0][0] == 'c'):
                            acc = 0
                            txt_stim.text = 'Incorrect!'
                            points = 0
                    elif trl_strc[trial][2] == RIGHT: # stimulus on the right side of screen
                        if (trl_strc[trial][1] == CON) & (pressed[0][0] == 'm'): # congruent rule, correct response
                            acc = 1
                            txt_stim.text = f'+ {POINTS_LOW} points!'
                            points = POINTS_LOW
                        elif (trl_strc[trial][1] == CON) & (pressed[0][0] == 'c'): # congruent rule, incorrect response
                            acc = 0
                            txt_stim.text = 'Incorrect!'
                            points = 0
                        elif (trl_strc[trial][1] == INCON) & (pressed[0][0] == 'c'): # congruent rule, correct response
                            acc = 1
                            txt_stim.text = f'+ {POINTS_LOW} points!'
                            points = POINTS_LOW
                        elif (trl_strc[trial][1] == INCON) & (pressed[0][0] == 'm'):
                            acc = 0
                            txt_stim.text = 'Incorrect!'
                            points = 0
                    break

            win.flip()
        
        if not pressed:
            rt = 999
            acc = 999
            points = 0
            txt_stim.text = 'Too slow!'

        for frame in range(0, FDB_DUR): # --------------------------------------------------- FDB ONSET

            if frame == 0:
                logging.warning('START_FDB')
                logging.flush()
            
            # present stimuli
            txt_stim.draw()
            win.flip()

        # recover some trial information
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
        
        if trl_strc[trial][2] == LEFT:
            stimPos = 'left'
        elif trl_strc[trial][2] == RIGHT:
            stimPos = 'right'

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
        exp.addData('RT', rt)
        exp.addData('Accuracy', acc)
        exp.addData('Points', points)
        exp.addData('CumulativePoints', cumulative_points)
        exp.nextEntry() # move to next line in data output
        runningTrialNo += 1
        framesPerTrial[f'Trial_{trial + 1}'] = win.frameIntervals

    # take a break at the end of the block --------------------------------------------------
    win.recordFrameIntervals = False
    txt_stim.text = f'''
    End of block {block}. Take a break. \n\n 
    You've earnt {cumulative_points} points! \n\n
    Press space when you're ready to continue.''' 
    txt_stim.draw()
    win.flip()
    event.clearEvents()
    while True:
        pressed = event.getKeys(keyList = CONKEYS)
        if pressed:
            break
    
    # save data at end of block
    exp.saveAsWideText(
        fileName = FILEPATH, 
        appendFile=None,
        fileCollisionMethod='overwrite')
    framesPerBlock[f'Block_{block}'] = framesPerTrial
    with open(FRMSFILEPATH, 'w') as file:
        file.write(json.dumps(framesPerBlock)) ## CHECK IF BETTER WAY TO DO THIS 

# end experiment and shut everything down ----------------------------------------------------
logging.warning('END_EXPERIMENT')
logging.flush()
txt_stim.text = 'End of experiment! \n\ Please press space and contact the experimenter.'
txt_stim.draw()
win.flip()
while True:
    pressed = event.getKeys(keyList = [CONKEYS, QUITKEYS])
    if pressed:
        break

win.close()
core.quit()


        
        











