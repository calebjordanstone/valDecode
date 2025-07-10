from datetime import datetime
from psychopy import core, event, visual, logging, gui, event, monitors, data # tools, parallel, misc
from pathlib import Path
from propixx_functions import *
import numpy as np
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
    'Session': ['demo'],
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
BLUE =   [37, 141, 165]
ORANGE = [194, 99, 32]  
WHITE = [255, 255, 255]
GREY = [119, 119, 119]

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

# define rules
CON = 'T' # respond toward the stimulus
INCON = 'A' # respond away from the stimulus

# define keys
RESPKEYS = ['c', 'm']
QUITKEYS = ['escape']
CONKEYS = ['space']

# define points
POINTS_HIGH = 50
POINTS_LOW = 5

# define number of trials/blocks
NTRIALS = 10
NBLOCKS = 10

# define timing (multiply all by 4 to account for increase in refrate from PROPixx)
ITI_RANGE = [int(0.8*REFRATE), int(1*REFRATE)] 
CUE_DUR = int(0.3*REFRATE)
SOA_DUR = int(0.2*REFRATE)
TAR_DUR = int(0.2*REFRATE)
RES_DUR = int(1*REFRATE)
RES_DUR_LNG = int(5*REFRATE)
FDB_DUR = int(0.5*REFRATE)
FDB_DUR_LNG = int(1.5*REFRATE)

# create stimuli
stim_params = { # common parameters used across stimuli
    'win': win, 
    'units': 'pix', 
    'opacity': 1,
    'contrast': 1,
    'colorSpace': 'rgb255'}
crc_stim_rad = dva_to_pix(dva=1.5, **size_params)
dis_stim = visual.Circle(
    radius=crc_stim_rad,
    edges=100,
    lineWidth=0,
    **stim_params)
tar_stim = visual.Circle(
    radius=crc_stim_rad,
    edges=100,
    lineWidth=0,
    fillColor=GREY,
    **stim_params)
txt_stim_height = dva_to_pix(dva=1, **size_params)
txt_stim = visual.TextStim(
    pos=CENTRE,
    color=WHITE,
    height=txt_stim_height,
    wrapWidth=MONITOR.getSizePix()[0]*0.8, # 80% of width of screen
    **stim_params)
fix_stim_height = dva_to_pix(dva=1, **size_params)
fix_stim = visual.TextStim(
    text="+",
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

# create lists of stimulus features to use later 
cols = [HIGH, LOW]
pos = [LEFT, RIGHT]
cue = [CON, INCON]
# create empty dictionary to save accuracy data
accDict = {}

# write instruction text
instructText = {

'intro_1': '''
    Welcome to our experiment! \n\n\n\
    On every trial, you will see a letter followed by a grey circle and a coloured circle presented side by side. \n
    Depending on the letter and the location of the GREY circle, you will need to press either the "C" or the "M" key. \n\n\n
    Press space to continue. 
    ''',

'intro_2': '''
    If you see the letter "T", press the response key that is on the SAME side as the GREY circle.\n
    "T" stands for responding "toward" the grey circle.\n
    So, if you see the letter "T", and the grey circle is on the LEFT side of the screen, you would press the "C" key.\n
    But if you see the letter "T", and the grey circle is on the RIGHT side, you would press the "M" key. \n\n\n
    Press space to continue. 
    ''',

'intro_3': '''
    If you see the letter "A", press the response key that is on the OPPOSITE side to the GREY circle.\n
    "A" stands for responding "away" from the grey circle. \n
    So, if you see the letter "A", and the grey circle is on the LEFT side of the screen, you would press the "M" key. \n
    But if you see the letter "A", and the grey circle is on the RIGHT side, you would press the "C" key. \n\n\n
    Press space to continue. 
    ''',

'intro_4': f'''
    The coloured circle will indicate how many points you can earn on that trial. \n
    If the coloured circle is {sub_col_assign_high}, you can earn {POINTS_HIGH} points for a correct response. \n
    If the coloured circle is {sub_col_assign_low}, you can earn {POINTS_LOW} points for a correct response. \n\n\n
    Press space to continue.
    '''
}
## ======================================================================
## Initialise PROPixx, adjust stimulus parameters, define triggers
## ======================================================================
# establish connection to hardware
dp.DPxOpen()
isReady = dp.DPxIsReady()
if isReady:
    dp.DPxSetPPxDlpSeqPgrm('RGB')
    dp.DPxEnableDoutPixelMode() # enable pixel mode for triggers
    dp.DPxEnablePPxRearProjection() # enable rear projection to reverse display
    dp.DPxWriteRegCache()
else:
    print('Warning! DPx call failed, check connection to hardware')
    core.quit()

## ======================================================================
## Present stimuli
## ======================================================================
## Present instructions -------------------------------------------------------------- START INSTRUCTIONS
for txt in instructText.keys():

    event.clearEvents()
    logging.warning(f'{txt}')
    logging.flush()
    txt_stim.text = instructText[txt]
    instr_idx = 0
    while True:

        # draw stimuli
        txt_stim.draw()
        win.flip()

        # collect user input to exit 
        pressed = event.getKeys(keyList = CONKEYS)
        if pressed: 
            break

        # udpate frame
        instr_idx += 1 

## Start experiment ------------------------------------------------------------------ EXP ONSET
logging.warning('START_EXP')
logging.flush()
framesPerBlock = {}
runningTrialNo = 1
cumulative_points = 0
for block in range(1, NBLOCKS + 1): #------------------------------------------------- BLOCK ONSET

    # present instructions 
    txt_stim.text = f'This is Practice Block {block}. \n\n Press space to start.' 
    txt_idx = 0
    while True:

        # draw
        txt_stim.draw()
        win.flip()

        # collect user input to exit 
        pressed = event.getKeys(keyList = CONKEYS)
        if pressed:
            break

        # udpate frame
        txt_idx += 1 
    
    # logging 
    logging.warning(f'START_BLOCK{block}')
    logging.flush()

    # turn on recording of frame intervals
    win.recordFrameIntervals = True
    framesPerTrial = {}

    # start presenting trials ---------------------------------------------------------- TRIAL ONSET
    accList = [] # create list to store accuracy data for this demo block
    for trial in range(0, NTRIALS):
        
        # set stimulus properties for this trial 
        dis_stim.fillColor = cols[np.random.choice([0,1])]
        dis_stim.pos = pos[np.random.choice([0, 1])]
        if np.all(dis_stim.pos == LEFT):
            tar_stim.pos = RIGHT
        elif np.all(dis_stim.pos == RIGHT): 
            tar_stim.pos = LEFT 
        cue_stim.text = cue[np.random.choice([0, 1])]
        ITI_DUR = np.random.randint(ITI_RANGE[0], ITI_RANGE[1])
        
        # reset frame interval counting
        win.frameClock.reset()
        win.frameIntervals = []

        # housekeeping
        event.clearEvents()
        logging.warning(f'START_TRIAL_{trial}')
        logging.flush()
        # start stimulus presentation -------------------------------------------------- ITI ONSET
        for frame in range(0, ITI_DUR): 

            if frame == 0:
                logging.warning('START_ISI')
                logging.flush()

            # draw fixation
            fix_stim.draw()
            win.flip()
        
        for frame in range(0, CUE_DUR): # ----------------------------------------------- CUE ONSET

            if frame == 0:
                logging.warning('START_CUE')
                logging.flush()

            # draw cue
            cue_stim.draw()
            win.flip()

        for frame in range(0, SOA_DUR): # ------------------------------------------------ SOA ONSET

            if frame == 0:
                logging.warning('START_SOA')
                logging.flush()
            
            # draw fixation
            fix_stim.draw()
            win.flip()

        # reset things before target display
        event.clearEvents()
        clock.reset()
        for frame in range(0, TAR_DUR): # ------------------------------------------------ TAR ONSET

            if frame == 0:
                logging.warning('START_TAR')
                logging.flush()

            # draw stimuli
            fix_stim.draw()
            tar_stim.draw()
            dis_stim.draw()
            win.flip()

            # collect user input 
            early_pressed = event.getKeys(keyList = RESPKEYS, timeStamped = clock)
            if early_pressed:
                logging.warning('RESPONSE')
                logging.flush()
                response = early_pressed[0][0]
                rt = early_pressed[0][1]
                break

        if block == 1: 
            res_dur = RES_DUR_LNG # extend the deadline on the first block
        else: 
            res_dur = RES_DUR
        for frame in range(0, res_dur): # ------------------------------------------------ RESP CUE ONSET

            if early_pressed: # skip response duration if responded during targed presentation
                break 

            if frame == 0:
                logging.warning('START_RES')
                logging.flush()

            # draw fixation
            fix_stim.draw()
            win.flip()

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
        
        # calculate correct response
        if cue_stim.text == CON:
            if np.all(tar_stim.pos == LEFT):
                cor_response = 'c'
            elif np.all(tar_stim.pos == RIGHT):
                cor_response = 'm'
        elif cue_stim.text == INCON:
            if np.all(tar_stim.pos == LEFT):
                cor_response = 'm'
            elif np.all(tar_stim.pos == RIGHT):
                cor_response = 'c'

        # calculate response accuracy and points if a response is  made
        if (pressed) or (early_pressed):  
            if cor_response == response:
                acc = 1
            elif cor_response != response:
                acc = 0
            # update points, tiggers, and feedback display 
            if acc == 0:
                points = 0
                if early_pressed:
                    txt_stim.text = 'Incorrect! \n No points. \n Slow down!'
                elif pressed: 
                    txt_stim.text = 'Incorrect! \n No points.'
            elif acc == 1:
                if np.all(dis_stim.fillColor == HIGH): # high value reward
                    points = POINTS_HIGH
                elif np.all(dis_stim.fillColor == LOW): # low value reward
                    points = POINTS_LOW
                txt_stim.text = f'+ {points} points!'
        # if no response made, assign missing values etc.     
        elif (not pressed) & (not early_pressed):
            response = 999
            rt = 999
            acc = np.nan
            points = 0
            txt_stim.text = 'Too slow! No points.'

        if acc != 1:
            fdb_dur = FDB_DUR_LNG
        elif acc == 1:
            fdb_dur = FDB_DUR
        for frame in range(0, fdb_dur): # --------------------------------------------------- FDB ONSET

            if frame == 0:
                logging.warning('START_FDB')
                logging.flush()

            # draw stimulus
            txt_stim.draw()
            win.flip()

        # store accuracy data
        accList.append(acc)

    # take a break and save data at the end of the block --------------------------------------------------
    win.recordFrameIntervals = False
    accDict[f'{block}'] = accList

    txt_stim.text = f'''
    End of Practice Block {block}. \n\n 
    You got {int((np.nansum(np.array(accList))/NTRIALS)*100)}% of trials correct. \n\n
    Press space when you're ready to continue.''' 
    event.clearEvents()
    txt_idx = 0
    while True:

        # draw
        txt_stim.draw()
        win.flip()

        # collect user input to exit 
        pressed = event.getKeys(keyList = CONKEYS)
        if pressed:
            break

        # udpate frame
        txt_idx += 1 
    
# end experiment and shut everything down ----------------------------------------------------
event.clearEvents()
logging.warning('END_EXPERIMENT')
logging.flush()
txt_stim.text = 'End of practice session!'

txt_idx = 0
while True:

    # draw
    txt_stim.draw()
    win.flip()

    # collect user input to exit 
    pressed = event.getKeys(keyList = CONKEYS)
    if pressed:
        break

    # udpate frame
    instr_idx += 1 

# show accuracy for each block and overall
mean_acc = []
for block in accDict.keys():
    block_acc = np.nanmean(accDict[block][:])
    mean_acc.append(block_acc)
    print(f'Mean accuracy block {block}: {block_acc}%')
print(f'Mean accuracy overall: {np.mean(mean_acc)}%')

# close everything down
dp.DPxSetPPxDlpSeqPgrm('RGB')
dp.DPxDisableDoutPixelMode()
dp.DPxWriteRegCache()
dp.DPxClose()
win.close()
core.quit()









        


        



        
        











