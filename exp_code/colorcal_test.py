## This script is for testing the luminance of different stimuli using the ColorCAL Colorimeter from Erin Goddard. 
## Note to get colorcal to work on stimulus PC ##
# 1) Plug ColorCal into USB slot.
# 2) Check /dev folder for virtual serial port. It has shown up as ttyACM0 or ttyACM1 for me in the past. Didn't show up using certain USB extension cables
# 3) When it appears in the /dev folder, you may need to grant the user permission to acces it: 
# 4) Navigate to terminal, check permissions using 'ls -l ttyACM0' from within the /dev directory 
# 5) To change permissions, enter 'sudo chmod a+rw /dev/ttyACM0'. You'll then be prompted to enter the user password 


from colorcal import *
from psychopy import core, event, visual, logging, gui, event, monitors, data 
from propixx_functions import *
import numpy as np
from pypixxlib import _libdpx as dp
logging.console.setLevel(logging.WARNING)

# create window
win = visual.Window(
        screen = 1, 
        fullscr=1,
        pos = [0, 0],
        color = [0, 0, 0], # black
        units = 'pix', 
        colorSpace = 'rgb255',
        blendMode = 'avg')

# define colours
BLUE =   [37, 141, 165] 
ORANGE = [194, 99, 32]
WHITE = [255, 255, 255]
GREY = [119, 119, 119]
GREEN = [88, 129, 56]
PURPLE = [233, 84, 233]


# create stimuli
stim_params = { # common parameters used across stimuli
    'win': win, 
    'units': 'pix', 
    'opacity': 1,
    'contrast': 1,
    'colorSpace': 'rgb255'}
crc_stim = visual.Circle(
    radius = 100,
    edges = 100,
    pos=[0,100],
    lineWidth = 0,
    **stim_params)

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


# present stimulus
crc_stim.fillColor = [233, 84, 233]
crc_stim.draw()
win.flip()

# test 
colorCal = ColorCAL() 
ok, x, y, z = colorCal.measure()
colorCal.getNeedsCalibrateZero()
colorCal.getLum()
