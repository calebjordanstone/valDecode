## This script is to test for the presence of dithering when using the PROPixx ##

from pypixxlib._libdpx import DPxOpen, DPxClose, DPxGetVidLine, DPxUpdateRegCache
from psychopy import visual, core
import numpy as np

#connect to our hardware
DPxOpen()

#draw an onscreen window
win = visual.Window([1920, 1080], fullscr=True, pos=[0,0], color=[0,0,0], units='pix', colorSpace='rgb255', screen=0)
finalDitherCount = 0
for value in range(256):
 
    #draw a rectangle that occupies the top row of pixels
    line = visual.Line(win, start=(-960,540), end=(960,540), lineWidth=20, lineColor=(value, value, value), colorSpace='rgb255')
    line.draw()
    win.update()
 
    #register update to get most recent device status, followed by a vline
    DPxUpdateRegCache()
    core.wait(0.1)
    vline = DPxGetVidLine()
 
    #compare vline against expected results
    vlineArray = np.array(vline)
    compare = (vlineArray==value)
    dither = np.size(compare) - np.sum(compare)
    print('Test ', value,': ', dither, ' discrepancies')
 
    #keep track of total
    finalDitherCount = finalDitherCount+dither

print('Test complete, ', finalDitherCount,' discrepancies detected. If this value is >0, you may need to adjust graphics card settings')
win.close()
DPxClose()