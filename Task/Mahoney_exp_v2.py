#Lori Mahoney First Experiment (choose or accept/reject aid)
#Code based on Zinn's Thesis Experiment 
#Code based on Yamani & McCarley (2018) 

from psychopy.core import wait, quit, Clock
from psychopy.visual import TextStim, Circle, RatingScale, Window, Rect, Line, ImageStim
from numpy.random import exponential, random_integers, binomial, normal, uniform, choice
from psychopy.event import Mouse, getKeys, waitKeys
from psychopy import sound, gui, logging
import random
from psychopy import visual

#Set up common parameters
autoRT = .25
# meanRT = .676
# autoRT = exponential(meanRT)
mu = [2.2, 2.8] #Short,Long
probability = [.5,.5]  #prevalance of short/long
totalBlocks= 3 
trialsPerBlock = 40
numberSets = 3
totalBlocksinExp = 30

#Define functions
def aidResponse(sd, fa):
    # takes sd and fa and returns signal, automation's reponse and stimulus length
    signal = choice([0, 1], p = [0.5,0.5]) # 50/50 prevalence of short/long
    mu = [2.2, 2.8] #Short,Long
    if uniform(0, 1, 1) < fa: # %(1-fa) = false alarms for the automation
        if signal == 1:  # long bar
            autoResp = 1
        else: 
            autoResp = 0  # short bar
    else:
        if signal == 1:
            autoResp = 0
        else:
            autoResp = 1
    StimLength = normal(mu[signal], sd) 
    return (signal, autoResp, StimLength)

def displayRecordResponse(on, trialStart, autoResp, StimLength):
   # take autoResp and trialStart and returns humanResp and humanRT
    while True:
        (button, time) = myMouse.getPressed(getTime = True)
        autoRT = 0.25
        if on > 0:
            if clock.getTime() > (trialStart+autoRT):
                if autoResp == 1:
                    target2 = Rect(win, units = "deg", width = 0.5, height = StimLength, fillColor = "FireBrick", lineColor = "FireBrick", pos = targpos)
                else:
                    target2 = Rect(win, units = "deg", width = 0.5, height = StimLength, fillColor = "Lime", lineColor = "Lime", pos = targpos)
                background.draw()
                target2.draw()
                win.flip()
        #Record response (either left or right click)
        if button[0]: #Left click
            humanResp = 0
            humanRT = time[0]
            break
        elif button[2]: #Right click
            humanResp = 1
            humanRT = time[2]
            break
    return (humanResp, humanRT)
   
def decideFeedback (on, humanResp, signal):
    # takes on/off condition, humanResp and signal and return trialACC and feedback image
    if on<2:
        if humanResp == signal:
            trialACC = 1
            feedback = TextStim(win, text="""+""", units='deg', pos =[0, 0], height=1, color='White')
        else:
            trialACC = 0
            feedback = TextStim(win, text="""x""", units='deg', pos =[0, 0], height=1, color='Red')
    elif on==2:
        if (autoResp == signal) and (humanResp == 0):
            trialACC = 1
            feedback = TextStim(win, text="""+""", units='deg', pos =[0, 0], height=1, color='White')
        elif (autoResp != signal) and (humanResp == 1):
            trialACC = 1
            feedback = TextStim(win, text="""+""", units='deg', pos =[0, 0], height=1, color='White')
        else:
            trialACC = 0
            feedback = TextStim(win, text="""x""", units='deg', pos =[0, 0], height=1, color='Red')
    return (trialACC, feedback)

clock = Clock()
#Inputs for the participants
info = {'ID': '99','Gender':['Male','Female'], 'Age':'', 'Order':['1','2','3','4']}
infoDlg = gui.DlgFromDict(dictionary=info, order=['ID', 'Gender', 'Age','Order'])
                                #Order Conditions: 
                                #1 = LE, LD, HE, HD; L-H, E-D
                                #2 = LD, LE, HD, HE; L-H, D-E
                                #3 = HE, HD, LE, LD; H-L, E-D
                                #4 = HD, HE, LD, LE; H-L, D-E

#Setting the conditions based off the inputs: 
#Order of FA and Difficulty: 
if info['Order'] == "1":
    randnum= 0 # low FA first
    fa = [0.95, 0.8]
    b = 0  #easy first
    sd = [0.15, 0.3]
if info['Order'] == "2":
    randnum= 0 
    fa = [0.95, 0.8]
    b = 1
    sd = [0.3, 0.15]
if info['Order'] == "3":
    randnum = 1
    fa = [0.8, 0.95]
    b = 0
    sd = [0.15, 0.3]
if info['Order'] == "4":
    randnum = 1
    fa = [0.8, 0.95]
    b = 1
    sd = [0.3, 0.15]


if int(info['ID'])%2 == 1:  #order of choice on or off (baseline = no aid, on = select an answer, off = agree/disagree with automation)
    a = [0, 1, 2]  # baseline, on, off 
else:
    a = [2, 0, 1]  # off, baseline, on
    
#Equipment set-up:
win = Window([1028, 768], monitor = "testMonitor", units = 'deg', allowGUI=False, color=(-.5, -.5, -.5), fullscr=True)
myMouse = Mouse()

#Output file set-up
filename = 'Mahoney_exp_' + info['ID'] + '.txt'
logClicks = logging.LogFile(filename,
        filemode='a',#if you set this to 'a' it will append instead of overwriting
        level=logging.CRITICAL)
message = 'ID\tgender\torder\texperiment\tdifficulty\tfalseAlarm\ttrial\tcondition\ttrial.start.time\tsignal\thumanResp\thumanRT\tautoResp\tautoRT\tACC\tstimlength\tRTor\tRTand\n'
logClicks.write(message)

pracfilename = 'Mahoney_exp_practice_' + info['ID'] +'.txt'
logClicksPrac = logging.LogFile(pracfilename,
        filemode = 'a',
        level = logging.CRITICAL)
message2 = 'ID\tage\tgender\torder\texperiment\ttrial\ttrial.start.time\tsignal\thumanResp\thumanRT\tautoResp\tautoRT\ttrialACC\tstimlength\n'
logClicksPrac.write(message2)

# Object Setup
positions = [0, 0]
startButton = Circle(win, units = "deg", radius = 1, lineWidth = .5, lineColor = (1, 1, 1), fillColor = (0, 0, 0))
fixCross = Circle(win, units = "deg", radius = 1, lineColor = (1, 1, 1), fillColor = (0, 0, 0))
background = Rect(win, units = "deg", width = 13, height = 13, fillColor = (0,0,0), lineColor = (0,0,0))
placeholder = Circle(win, radius = 0.5, fillColor = (-.5,-.5,-.5), lineColor = (-.5,-.5,-.5), pos = [0,6])

#Introduction Slides

intromes = TextStim(win, text="""Welcome to the Production Decision Experiment""", wrapWidth=20, units='deg', pos =[0, 10], height=.75,color='White')

descriptionmes = TextStim(win, text="""In this simulated work environment, you are an operator and must make decisions regarding whether or not to attempt production. Your decision should be based on the strength of the raw material you are given, as represented by the length of a vertical rectangular bar presented on the display. A longer bar is weaker and a shorter bar is stronger.

Your task is to report whether or not to attempt production. Longer bars indicate a failure and should not be used in production. Shorter bars indicate a success and should be used in production. There is some variability in the length of short and long bars, but the probability of a short or long bar occurring remains constant across trials. 

Two different automated aids are provided to assist you in your decision. When the aid classifies a vertical bar as short, the bar will be colored green. When the aid classifies a vertical bar as too long, the bar will be colored red. The automated aid's judgement will display on the vertical bar of raw materials. One aid is reliable, but not perfect, while the other is less reliable. 

For two-thirds of the trials you will directly choose whether or not to attempt production by pressing the left mouse button to attempt production and the right mouse button to refrain from production. For the other third of the trials you will press the left mouse button to accept the automated aid's decision (attempt production for short bars or refrain from production for long bars) or the right mouse button to reject the automated aid's decision. There will be a block of practice trials before each condition.

For all trials make your decision as quickly and accurately as possible. Following each decision, you will see + if you have made a correct decision or an x if you have made an incorrect decision.

You may take a break between blocks if you need one. There are 30 blocks in total. On the next slide you will see examples of long and short bars with the automated aid display format. 

Please press any key to continue.""", wrapWidth=30, units='deg', pos =[0, -2], height=.6,color='White')

intromes.draw()
descriptionmes.draw()
win.flip()
waitKeys()

#Setting up the examples and questions:
sample_aid_short = ImageStim(win, image = "C:\Users\lasch\OneDrive\Documents\Wright State\Research\Cara_experiment_code\intShort.png", pos = [-7, 0])
sample_aid_long = ImageStim(win, image = "C:\Users\lasch\OneDrive\Documents\Wright State\Research\Cara_experiment_code\intLong.png", pos = [7, 0])
aid = TextStim(win, text="""Examples of long and short bars with automated aid \n \nPress any key to continue""", wrapWidth=40, units='deg', pos =[0, 10], height=1,color='White')

sample_aid_long.draw()
sample_aid_short.draw()
aid.draw()
win.flip()
waitKeys()

questions = TextStim(win, text="""If you have any questions, please ask the researcher now.\n\nPress any key to start""", wrapWidth=20, units='deg', pos =[0, 0], height=1,color='White')
questions.draw()
win.flip()
waitKeys()

block_counter = 1

#Messages
baseline= TextStim(win, text="""Please read carefully. \n\nIn this series of blocks, no aid is available. \n\nUse the left mouse button to attempt production or the right mouse button to refrain from production. \n\nPress any key to continue""", wrapWidth=20, units='deg', pos =[0, 6], height=1,color='White')
choiceON= TextStim(win, text="""Please read carefully. \n\nIn this series of blocks, an aid is available. \n\nUse the left mouse button to attempt production or the right mouse button to refrain from production. \n\nPress any key to continue""", wrapWidth=20, units='deg', pos =[0, 6], height=1,color='White')
choiceOFF = TextStim(win, text="""Please read carefully. \n\nIn this series of blocks, an aid is available. \n\nUse the left mouse button to agree with the aid's decision or the right mouse button to disagree with the decision. \n\nPress any key to continue.""", wrapWidth=20, units='deg', pos =[0, 6], height=1,color='White')
expintro_low = TextStim(win, text="""In this series of trials, Automated Aid Alpha will provide the recommendation to assist you in your decision. \n\nPress any key to continue.""", wrapWidth=20, units='deg', pos =[0, 3], height=1,color='White')
expintro_high = TextStim(win, text="""In this series of trials, Automated Aid Beta will provide the recommendation to assist you in your decision. \n\nPress any key to continue.""", wrapWidth=20, units='deg', pos =[0, 3], height=1,color='White')
expintro = TextStim(win, text="""Practice Block \n\nPress any key to continue""", wrapWidth=20, units='deg', pos =[0, 3], height=1,color='White')
meg = TextStim(win, text="""Click the circle to start.""", wrapWidth=20, units='deg', pos =[0, 3], height=1,color='White')


expStartTime = clock.getTime()
for condition in range(0,3):
    if a[condition] == 0:
        megchoice = baseline
        cond = 'NO AID'
        startPracticeTrials = 1
        endPracticeTrials = 21
        megprac= TextStim(win, text="""Practice Block. \n\nUse the left mouse button to attempt production or the right mouse button to refrain from production.""", wrapWidth=25, units='deg', pos =[0, 8], height=1,color='White')
    elif a[condition] == 1:
        megchoice = choiceON
        cond = 'DECIDE'
        startPracticeTrials = 1
        endPracticeTrials = 1
        megprac= TextStim(win, text=""" """, wrapWidth=25, units='deg', pos =[0, 6], height=1,color='White')
    elif a[condition] == 2:
        megchoice = choiceOFF
        cond = 'REJECT'
        startPracticeTrials = 1
        endPracticeTrials = 21
        megprac = TextStim(win, text="""Practice Block. \n\nUse the left mouse button to agree with the aid's decision or the right mouse button to disagree with the decision.""", wrapWidth=25, units='deg', pos =[0, 8], height=1,color='White')    
    megchoice.draw()
    # Practice Block 
    win.flip()
    waitKeys()
    win.mouseVisible = True
    megprac.draw()
    meg.draw()
    startButton.draw()
    win.flip()
    while myMouse.isPressedIn(startButton) == False:
        pass
    for trial in range(startPracticeTrials, endPracticeTrials):
        win.mouseVisible = False
        #Create the Aid's response
        (signal, autoResp, StimLength) = aidResponse(.15, .95)  #easy, low FA
        wait(.5)
        #Trial begins with the onset of pointers
        trialStart = clock.getTime()
        myMouse.clickReset()
        background.draw()
        #Draw target
        targpos = [random.uniform(-1, 1), random.uniform(-1, 1)]
        target = Rect(win, units = "deg", width = 0.5, height = StimLength, fillColor = (-1,-1,-1), lineColor = (-1,-1,-1), pos = targpos)
        target.draw()
        win.flip()
        (humanResp, humanRT) = displayRecordResponse(a[condition], trialStart, autoResp, StimLength)
        #Feedback
        (trialACC, feedback) = decideFeedback(a[condition], humanResp, signal)
        feedback.draw()
        win.flip()
        wait(.5)
        experiment = 'prac'
        message = '{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t\n'\
        .format(info['ID'], info['Age'], info['Gender'], info['Order'], experiment, trial, trialStart, signal, humanResp, humanRT, autoResp, autoRT, trialACC, StimLength)
        logClicksPrac.write(message)
#    pracintro = TextStim(win, text="""End Practice Block. Press any key to continue.""", wrapWidth=20, units='deg', pos =[0, 3], height=1,color='White')
#    pracintro.draw()
#    win.flip()
#    waitKeys()
    if a[condition]==0:
        falseA = 'n/a'
        experiment = 'n/a'
        for sd_num in range(0,2):
            if b == 0:  #Set up order of Easy and Difficult Conditions
                if sd_num == 0:
                    difficulty = 'easy'
                else:
                    difficulty = 'hard'
            else:
                if sd_num == 0:
                    difficulty = 'hard'
                else:
                    difficulty = 'easy'
            for block in range (1, totalBlocks+1):          #Set up trial counter
                win.mouseVisible = True
                startTrial = 1 + (block - 1)*trialsPerBlock
                endTrial = startTrial + trialsPerBlock - 1
                bc = str(block_counter) + "/" + str(totalBlocksinExp)
                Block_counter = TextStim(win, text= bc, wrapWidth=20, units='deg', pos =[0, 10], height=1,color='Gainsboro')
                Block_counter.draw()
                meg.draw()
                startButton.draw()
                win.flip()
                while myMouse.isPressedIn(startButton) == False:
                    pass
                #Trial Creation
                for trial in range(startTrial, endTrial + 1):
                    win.mouseVisible = False
                    (signal, autoResp, StimLength) = aidResponse(sd[sd_num], .95)
                    wait(.5)
                    #Trial begins with the onset of pointers
                    trialStart = clock.getTime()
                    myMouse.clickReset()
                    background.draw()
                    #Draw target
                    targpos = [random.uniform(-1, 1), random.uniform(-1, 1)]
                    target = Rect(win, units = "deg", width = 0.5, height = StimLength, fillColor = (-1,-1,-1), lineColor = (-1,-1,-1), pos = targpos)
                    target.draw()
                    win.flip()
                    for keys in getKeys(): #this allows to kick out of experiment.
                        if keys in ['escape', 'q']:
                            msg = TextStim(win, text = "All done!", wrapWidth=15, units='deg', height=1, color='White', pos=[0, 0], alignHoriz='center',alignVert='bottom')
                            msg.draw()
                            win.flip()
                            wait(.5)
                            win.close()
                            quit()
                        if keys in ['x']: #this allows to screenshot the stimuli.
                            win.getMovieFrame()
                            win.saveMovieFrames('SampleStim' + str(trial) + '.jpg')
                    (humanResp, humanRT) = displayRecordResponse(a[condition], trialStart, autoResp, StimLength)
                    #Feedback
                    (trialACC, feedback) = decideFeedback (a[condition], humanResp, signal)                
                    feedback.draw()
                    win.flip()
                    wait(.5)
                    RTor = min(humanRT, autoRT)
                    RTand = max(humanRT, autoRT)
                    message = '{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\n'\
                        .format(info['ID'], info['Gender'], info['Order'], experiment, difficulty, falseA, trial, cond, trialStart, signal, humanResp, humanRT, autoResp, autoRT, trialACC, StimLength, RTor, RTand)
                    logClicks.write(message)
                block_counter += 1
            wait
    else:
        if randnum == 0:                                   #randnum 0 = Low FA > High FA
            for experiment in range (0,2):
                if experiment == 0:             # Intro Message for FA level
                    expintro_low.draw()
                    falseA = 'low'
                    win.flip()
                    waitKeys()
                else:
                    expintro_high.draw()
                    falseA = 'high'
                    win.flip()
                    waitKeys() 
                for sd_num in range(0,2):
                    if b == 0:  #Set up order of Easy and Difficult Conditions
                        if sd_num == 0:
                            difficulty = 'easy'
                        else:
                            difficulty = 'hard'
                    else:
                        if sd_num == 0:
                            difficulty = 'hard'
                        else:
                            difficulty = 'easy'
                    for block in range (1, totalBlocks+1):          #Set up trial counter
                        win.mouseVisible = True
                        startTrial = 1 + (block - 1)*trialsPerBlock
                        endTrial = startTrial + trialsPerBlock - 1
                        bc = str(block_counter) + "/" + str(totalBlocksinExp)
                        Block_counter = TextStim(win, text= bc, wrapWidth=20, units='deg', pos =[0, 10], height=1,color='Gainsboro')
                        Block_counter.draw()
                        meg.draw()
                        startButton.draw()
                        win.flip()
                        while myMouse.isPressedIn(startButton) == False:
                            pass
                        #Trial Creation
                        for trial in range(startTrial, endTrial + 1):
                            win.mouseVisible = False
                            (signal, autoResp, StimLength) = aidResponse(sd[sd_num], fa[experiment])
                            wait(.5)
                            #Trial begins with the onset of pointers
                            trialStart = clock.getTime()
                            myMouse.clickReset()
                            background.draw()
                            #Draw target
                            targpos = [random.uniform(-1, 1), random.uniform(-1, 1)]
                            target = Rect(win, units = "deg", width = 0.5, height = StimLength, fillColor = (-1,-1,-1), lineColor = (-1,-1,-1), pos = targpos)
                            target.draw()
                            win.flip()
                            for keys in getKeys(): #this allows to kick out of experiment.
                                if keys in ['escape', 'q']:
                                    msg = TextStim(win, text = "All done!", wrapWidth=15, units='deg', height=1, color='White', pos=[0, 0], alignHoriz='center',alignVert='bottom')
                                    msg.draw()
                                    win.flip()
                                    wait(.5)
                                    win.close()
                                    quit()
                                if keys in ['x']: #this allows to screenshot the stimuli.
                                    win.getMovieFrame()
                                    win.saveMovieFrames('SampleStim' + str(trial) + '.jpg')
                            (humanResp, humanRT) = displayRecordResponse(a[condition], trialStart, autoResp, StimLength)
                            #Feedback
                            (trialACC, feedback) = decideFeedback (a[condition], humanResp, signal)                
                            feedback.draw()
                            win.flip()
                            wait(.5)
                            RTor = min(humanRT, autoRT)
                            RTand = max(humanRT, autoRT)
                            message = '{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\n'\
                                .format(info['ID'], info['Gender'], info['Order'], experiment, difficulty, falseA, trial, cond, trialStart, signal, humanResp, humanRT, autoResp, autoRT, trialACC, StimLength, RTor, RTand)
                            logClicks.write(message)
                        block_counter += 1
                    wait (1)
        else:                                           #randnum 1 = High FA > Low FA
            for experiment in range(0,2):
                if experiment == 0:             # Intro Message for FA level
                    expintro_high.draw()
                    falseA = 'high'
                    win.flip()
                    waitKeys()
                elif experiment == 1:
                    expintro_low.draw()
                    falseA = 'low'
                    win.flip()
                    waitKeys() 
                for sd_num in range(0,2):
                    if b == 0:  #Set up order of Easy and Difficult Conditions
                        if sd_num == 0:
                            difficulty = 'easy'
                        else:
                            difficulty = 'hard'
                    else:
                        if sd_num == 0:
                            difficulty = 'hard'
                        else:
                            difficulty = 'easy'
                    for block in range (1, totalBlocks+1):
                        win.mouseVisible = True
                        startTrial = 1 + (block - 1)*trialsPerBlock
                        endTrial = startTrial + trialsPerBlock -1
                        bc = str(block_counter) + "/" + str(totalBlocksinExp)
                        Block_counter = TextStim(win, text= bc, wrapWidth=20, units='deg', pos =[0, 10], height=1,color='Gainsboro')
                        Block_counter.draw()
                        meg.draw()
                        startButton.draw()
                        win.flip()
                        while myMouse.isPressedIn(startButton) == False:
                            pass
                        for trial in range(startTrial, endTrial + 1):
                            win.mouseVisible = False
                            (signal, autoResp, StimLength) = aidResponse(sd[sd_num], fa[experiment])
                            wait(.5)
                            #Trial begins with the onset of pointers
                            trialStart = clock.getTime()
                            myMouse.clickReset()
                            background.draw()
                            targpos = [random.uniform(-1, 1), random.uniform(-1, 1)]
                            target = Rect(win, units = "deg", width = 0.5, height = StimLength, fillColor = (-1,-1,-1), lineColor = (-1,-1,-1), pos = targpos)
                            target.draw()
                            win.flip()
                            for keys in getKeys(): #this allows to kick out of experiment.
                                if keys in ['escape', 'q']:
                                    msg = TextStim(win, text = "All done!", wrapWidth=15, units='deg', height=1, color='White', pos=[0, 0], alignHoriz='center',alignVert='bottom')
                                    msg.draw()
                                    win.flip()
                                    wait(.5)
                                    win.close()
                                    quit()
                                if keys in ['x']: #this allows to screenshot the stimuli.
                                    win.getMovieFrame()
                                    win.saveMovieFrames('SampleStim' + str(trial) + '.jpg')
                            (humanResp, humanRT) = displayRecordResponse(a[condition], trialStart, autoResp, StimLength)
                            #Feedback
                            (trialACC, feedback) = decideFeedback (a[condition], humanResp, signal)
                            feedback.draw()
                            win.flip()
                            wait(.5)
                            RTor = min(humanRT, autoRT)
                            RTand = max(humanRT, autoRT)
                            message = '{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\n'\
                                .format(info['ID'], info['Gender'], info['Order'], experiment, difficulty, falseA, trial, cond, trialStart, signal, humanResp, humanRT, autoResp, autoRT, trialACC, StimLength, RTor, RTand)
                            logClicks.write(message)
                        block_counter += 1
                    wait (1)
                
wait(1)
msg = TextStim(win, text = "All done!", wrapWidth=15, units='deg', height=1,color='White', alignHoriz='center',alignVert='bottom')
msg.draw()
win.flip()
wait(2)
quit()
