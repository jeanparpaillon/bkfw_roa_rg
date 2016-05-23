#!/usr/bin/python
"""BKTEL PHOTONICS
Title   = 16x2 characters display control
Author  = Cyril Le Goas, Jean Parpaillon
Date    = 04/05/2016"""
version = '1.4'

imports = {}

import sys
import time
try:
  import Adafruit_CharLCD as LCD
  imports['LCD'] = True
except:
  print "WARN: LCD control disabled"
  imports['LCD'] = False

try:
  import RPi.GPIO as GPIO
  imports['GPIO'] = True
except:
  print "WARN: GPIO disabled"
  imports['GPIO'] = False
  
import requests

# Modes
OFF = 0
CC	= 1
PC	= 2
GC	= 3

# LCD INIT
lcd_rs        = 25
lcd_en        = 24
lcd_d4        = 23
lcd_d5        = 17
lcd_d6        = 21
lcd_d7        = 22
lcd_backlight = None
lcd_columns   = 16
lcd_rows      = 2
lcd           = None

# NAVIGATION BUTTONS INIT
Button_TOP	= 2
Button_BOT	= 3
Button_LEFT	= 4
Button_RIGHT	= 10
Button_SET	= 9

# CLASS
class FakeLCD(object):
  def __init__(self):
    
    return

  def clear(self):
    return

  def message(self, message):
    print message
    return

  def set_cursor(self, X, Y):
    return
  

class Client(object):

  def __init__(self, baseurl):
    self.baseurl = baseurl
    self.json = {}

  def url(self):
    return self.baseurl

  def load(self):
    ret = requests.get(self.url())
    self.json = ret.json()
    

class Param(Client):
  """
  .time_stamp: last time value
  .screen_update: define if an update is needed or not
  .current_screen: screen postition 
  .has_PC_mode: define if the unit have a PC mode 
  .has_GC_mode: define if the unit have a GC mode
  .has_input_PD: define if the unit have an input PD or not
  .has_output_PD: define is the unit have an output PD or not
  .number_of_edfa: define the number of edfa in the rack
  """

  def __init__(self, baseurl):
    super(Param, self).__init__(baseurl)
    
    self.time_stamp        = time.time()
    self.screen_update     = True
    self.current_screen    = 0
    self.has_PC_mode       = True
    self.has_GC_mode       = True
    self.has_input_PD      = True
    self.has_output_PD     = True
    self.number_of_edfa    = 1
    
    self.load()

  def url(self):
    return self.baseurl + "/api/edfa"
    

class Info(Client):
  """
  .serialnum : unit serial number
  .partnum : unit part number
  .date : production date
  .vendor : vendor name, string used during boot
  .hard : hardware version
  .soft : software version
  """

  def __init__(self, baseurl):
    super(Info, self).__init__(baseurl)
    
    self.serialnum	= 'TBD'
    self.partnum	= 'TBD'
    self.date	    = 'TBD'
    self.vendor	    = '1'
    self.hard	    = 'TBD'
    self.soft	    = 'TBD'
    self.load()

  def url(self):
    return self.baseurl + "/api/mcu/1"

  def load(self):
    super(Info, self).load()
    self.serialnum = self.json['serialNum']
    self.partnum = self.json['partNum']
    self.date = self.json['productDate']
    self.vendor = self.json['vendor']
    self.hard = self.json['hwVer']
    self.soft = self.json['swVer']


class Screen(object):
  """
  .page_position: current page position
  .string_page1: default string for page 1
  .string_page2: default string for page 2
  .string_page3: default string for page 3 
  .string_page4: default string for page 4
  .string_page5: default string for page 5 
  .number_of_page: define the number of page for the screen X
  .cursor_X: cursor position X
  .cursor_Y: cursor position Y
  .TB_enabled: define if the top and bot buttons are enabled
  .SET_enabled: define if the set button is enabled
  """

  def __init__(self, string_page1, string_page2, string_page3, string_page4, string_page5,
               number_of_page, cursor_X, cursor_Y, TB_enabled, SET_enabled):
    self.page_position 	= 1
    self.string_page1	= string_page1
    self.string_page2	= string_page2
    self.string_page3	= string_page3
    self.string_page4	= string_page4
    self.string_page5	= string_page5
    self.number_of_page	= number_of_page
    self.cursor_X      	= cursor_X
    self.cursor_Y      	= cursor_Y
    self.TB_enabled    	= TB_enabled
    self.SET_enabled   	= SET_enabled

class EDFA(Client):
  """
  .mode: edfa operating mode
  .max_current_LD1: edfa maximum laser 1 current
  .max_current_LD2: edfa maximum laser 2 current
  .min_pc: edfa minimum pc setpoint
  .max_pc: edfa maximum pc setpoint
  .min_gc: edfa minimum gc setpoint
  .max_gc: edfa maximum gc setpoint
  .number_of_laser: number of optical stage
  .has_settable_LD1 : define if LD1 bias current is settable by the customer
  .alarms: edfa alarms
  .LD1_current: laser 1 current
  .LD2_current: laser 2 current
  .input_power: edfa optical input power
  .output_power: edfa optical output power
  .internal_temp: edfa internal temperature
  .CC1_setpoint: laser 1 current setpoint
  .CC2_setpoint: laser 2 current setpoint
  .PC_setpoint: pc mode setpoint
  .GC_setpoint: gc mode setpoint
  """

  def __init__(self, index, baseurl):
    super(EDFA, self).__init__(baseurl)

    self.index = index
    
    self.mode = OFF
    self.max_current_LD1 	= 250.0
    self.max_current_LD2 	= 5000.0
    self.min_pc 		= 0.0
    self.max_pc		= 23.0
    self.min_gc		= 0.0
    self.max_gc		= 33.0
    self.number_of_laser	= 2
    self.has_settable_LD1	= True
    self.alarm		= None
    self.LD1_current	= 0.0
    self.LD2_current	= 0.0
    self.input_power	= 0.0
    self.output_power	= 21.0
    self.internal_temp	= 25.0
    self.CC1_setpoint       = 150
    self.CC2_setpoint       = 5000
    self.PC_setpoint        = 25.5
    self.GC_setpoint        = 20.5

    self.load()

  def url(self):
    return self.baseurl + "/api/mcu/%d" % (self.index)

  def load(self):
    super(EDFA, self).load()

    self.mode = self.json['operatingMode']
    self.max_current_LD1 	= 250.0
    self.max_current_LD2 	= 5000.0
    self.min_pc 		= 0.0
    self.max_pc		= 23.0
    self.min_gc		= 0.0
    self.max_gc		= 33.0
    self.number_of_laser	= 2
    self.has_settable_LD1	= True
    self.alarm		= None
    self.LD1_current	= 0.0
    self.LD2_current	= 0.0
    self.input_power	= 0.0
    self.output_power	= 21.0
    self.internal_temp	= 25.0
    self.CC1_setpoint       = 150
    self.CC2_setpoint       = 5000
    self.PC_setpoint        = 25.5
    self.GC_setpoint        = 20.5
  

# FUNCTIONS
def draw_screen(lcd, type):
  lcd.clear()

  if type.page_position == 1:
    lcd.message(type.string_page1)
  elif type.page_position == 2:
    lcd.message(type.string_page2)
  elif type.page_position == 3:
    lcd.message(type.string_page3)
  elif type.page_position == 4:
    lcd.message(type.string_page4)
  elif type.page_position == 5:
    lcd.message(type.string_page5)

  return 

def vendor_to_string(nb):
  switcher = {
		  1: ' Bktel\n      Photonics ',
		  2: '   Laser 2000   ',
		  3: '     Alnair     ',
		  4: '   Infractive   '
  }
  return switcher.get(nb,'vendor not\nconfigured')

def string_to_bool(string):
  return string == 'True'

def fill_with_blank(lcd, value):
  if value >= 10:
    lcd.message(' ')
  elif value >= 0 and value < 10:
    lcd.message('  ')
  elif value > -10 and value < 0:
    lcd.message(' ')

def screen_edfa1_monitoring(lcd, tab, params, edfa):
  if edfa.number_of_laser == 1:	
    if tab[0].page_position == 1: 
      if params.has_input_PD == True  and params.has_output_PD == True:
        lcd.set_cursor(5,0)
        fill_with_blank(lcd, edfa.input_power)
        lcd.message('%2.1f' % (edfa.input_power))
        lcd.set_cursor(5,1)
        fill_with_blank(lcd, edfa.output_power)
        lcd.message('%2.1f' % (edfa.output_power))
      elif params.has_input_PD == False and params.has_output_PD == True:
        lcd.set_cursor(5,0)
        fill_with_blank(lcd, edfa.output_power)
        lcd.message('%2.1f' % (edfa.output_power))
        lcd.set_cursor(6,1)
        lcd.message('%5.0f' % (edfa.LD1_current))
      elif params.has_input_PD == True  and params.has_output_PD == False:
        lcd.set_cursor(5,0)
        fill_with_blank(lcd, edfa.input_power)
        lcd.message('%2.1f' % (edfa.input_power))
        lcd.set_cursor(6,1)
        lcd.message('%5.0f' % (edfa.LD1_current))
      elif params.has_input_PD == False and params.has_output_PD == False:
        lcd.set_cursor(6,0)
        lcd.message('%5.0f' % (edfa.LD1_current))
        lcd.set_cursor(5,1)
        fill_with_blank(lcd, edfa.internal_temp)
        lcd.message('%2.1f' % (edfa.internal_temp))
    elif tab[0].page_position == 2:
      if params.has_input_PD == True  and params.has_output_PD == True:
        lcd.set_cursor(6,0)
        lcd.message('%5.0f' % (edfa.LD1_current))
        lcd.set_cursor(5,1)
        fill_with_blank(lcd, edfa.internal_temp)
        lcd.message('%2.1f' % (edfa.internal_temp))
      else:
        lcd.set_cursor(5,0)
        fill_with_blank(lcd, edfa.internal_temp)
        lcd.message('%2.1f' % (edfa.internal_temp)) 
  elif edfa.number_of_laser == 2:
    if tab[0].page_position == 1:
      if params.has_input_PD == True  and params.has_output_PD == True:
        lcd.set_cursor(5,0)
        fill_with_blank(lcd, edfa.input_power)
        lcd.message('%2.1f' % (edfa.input_power))
        lcd.set_cursor(5,1)
        fill_with_blank(lcd, edfa.output_power)
        lcd.message('%2.1f' % (edfa.output_power))
      elif params.has_input_PD == False and params.has_output_PD == True:
        lcd.set_cursor(5,0)
        fill_with_blank(lcd, edfa.output_power)
        lcd.message('%2.1f' % (edfa.output_power))
        lcd.set_cursor(6,1)
        lcd.message('%5.0f' % (edfa.LD1_current))
      elif params.has_input_PD == True  and params.has_output_PD == False:
        lcd.set_cursor(5,0)
        fill_with_blank(lcd, edfa.input_power)
        lcd.message('%2.1f' % (edfa.input_power))
        lcd.set_cursor(6,1)
        lcd.message('%5.0f' % (edfa.LD1_current))
      elif params.has_input_PD == False and params.has_output_PD == False:
        lcd.set_cursor(6,0)
        lcd.message('%5.0f' % (edfa.LD1_current))
        lcd.set_cursor(6,1)
        lcd.message('%5.0f' % (edfa.LD2_current))
    elif tab[0].page_position == 2:
      if params.has_input_PD == True  and params.has_output_PD == True:
        lcd.set_cursor(6,0)
        lcd.message('%5.0f' % (edfa.LD1_current))
        lcd.set_cursor(6,1)
        lcd.message('%5.0f' % (edfa.LD2_current))
      elif params.has_input_PD == False and params.has_output_PD == True:
        lcd.set_cursor(6,0)
        lcd.message('%5.0f' % (edfa.LC2_current))
        lcd.set_cursor(5,1)
        fill_with_blank(lcd, edfa.internal_temp)
        lcd.message('%2.1f' % (edfa.internal_temp))
      elif params.has_input_PD == True  and params.has_output_PD == False:
        lcd.set_cursor(6,0)
        lcd.message('%5.0f' % (edfa.LC2_current))
        lcd.set_cursor(5,1)
        fill_with_blank(lcd, edfa.internal_temp)
        lcd.message('%2.1f' % (edfa.internal_temp))
      elif params.has_input_PD == False and params.has_output_PD == False:
        lcd.set_cursor(5,0)
        fill_with_blank(lcd, edfa.internal_temp)
        lcd.message('%2.1f' % (edfa.internal_temp))
    elif tab[0].page_position == 3:
      lcd.set_cursor(5,0)
      fill_with_blank(lcd, edfa.internal_temp)
      lcd.message('%2.1f' % (edfa.internal_temp))

def screen_edfa2_monitoring(lcd, tab, params, edfa):
  if edfa.number_of_laser == 1:
    if tab[2].page_position == 1:
      lcd.set_cursor(1,1)
      if edfa.mode == OFF:
        lcd.message('OFF')
      elif edfa.mode == CC:
		lcd.message(' CC')
      elif edfa.mode == PC:
		lcd.message(' PC')
      elif edfa.mode == GC:
		lcd.message(' GC')			
    elif tab[2].page_position == 2:
      lcd.set_cursor(0,1)
      lcd.message('%5.0f' % (edfa.CC1_setpoint))
    elif tab[2].page_position == 3:
      lcd.set_cursor(0,1)
      if params.has_PC_mode == True :
        fill_with_blank(lcd, edfa.PC_setpoint)
        lcd.message('%2.1f' % (edfa.PC_setpoint))
      elif params.has_GC_mode == True:
        fill_with_blank(lcd, edfa.GC_setpoint)
        lcd.message('%2.1f' % (edfa.GC_setpoint))
    elif tab[2].page_position == 4:
      lcd.set_cursor(0,1)
      fill_with_blank(lcd, edfa.GC_setpoint)
      lcd.message('%2.1f' % (edfa.GC_setpoint))			
  elif edfa.number_of_laser == 2:
    if tab[2].page_position == 1:
      lcd.set_cursor(1,1)
      if edfa.mode == OFF:
        lcd.message('OFF')
      elif edfa.mode == CC:
        lcd.message(' CC')
      elif edfa.mode == PC:
        lcd.message(' PC')
      elif edfa.mode == GC:
        lcd.message(' GC')
    elif tab[2].page_position == 2:
      lcd.set_cursor(0,1)
      lcd.message('%5.0f' % (edfa.CC1_setpoint))
    elif tab[2].page_position == 3:
      lcd.set_cursor(0,1)
      lcd.message('%5.0f' % (edfa.CC2_setpoint))
    elif tab[2].page_position == 4:
      lcd.set_cursor(0,1)
      if params.has_PC_mode == True :
        fill_with_blank(lcd, edfa.PC_setpoint)
        lcd.message('%2.1f' % (edfa.PC_setpoint))
      elif params.has_GC_mode == True:
        fill_with_blank(lcd, edfa.GC_setpoint)
        lcd.message('%2.1f' % (edfa.GC_setpoint))
    elif tab[2].page_position == 5:
      lcd.set_cursor(0,1)
      fill_with_blank(lcd, edfa.GC_setpoint)
      lcd.message('%2.1f' % (edfa.GC_setpoint))
	  #GC only

def screen_edfa_alarms(lcd, edfas):
  lcd.set_cursor(0,0)
   
def screen_rack_info(lcd, infos):
  lcd.set_cursor(4,0)
  lcd.message('%s' % (infos.serialnum))
  lcd.set_cursor(5,1)
  lcd.message('%s' % (infos.partnum))
   
def screen_update(lcd, tab, params, infos, edfas):	#need an update for more edfa
  for i in edfas:
    edfas[i].load()
  
  if params.current_screen == 0: #EDFA1 MONITORING
    screen_edfa1_monitoring(lcd, tab, params, edfas[1])
  elif params.current_screen == 2:
    screen_edfa2_monitoring(lcd, tab, params, edfas[1])
  elif params.current_screen == 4: #EDFA1 ALARMS
    screen_edfa_alarms(lcd, edfas)
  elif params.current_screen == 6: #RACK INFO
    screeen_rack_info(lcd, infos)

def lcd_init():
  if not imports['LCD']:
    return FakeLCD()
  
  lcd = LCD.Adafruit_CharLCD(lcd_rs, lcd_en, lcd_d4, lcd_d5, lcd_d6, lcd_d7,
                             lcd_columns, lcd_rows, lcd_backlight)
  lcd.create_char(1,[0,4,10,17,0,0,0,0])
  lcd.create_char(2,[0,0,0,0,17,10,4,0])
  return lcd

def gpio_init():
  if not imports['GPIO']:
    print "WARN: gpio_init() disabled"
    return
  
  GPIO.setup(Button_TOP,GPIO.IN)
  GPIO.setup(Button_BOT,GPIO.IN)
  GPIO.setup(Button_LEFT,GPIO.IN)
  GPIO.setup(Button_RIGHT,GPIO.IN)
  GPIO.setup(Button_SET,GPIO.IN)

  GPIO.add_event_detect(Button_TOP,GPIO.RISING)
  GPIO.add_event_detect(Button_BOT,GPIO.RISING)
  GPIO.add_event_detect(Button_LEFT,GPIO.RISING)
  GPIO.add_event_detect(Button_RIGHT,GPIO.RISING)
  GPIO.add_event_detect(Button_SET,GPIO.RISING)

def screen_init(param, edfas):
  tab = []
  
  if len(edfas) == 1:
    if edfas[1].number_of_laser == 1:
      # MONITORING
      if param.has_input_PD == True and param.has_output_PD == True:
	    Monitoring_PAGE_1 = 'IN      .  dBm \x01\nOUT     .  dBm \x02'
	    Monitoring_PAGE_2 = 'LC1         mA \x01\nTEMP    .  C   \x02'
	    Monitoring_nb = 2
      elif param.has_input_PD == False and param.has_output_PD == True:
        Monitoring_PAGE_1 = 'OUT     .  dBm \x01\nLC1         mA \x02'
        Monitoring_PAGE_2 = 'TEMP    .  C   \x01\n               \x02'
        Monitoring_nb = 2
      elif param.has_input_PD == True and param.has_output_PD == False:
        Monitoring_PAGE_1 = 'IN      .  dBm \x01\nLC1         mA \x02'
        Monitoring_PAGE_2 = 'TEMP    .  C   \x01\n               \x02'
        Monitoring_nb = 2
      elif param.has_input_PD == False and param.has_output_PD == False:
        Monitoring_PAGE_1 = 'LC1         mA \x01\nTEMP    .  C   \x02'
        Monitoring_PAGE_2 = None
        Monitoring_nb = 1
      Monitoring_PAGE_3 = None
      Monitoring_PAGE_4 = None
      Monitoring_PAGE_5 = None
      # SETTINGS
      Settings_PAGE_1 = 'OPERATING MODE \x01\n         (set) \x02'
      Settings_PAGE_2 = ' CC1 SETPOINT  \x01\n      mA (set) \x02'
      if param.has_PC_mode == True and param.has_GC_mode == True:
        Settings_PAGE_3 = '  PC SETPOINT  \x01\n  .  dBm (set) \x02'
        Settings_PAGE_4 = '  GC SETPOINT  \x01\n  .  dBm (set) \x02'
        Settings_nb = 4		
      elif param.has_PC_mode == False and param.has_GC_mode == True:
        Settings_PAGE_3 = '  GC SETPOINT  \x01\n  .  dBm (set) \x02'
        Settings_PAGE_4 = None
        Settings_nb = 3
      elif param.has_PC_mode == True and param.has_GC_mode == False:
        Settings_PAGE_3 = '  PC SETPOINT  \x01\n  .  dBm (set) \x02'
        Settings_PAGE_4 = None
        Settings_nb = 3
      elif param.has_PC_mode == False and param.has_GC_mode == False:
        Settings_PAGE_3 = None
        Settings_PAGE_4 = None
        Settings_nb = 2
      Settings_PAGE_5 = None
    
    elif edfas[1].number_of_laser == 2:
      # MONITORING
      if param.has_input_PD == True and param.has_output_PD == True:
        Monitoring_PAGE_1 = 'IN      .  dBm \x01\nOUT     .  dBm \x02'
        Monitoring_PAGE_2 = 'LC1         mA \x01\nLC2         mA \x02'
        Monitoring_PAGE_3 = 'TEMP    .  C   \x01\n               \x02'
        Monitoring_nb = 3
      elif param.has_input_PD == False and param.has_output_PD == True:
        Monitoring_PAGE_1 = 'OUT     .  dBm \x01\nLC1         mA \x02'
        Monitoring_PAGE_2 = 'LC2         mA \x01\nTEMP    .  C   \x02'
        Monitoring_PAGE_3 = None
        Monitoring_nb = 2
      elif param.has_input_PD == True and param.has_output_PD == False:
        Monitoring_PAGE_1 = 'IN      .  dBm \x01\nLC1         mA \x02'
        Monitoring_PAGE_2 = 'LC2         mA \x01\nTEMP    .  C   \x02'
        Monitoring_PAGE_3 = None
        Monitoring_nb = 2
      elif param.has_input_PD == False and param.has_output_PD == False:
        Monitoring_PAGE_1 = 'LC1         mA \x01\nLC2         mA \x02'
        Monitoring_PAGE_2 = 'TEMP    .  C   \x01\n               \x02'
        Monitoring_PAGE_3 = None
        Monitoring_nb = 2
      Monitoring_PAGE_4 = None
      Monitoring_PAGE_5 = None
      # SETTINGS
      Settings_PAGE_1 = 'OPERATING MODE \x01\n         (set) \x02'
      Settings_PAGE_2 = ' CC1 SETPOINT  \x01\n      mA (set) \x02'
      Settings_PAGE_3 = ' CC2 SETPOINT  \x01\n      mA (set) \x02'
      if param.has_PC_mode == True and param.has_GC_mode == True:
        Settings_PAGE_4 = '  PC SETPOINT  \x01\n  .  dBm (set) \x02'
        Settings_PAGE_5 = '  GC SETPOINT  \x01\n  .  dBm (set) \x02'
        Settings_nb = 5
      elif param.has_PC_mode == False and param.has_GC_mode == True:
        Settings_PAGE_4 = '  GC SETPOINT  \x01\n  .  dBm (set) \x02'
        Settings_PAGE_5 = None
        Settings_nb = 4
      elif param.has_PC_mode == True and param.has_GC_mode == False:
        Settings_PAGE_4 = '  PC SETPOINT  \x01\n  .  dBm (set) \x02'
        Settings_PAGE_5 = None
        Settings_nb = 4
      elif param.has_PC_mode == False and param.has_GC_mode == False:
        Settings_PAGE_4 = None
        Settings_PAGE_5 = None
        Settings_nb = 3

    EDFA1_MONITORING = Screen(Monitoring_PAGE_1,
                              Monitoring_PAGE_2,
                              Monitoring_PAGE_3,
                              Monitoring_PAGE_4,
                              Monitoring_PAGE_5,
                              Monitoring_nb,
                              None,
                              None,
                              True,
                              False)
    EDFA1_SETTINGS = Screen(Settings_PAGE_1,
                            Settings_PAGE_2,
                            Settings_PAGE_3,
                            Settings_PAGE_4,
                            Settings_PAGE_5,
                            Settings_nb,
                            None,
                            None,
                            True,
                            False)	
    EDFA1_ALARMS = Screen('ALARMS:', None, None, None, None, 1, None, None, True, False)
    INFORMATIONS = Screen('SER\nPART', None, None, None, None, 1, None, None, True, False)
    tab = [ EDFA1_MONITORING, None, EDFA1_SETTINGS, None, EDFA1_ALARMS, None, INFORMATIONS ]

  elif param.number_of_edfa == 2:
    #EDFA2 = edfas[2]

    # need to read EDFA2 config here
    #tab = [EDFA1_MONITORING,EDFA2_MONITORING,EDFA1_SETTINGS,EDFA2_SETTINGS,EDFA1_ALARMS,EDFA2_ALARMS,INFORMATIONS]
    pass

  return tab
  

def loop(lcd, tab, params, infos, edfas):
  while True:
    if imports['GPIO'] and GPIO.event_detected(Button_LEFT):
      Old_screen = params.current_screen
      if params.number_of_edfa == 1:
        params.current_screen -= 2
      elif params.number_of_edfa == 2:
        params.current_screen -= 1
      if params.current_screen < 0:
        params.current_screen = 6
      if Old_screen != params.current_screen:
        params.screen_update = True

    elif imports['GPIO'] and GPIO.event_detected(Button_RIGHT):
      Old_screen = params.current_screen
      if params.number_of_edfa == 1:
        params.current_screen += 2
      elif params.number_of_edfa == 2:
        params.current_screen += 1
      if params.current_screen > 6:
        params.current_screen = 0
      if Old_screen != params.current_screen:		
        params.screen_update = True
      
    elif imports['GPIO'] and GPIO.event_detected(Button_TOP):
      Old_page = tab[params.current_screen].page_position
      if tab[params.current_screen].TB_enabled == True:
        tab[params.current_screen].page_position -= 1
        if tab[params.current_screen].page_position < 1:
          tab[params.current_screen].page_position = tab[params.current_screen].number_of_page
        if Old_page != tab[params.current_screen].page_position:
          params.screen_update = True
                
    elif imports['GPIO'] and GPIO.event_detected(Button_BOT):
      Old_page = tab[params.current_screen].page_position
      if tab[params.current_screen].TB_enabled == True:
        tab[params.current_screen].page_position += 1
        if tab[params.current_screen].page_position > tab[params.current_screen].number_of_page:
          tab[params.current_screen].page_position = 1
        if Old_page != tab[params.current_screen].page_position:
          params.screen_update = True

    if params.screen_update == True:
      draw_screen(lcd, tab[params.current_screen])
      params.screen_update = False

    if (time.time() - params.time_stamp) > 1:
      screen_update(lcd, tab, params, infos, edfas)
      params.time_stamp = time.time()

def main(args):
  baseurl = "http://localhost:80"
  
  if len(args) > 1:    
    baseurl = args[1]
  print "API: %s" % (baseurl)

  edfas = {}
  infos = Info(baseurl)
  params = Param(baseurl)
  
  # INIT
  lcd = lcd_init()
  gpio_init()

  edfas[1] = EDFA(1, baseurl)
  if params.number_of_edfa == 2:
    edfas[2] = EDFA(2, baseurl)
  tab = screen_init(params, edfas)

  lcd.clear()
  lcd.message(vendor_to_string(infos.vendor)) # "welcome" message // vendor name
  time.sleep(2.0)
  lcd.clear()

  loop(lcd, tab, params, infos, edfas)


if __name__ == '__main__':
  main(sys.argv)
