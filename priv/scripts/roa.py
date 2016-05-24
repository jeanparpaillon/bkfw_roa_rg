#!/usr/bin/python
"""BKTEL PHOTONICS
Title   = 16x2 characters display control
Author  = Cyril Le Goas
Date    = 04/05/2016"""
version = '1.5'
imports = {}

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

import configparser
import requests

OFF 	= 0
CC	= 1
PC	= 2
GC	= 3

SET_mode	= 0
SET_CC1		= 1
SET_CC2		= 2
SET_PC		= 3
SET_GC		= 4

tab_CC = [10000,1000,100,10,1]
tab_PC_GC = [100,10,1,0,0.1]

# LCD INIT
lcd_rs        = 25
lcd_en        = 24
lcd_d4        = 23
lcd_d5        = 17
lcd_d6        = 21
lcd_d7        = 22
lcd_backlight = None
lcd_columns = 16
lcd_rows    = 2
if imports['LCD']:
  lcd = LCD.Adafruit_CharLCD(lcd_rs, lcd_en, lcd_d4, lcd_d5, lcd_d6, lcd_d7,
                             lcd_columns, lcd_rows, lcd_backlight)
else:
  lcd = FakeLCD()

lcd.create_char(1,[0,4,10,17,0,0,0,0])
lcd.create_char(2,[0,0,0,0,17,10,4,0])
# NAVIGATION BUTTONS INIT
Button_TOP	= 2
Button_BOT	= 3
Button_LEFT	= 4
Button_RIGHT	= 10
Button_SET	= 9

if imports['GPIO']:
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

  def create_char(self, pos, char):
    return
              

class Param:
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

  def __init__(self):
    self.time_stamp        		= time.time()
    self.screen_update     		= True
    self.current_screen    		= 0
    self.has_PC_mode       		= True
    self.has_GC_mode       		= True
    self.has_input_PD      		= True
    self.has_output_PD     		= True
    self.number_of_edfa    		= 1

class Info:
  """
  .serialnum : unit serial number
  .partnum : unit part number
  .date : production date
  .vendor : vendor name, string used during boot
  .hard : hardware version
  .soft : software version
  """

  def __init__(self,soft):
	self.serialnum	= 'TBD'
	self.partnum	= 'TBD'
	self.date	= 'TDB'
	self.vendor	= '1'
	self.hard	= 'TDB'
	self.soft	= soft
    
class Set:
  """
  .cursor_position: define the position of the cursor and which digit will be modified
  .temp_value:
  .flag: flag is set when the set button is pressed and clear when it's pressed another time
  """

  def __init__(self):
    self.cursor_position 	= 4
    self.temp_value 	= 0
    self.flag 		= False
    self.settable_value	= 0

class Screen:
  """
  .page_position: current page position
  .string_page1: default string for page 1
  .string_page2: default string for page 2
  .string_page3: default string for page 3 
  .string_page4: default string for page 4
  .string_page5: default string for page 5 
  .number_of_page: define the number of page for the screen X
  .TB_enabled: define if the top and bot buttons are enabled
  .SET_enabled: define if the set button is enabled
  """

  def __init__(self,string_page1,string_page2,string_page3,string_page4,string_page5,number_of_page,TB_enabled,SET_enabled):
    self.page_position 	= 1
    self.string_page1	= string_page1
    self.string_page2	= string_page2
    self.string_page3	= string_page3
    self.string_page4	= string_page4
    self.string_page5	= string_page5
    self.number_of_page	= number_of_page
    self.TB_enabled    	= TB_enabled
    self.SET_enabled   	= SET_enabled

class EDFA:
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

  def __init__(self):
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

# FUNCTIONS
def draw_screen(type):
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
	      Param.screen_update = False

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

def read_edfa_param():
  config.read('Config.ini')
  EDFA1.mode 		 = float(config.get('EDFA1 PARAMETERS','EDFA1_mode'))
  EDFA1.max_current_LD1    = float(config.get('EDFA1 PARAMETERS','EDFA1_max_current_LD1'))
  EDFA1.max_current_LD2    = float(config.get('EDFA1 PARAMETERS','EDFA1_max_current_LD2'))
  EDFA1.min_pc             = float(config.get('EDFA1 PARAMETERS','EDFA1_min_pc'))
  EDFA1.max_pc             = float(config.get('EDFA1 PARAMETERS','EDFA1_max_pc'))
  EDFA1.min_gc             = float(config.get('EDFA1 PARAMETERS','EDFA1_min_gc'))
  EDFA1.max_gc             = float(config.get('EDFA1 PARAMETERS','EDFA1_max_gc'))
  EDFA1.number_of_laser    = float(config.get('EDFA1 PARAMETERS','EDFA1_number_of_laser'))
  EDFA1.has_settable_LD1   = string_to_bool(config.get('EDFA1 PARAMETERS','EDFA1_has_settable_LD1'))
  EDFA1.alarm              = float(config.get('EDFA1 PARAMETERS','EDFA1_alarm'))
  EDFA1.LD1_current        = float(config.get('EDFA1 PARAMETERS','EDFA1_LD1_current'))
  EDFA1.LD2_current        = float(config.get('EDFA1 PARAMETERS','EDFA1_LD2_current'))
  EDFA1.input_power        = float(config.get('EDFA1 PARAMETERS','EDFA1_input_power'))
  EDFA1.output_power       = float(config.get('EDFA1 PARAMETERS','EDFA1_output_power'))
  EDFA1.internal_temp      = float(config.get('EDFA1 PARAMETERS','EDFA1_internal_temp'))
  EDFA1.CC1_setpoint       = float(config.get('EDFA1 PARAMETERS','EDFA1_CC1_setpoint'))
  EDFA1.CC2_setpoint       = float(config.get('EDFA1 PARAMETERS','EDFA1_CC2_setpoint'))
  EDFA1.PC_setpoint        = float(config.get('EDFA1 PARAMETERS','EDFA1_PC_setpoint'))
  EDFA1.GC_setpoint        = float(config.get('EDFA1 PARAMETERS','EDFA1_GC_setpoint'))

def fill_with_blank(value):
	if value >= 10:
	  lcd.message(' ')
	elif value >= 0 and value < 10:
	  lcd.message('  ')
	elif value > -10 and value < 0:
	  lcd.message(' ')
def set_value():
  string = '%.0f' % Set.temp_value
	if Set.settable_value == SET_mode:
	  config.set('EDFA1 PARAMETERS','EDFA1_mode',string)
	elif Set.settable_value == SET_CC1:
	  config.set('EDFA1 PARAMETERS','EDFA1_CC1_setpoint',string)
	elif Set.settable_value == SET_CC2:
	  config.set('EDFA1 PARAMETERS','EDFA1_CC2_setpoint',string)
	  string = '%.1f' % Set.temp_value
	if Set.settable_value == SET_PC:
	  config.set('EDFA1 PARAMETERS','EDFA1_PC_setpoint',string)
	elif Set.settable_value == SET_GC:
	  config.set('EDFA1 PARAMETERS','EDFA1_GC_setpoint',string)	
	  config.write(open('Config.ini','w'))
def screen_update():	#need an update for more edfa
	if Param.current_screen == 0: #EDFA1 MONITORING
		if EDFA1.number_of_laser == 1:	
			if EDFA1_MONITORING.page_position == 1: 
				if   Param.has_input_PD == True  and Param.has_output_PD == True:
				  lcd.set_cursor(5,0)
				  fill_with_blank(EDFA1.input_power)
				  lcd.message('%2.1f' % (EDFA1.input_power))
				  lcd.set_cursor(5,1)
                  fill_with_blank(EDFA1.output_power)
				  lcd.message('%2.1f' % (EDFA1.output_power))
				elif Param.has_input_PD == False and Param.has_output_PD == True:
                  lcd.set_cursor(5,0)
                  fill_with_blank(EDFA1.output_power)
                  lcd.message('%2.1f' % (EDFA1.output_power))
                  lcd.set_cursor(6,1)
	              lcd.message('%5.0f' % (EDFA1.LD1_current))
				elif Param.has_input_PD == True  and Param.has_output_PD == False:
                  lcd.set_cursor(5,0)
                  fill_with_blank(EDFA1.input_power)
                  lcd.message('%2.1f' % (EDFA1.input_power))
                  lcd.set_cursor(6,1)
                  lcd.message('%5.0f' % (EDFA1.LD1_current))
				elif Param.has_input_PD == False and Param.has_output_PD == False:
                  lcd.set_cursor(6,0)
                  lcd.message('%5.0f' % (EDFA1.LD1_current))
                  lcd.set_cursor(5,1)
                  fill_with_blank(EDFA1.internal_temp)
                  lcd.message('%2.1f' % (EDFA1.internal_temp))
			elif EDFA1_MONITORING.page_position == 2:
				if   Param.has_input_PD == True  and Param.has_output_PD == True:
                  lcd.set_cursor(6,0)
                  lcd.message('%5.0f' % (EDFA1.LD1_current))
                  lcd.set_cursor(5,1)
				  fill_with_blank(EDFA1.internal_temp)
                  lcd.message('%2.1f' % (EDFA1.internal_temp))
                                else:
                                  lcd.set_cursor(5,0)
					              fill_with_blank(EDFA1.internal_temp)
                                  lcd.message('%2.1f' % (EDFA1.internal_temp)) 
		elif EDFA1.number_of_laser == 2:	
			if EDFA1_MONITORING.page_position == 1:
				if   Param.has_input_PD == True  and Param.has_output_PD == True:
                  lcd.set_cursor(5,0)
				  fill_with_blank(EDFA1.input_power)
                  lcd.message('%2.1f' % (EDFA1.input_power))
                  lcd.set_cursor(5,1)
				  fill_with_blank(EDFA1.output_power)
                  lcd.message('%2.1f' % (EDFA1.output_power))
                                elif Param.has_input_PD == False and Param.has_output_PD == True:
                                  lcd.set_cursor(5,0)
					              fill_with_blank(EDFA1.output_power)
                                  lcd.message('%2.1f' % (EDFA1.output_power))
                                  lcd.set_cursor(6,1)
                                  lcd.message('%5.0f' % (EDFA1.LD1_current))
                                elif Param.has_input_PD == True  and Param.has_output_PD == False:
                                  lcd.set_cursor(5,0)
					              fill_with_blank(EDFA1.input_power)
                                  lcd.message('%2.1f' % (EDFA1.input_power))
                                  lcd.set_cursor(6,1)
                                  lcd.message('%5.0f' % (EDFA1.LD1_current))
                                elif Param.has_input_PD == False and Param.has_output_PD == False:
                                  lcd.set_cursor(6,0)
                                  lcd.message('%5.0f' % (EDFA1.LD1_current))
                                  lcd.set_cursor(6,1)
                                  lcd.message('%5.0f' % (EDFA1.LD2_current))
			elif EDFA1_MONITORING.page_position == 2:
                                if   Param.has_input_PD == True  and Param.has_output_PD == True:
                                  lcd.set_cursor(6,0)
                                  lcd.message('%5.0f' % (EDFA1.LD1_current))
                                  lcd.set_cursor(6,1)
                                  lcd.message('%5.0f' % (EDFA1.LD2_current))
                                elif Param.has_input_PD == False and Param.has_output_PD == True:
                                  lcd.set_cursor(6,0)
                                  lcd.message('%5.0f' % (EDFA1.LC2_current))
                                  lcd.set_cursor(5,1)
					              fill_with_blank(EDFA1.internal_temp)
                                  lcd.message('%2.1f' % (EDFA1.internal_temp))
                                elif Param.has_input_PD == True  and Param.has_output_PD == False:
                                  lcd.set_cursor(6,0)
                                  lcd.message('%5.0f' % (EDFA1.LC2_current))
                                  lcd.set_cursor(5,1)
					              fill_with_blank(EDFA1.internal_temp)
                                  lcd.message('%2.1f' % (EDFA1.internal_temp))
                                elif Param.has_input_PD == False and Param.has_output_PD == False:
                                  lcd.set_cursor(5,0)
					              fill_with_blank(EDFA1.internal_temp)
                                  lcd.message('%2.1f' % (EDFA1.internal_temp))
			elif EDFA1_MONITORING.page_position == 3:
			  lcd.set_cursor(5,0)
			  fill_with_blank(EDFA1.internal_temp)
              lcd.message('%2.1f' % (EDFA1.internal_temp))
	elif Param.current_screen == 2:
		if EDFA1.number_of_laser == 1:
			if EDFA1_SETTINGS.page_position == 1:
			  lcd.set_cursor(2,1)
				if EDFA1.mode == OFF:
				  lcd.message('OFF')
				elif EDFA1.mode == CC:
				  lcd.message(' CC')
				elif EDFA1.mode == PC:
				  lcd.message(' PC')
				elif EDFA1.mode == GC:
				  lcd.message(' GC')			
			elif EDFA1_SETTINGS.page_position == 2:
			  lcd.set_cursor(0,1)
			  lcd.message('%5.0f' % (EDFA1.CC1_setpoint))
			elif EDFA1_SETTINGS.page_position == 3:
			  lcd.set_cursor(0,1)
				if Param.has_PC_mode == True :
				  fill_with_blank(EDFA1.PC_setpoint)
				  lcd.message('%2.1f' % (EDFA1.PC_setpoint))
				elif Param.has_GC_mode == True:
				  fill_with_blank(EDFA1.GC_setpoint)
				  lcd.message('%2.1f' % (EDFA1.GC_setpoint))
			elif EDFA1_SETTINGS.page_position == 4:
			  lcd.set_cursor(0,1)
			  fill_with_blank(EDFA1.GC_setpoint)
			  lcd.message('%2.1f' % (EDFA1.GC_setpoint))			
		elif EDFA1.number_of_laser == 2:
			if EDFA1_SETTINGS.page_position == 1:
			  lcd.set_cursor(2,1)
                                if EDFA1.mode == OFF:
				                  lcd.message('OFF')
                                elif EDFA1.mode == CC:
                                  lcd.message(' CC')
                                elif EDFA1.mode == PC:
                                  lcd.message(' PC')
                                elif EDFA1.mode == GC:
                                  lcd.message(' GC')
			elif EDFA1_SETTINGS.page_position == 2:
			  lcd.set_cursor(0,1)
			  lcd.message('%5.0f' % (EDFA1.CC1_setpoint))
			elif EDFA1_SETTINGS.page_position == 3:
			  lcd.set_cursor(0,1)
			  lcd.message('%5.0f' % (EDFA1.CC2_setpoint))
			elif EDFA1_SETTINGS.page_position == 4:
			  lcd.set_cursor(0,1)
				if Param.has_PC_mode == True :
				  fill_with_blank(EDFA1.PC_setpoint)
				  lcd.message('%2.1f' % (EDFA1.PC_setpoint))
				elif Param.has_GC_mode == True:
				  fill_with_blank(EDFA1.GC_setpoint)
				  lcd.message('%2.1f' % (EDFA1.GC_setpoint))
			elif EDFA1_SETTINGS.page_position == 5:
			  lcd.set_cursor(0,1)
			  fill_with_blank(EDFA1.GC_setpoint)
			  lcd.message('%2.1f' % (EDFA1.GC_setpoint))
			  #GC only
	elif Param.current_screen == 4: #EDFA1 ALARMS
	  lcd.set_cursor(0,0)
	elif Param.current_screen == 6: #RACK INFO
	  lcd.set_cursor(4,0)
	  lcd.message('%s' % (Product_info.serialnum))
	  lcd.set_cursor(5,1)
	  lcd.message('%s' % (Product_info.partnum))

def which_parameter(page):
	if page == 1:
	  Set.temp_value = EDFA1.mode
		return SET_mode
	elif page == 2:
	  Set.temp_value = EDFA1.CC1_setpoint
		return SET_CC1
	elif page == 3 and EDFA1.number_of_laser == 2:
	  Set.temp_value = EDFA1.CC2_setpoint
		return SET_CC2
	elif page == 3 and Param.has_PC_mode == True:
	  Set.temp_value = EDFA1.PC_setpoint
		return SET_PC
	elif page == 3 and Param.has_GC_mode == True:
	  Set.temp_value = EDFA1.GC_setpoint
		return SET_GC
	elif page == 4 and Param.has_PC_mode == True and EDFA1.number_of_laser == 2:
	  Set.temp_value = EDFA1.PC_setpoint
		return SET_PC
	elif page == 4 and Param.has_GC_mode == True:
	  Set.temp_value = EDFA1.GC_setpoint
		return SET_GC
	elif page == 5:
		return SET_GC		
	  
def check_buttons():
	if GPIO.event_detected(Button_LEFT):
		if Set.flag:
			if Set.settable_value != SET_mode:
				if Set.cursor_position == 4 and (Set.settable_value == SET_PC or Set.settable_value == SET_GC):
				  Set.cursor_position -= 2
				else:
				  Set.cursor_position -= 1
				if Set.settable_value == SET_PC or Set.settable_value == SET_GC:
					if Set.cursor_position <= 1:
					  Set.cursor_position = 1
				else:
					if Set.cursor_position <= 0:
					  Set.cursor_position = 0
				      lcd.set_cursor(Set.cursor_position,1)
		else:
		  Old_screen = Param.current_screen
			if Param.number_of_edfa == 1:
			  Param.current_screen -= 2
			elif Param.number_of_edfa == 2:
			  Param.current_screen -= 1
			if Param.current_screen < 0:
			  Param.current_screen = 6
			if Old_screen != Param.current_screen:
			  Param.screen_update = True
	elif GPIO.event_detected(Button_RIGHT):
		if Set.flag:
			if Set.settable_value != SET_mode:
				if Set.cursor_position == 2 and (Set.settable_value == SET_PC or Set.settable_value == SET_GC):
				  Set.cursor_position += 2
				else:
				  Set.cursor_position += 1
			if Set.cursor_position >= 4:
			  Set.cursor_position = 4
			  lcd.set_cursor(Set.cursor_position,1)
		else:
		  Old_screen = Param.current_screen
			if Param.number_of_edfa == 1:
			  Param.current_screen += 2
			elif Param.number_of_edfa == 2:
			  Param.current_screen += 1
			if Param.current_screen > 6:
			  Param.current_screen = 0
			if Old_screen != Param.current_screen:		
			  Param.screen_update = True
	elif GPIO.event_detected(Button_TOP):
		if Set.flag:
			if Set.settable_value == SET_mode:
			  Set.temp_value += 1
				if Set.temp_value == PC and Param.has_PC_mode == False:
				  Set.temp_value += 1
				if Set.temp_value == GC and Param.has_GC_mode == False:
				  Set.temp_value += 1
				if Set.temp_value >= 4:
				  Set.temp_value = OFF
				  lcd.blink(False)
				  lcd.set_cursor(2,1)				
				if Set.temp_value == OFF:
				  lcd.message('OFF')
				elif Set.temp_value == CC:
				  lcd.message(' CC')
				elif Set.temp_value == PC:
				  lcd.message(' PC')
				elif Set.temp_value == GC:
				  lcd.message(' GC')
				  lcd.set_cursor(Set.cursor_position,1)
				  lcd.blink(True)
			elif Set.settable_value == SET_CC1 or Set.settable_value == SET_CC2:
			  Set.temp_value += tab_CC[Set.cursor_position]
				if Set.temp_value <= 0:
				  Set.temp_value = 0
				elif Set.temp_value >= EDFA1.max_current_LD1 and Set.settable_value == SET_CC1:
				  Set.temp_value = EDFA1.max_current_LD1
				elif Set.temp_value >= EDFA1.max_current_LD2 and Set.settable_value == SET_CC2:
				  Set.temp_value = EDFA1.max_current_LD2
				  lcd.blink(False)
				  lcd.set_cursor(0,1)
				  lcd.message('%5.0f' % (Set.temp_value))
				  lcd.set_cursor(Set.cursor_position,1)
				  lcd.blink(True)
			elif Set.settable_value == SET_GC or Set.settable_value == SET_PC:
			  Set.temp_value += tab_PC_GC[Set.cursor_position]
				if Set.temp_value <= EDFA1.min_pc and Set.settable_value == SET_PC:
				  Set.temp_value = EDFA1.min_pc
				elif Set.temp_value <= EDFA1.min_gc and Set.settable_value == SET_GC:
				  Set.temp_value = EDFA1.min_gc
				elif Set.temp_value >= EDFA1.max_pc and Set.settable_value == SET_PC:
				  Set.temp_value = EDFA1.max_pc
				elif Set.temp_value >= EDFA1.max_gc and Set.settable_value == SET_GC:
				  Set.temp_value = EDFA1.max_gc
				  lcd.blink(False)
				  lcd.set_cursor(0,1)
				  fill_with_blank(Set.temp_value)
				  lcd.message('%2.1f' % (Set.temp_value))
				  lcd.set_cursor(Set.cursor_position,1)
				  lcd.blink(True)				
		else:
		  Old_page = tab[Param.current_screen].page_position
			if tab[Param.current_screen].TB_enabled == True:
			  tab[Param.current_screen].page_position -= 1
				if tab[Param.current_screen].page_position < 1:
				  tab[Param.current_screen].page_position = tab[Param.current_screen].number_of_page
				if Old_page != tab[Param.current_screen].page_position:
				  Param.screen_update = True
	elif GPIO.event_detected(Button_BOT):
		if Set.flag:
			if Set.settable_value == SET_mode:
			  Set.temp_value -= 1
				if Set.temp_value == PC and Param.has_PC_mode == False:
				  Set.temp_value -= 1
				if Set.temp_value == GC and Param.has_GC_mode == False:
				  Set.temp_value -= 1
				if Set.temp_value <= 0 and Param.has_GC_mode == True:
				  Set.temp_value = GC
				elif Set.temp_value <= 0 and Param.has_GC_mode == True:
				  Set.temp_value = PC
				elif Set.temp_value <= 0:
				  Set.temp_value = CC
				  lcd.blink(False)
				  lcd.set_cursor(2,1)				
				if Set.temp_value == OFF:
				  lcd.message('OFF')
				elif Set.temp_value == CC:
				  lcd.message(' CC')
				elif Set.temp_value == PC:
				  lcd.message(' PC')
				elif Set.temp_value == GC:
				  lcd.message(' GC')
				  lcd.set_cursor(Set.cursor_position,1)
				  lcd.blink(True)
			elif Set.settable_value == SET_CC1 or Set.settable_value == SET_CC2:
			  Set.temp_value -= tab_CC[Set.cursor_position]
				if Set.temp_value <= 0:
				  Set.temp_value = 0
				elif Set.temp_value >= EDFA1.max_current_LD1 and Set.settable_value == SET_CC1:
				  Set.temp_value = EDFA1.max_current_LD1
				elif Set.temp_value >= EDFA1.max_current_LD2 and Set.settable_value == SET_CC2:
				  Set.temp_value = EDFA1.max_current_LD2
				  lcd.blink(False)
				  lcd.set_cursor(0,1)
				  lcd.message('%5.0f' % (Set.temp_value))
				  lcd.set_cursor(Set.cursor_position,1)
				  lcd.blink(True)
			elif Set.settable_value == SET_GC or Set.settable_value == SET_PC:
			  Set.temp_value -= tab_PC_GC[Set.cursor_position]
				if Set.temp_value <= EDFA1.min_pc and Set.settable_value == SET_PC:
				  Set.temp_value = EDFA1.min_pc
				elif Set.temp_value <= EDFA1.min_gc and Set.settable_value == SET_GC:
				  Set.temp_value = EDFA1.min_gc
				elif Set.temp_value >= EDFA1.max_pc and Set.settable_value == SET_PC:
				  Set.temp_value = EDFA1.max_pc
				elif Set.temp_value >= EDFA1.max_gc and Set.settable_value == SET_GC:
				  Set.temp_value = EDFA1.max_gc
				  lcd.blink(False)
				  lcd.set_cursor(0,1)
				  fill_with_blank(Set.temp_value)
				  lcd.message('%2.1f' % (Set.temp_value))
				  lcd.set_cursor(Set.cursor_position,1)
				  lcd.blink(True)				
		else:
		  Old_page = tab[Param.current_screen].page_position
			if tab[Param.current_screen].TB_enabled == True:
			  tab[Param.current_screen].page_position += 1
				if tab[Param.current_screen].page_position > tab[Param.current_screen].number_of_page:
				  tab[Param.current_screen].page_position = 1
				if Old_page != tab[Param.current_screen].page_position:
				  Param.screen_update = True
	elif GPIO.event_detected(Button_SET) and tab[Param.current_screen].SET_enabled:
		if Set.flag == False:
			if not(tab[Param.current_screen].page_position == 2 and EDFA1.has_settable_LD1):
			  lcd.set_cursor(Set.cursor_position,1)
			  #lcd.show_cursor(True)
			  lcd.blink(True)
			  Set.flag = True
			  Set.settable_value = which_parameter(tab[Param.current_screen].page_position)
		else:
		  Set.cursor_position = 4
		  Set.flag = False
		  set_value()
		  Set.temp_value = 0
		  lcd.blink(False)
		  #lcd.show_cursor(False)



		
		
# INIT

Product_info = Info(version)
Param = Param()
Set = Set()
config = configparser.ConfigParser()
config.read('Config.ini')

Param.has_PC_mode       = string_to_bool(config.get('Param','has_PC_mode'))
Param.has_GC_mode       = string_to_bool(config.get('Param','has_GC_mode'))
Param.has_input_PD      = string_to_bool(config.get('Param','has_input_PD'))
Param.has_output_PD     = string_to_bool(config.get('Param','has_output_PD'))
Param.number_of_edfa	= float(config.get('Param','number_of_edfa'))

Product_info.serialnum  = config.get('Product_info','serial')
Product_info.partnum    = config.get('Product_info','partnum')
Product_info.date       = config.get('Product_info','date')
Product_info.vendor	= float(config.get('Product_info','vendor'))
Product_info.hard	= config.get('Product_info','hard')

EDFA1 = EDFA()
read_edfa_param()
if Param.number_of_edfa == 1:
        if EDFA1.number_of_laser == 1:
		  # MONITORING
		        if Param.has_input_PD == True and Param.has_output_PD == True:
			      Monitoring_PAGE_1 = 'IN      .  dBm \x01\nOUT     .  dBm \x02'
			      Monitoring_PAGE_2 = 'LC1         mA \x01\nTEMP    .  C   \x02'
                  Monitoring_nb = 2
                elif Param.has_input_PD == False and Param.has_output_PD == True:
			      Monitoring_PAGE_1 = 'OUT     .  dBm \x01\nLC1         mA \x02'
			      Monitoring_PAGE_2 = 'TEMP    .  C   \x01\n               \x02'
			      Monitoring_nb = 2
		        elif Param.has_input_PD == True and Param.has_output_PD == False:
			      Monitoring_PAGE_1 = 'IN      .  dBm \x01\nLC1         mA \x02'
			      Monitoring_PAGE_2 = 'TEMP    .  C   \x01\n               \x02'
			      Monitoring_nb = 2
		        elif Param.has_input_PD == False and Param.has_output_PD == False:
			      Monitoring_PAGE_1 = 'LC1         mA \x01\nTEMP    .  C   \x02'
			      Monitoring_PAGE_2 = None
			      Monitoring_nb = 1
		          Monitoring_PAGE_3 = None
		          Monitoring_PAGE_4 = None
		          Monitoring_PAGE_5 = None
		          # SETTINGS
		          Settings_PAGE_1 = 'OPERATING MODE \x01\n         (set) \x02'
		          Settings_PAGE_2 = ' CC1 SETPOINT  \x01\n      mA (set) \x02'
		if Param.has_PC_mode == True and Param.has_GC_mode == True:
		  Settings_PAGE_3 = '  PC SETPOINT  \x01\n  .  dBm (set) \x02'
		  Settings_PAGE_4 = '  GC SETPOINT  \x01\n  .  dBm (set) \x02'
		  Settings_nb = 4		
		elif Param.has_PC_mode == False and Param.has_GC_mode == True:
		  Settings_PAGE_3 = '  GC SETPOINT  \x01\n  .  dBm (set) \x02'
		  Settings_PAGE_4 = None
		  Settings_nb = 3
		elif Param.has_PC_mode == True and Param.has_GC_mode == False:
		  Settings_PAGE_3 = '  PC SETPOINT  \x01\n  .  dBm (set) \x02'
		  Settings_PAGE_4 = None
		  Settings_nb = 3
		elif Param.has_PC_mode == False and Param.has_GC_mode == False:
          Settings_PAGE_3 = None
          Settings_PAGE_4 = None
          Settings_nb = 2
		  Settings_PAGE_5 = None
	elif EDFA1.number_of_laser == 2:
	  # MONITORING
                if Param.has_input_PD == True and Param.has_output_PD == True:
                  Monitoring_PAGE_1 = 'IN      .  dBm \x01\nOUT     .  dBm \x02'
                  Monitoring_PAGE_2 = 'LC1         mA \x01\nLC2         mA \x02'
			      Monitoring_PAGE_3 = 'TEMP    .  C   \x01\n               \x02'
                  Monitoring_nb = 3
                elif Param.has_input_PD == False and Param.has_output_PD == True:
                  Monitoring_PAGE_1 = 'OUT     .  dBm \x01\nLC1         mA \x02'
                  Monitoring_PAGE_2 = 'LC2         mA \x01\nTEMP    .  C   \x02'
                  Monitoring_PAGE_3 = None
			      Monitoring_nb = 2
                elif Param.has_input_PD == True and Param.has_output_PD == False:
                  Monitoring_PAGE_1 = 'IN      .  dBm \x01\nLC1         mA \x02'
                  Monitoring_PAGE_2 = 'LC2         mA \x01\nTEMP    .  C   \x02'
			      Monitoring_PAGE_3 = None
                  Monitoring_nb = 2
                elif Param.has_input_PD == False and Param.has_output_PD == False:
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
                if Param.has_PC_mode == True and Param.has_GC_mode == True:
                  Settings_PAGE_4 = '  PC SETPOINT  \x01\n  .  dBm (set) \x02'
                  Settings_PAGE_5 = '  GC SETPOINT  \x01\n  .  dBm (set) \x02'
                  Settings_nb = 5
                elif Param.has_PC_mode == False and Param.has_GC_mode == True:
                  Settings_PAGE_4 = '  GC SETPOINT  \x01\n  .  dBm (set) \x02'
                  Settings_PAGE_5 = None
                  Settings_nb = 4
                elif Param.has_PC_mode == True and Param.has_GC_mode == False:
                  Settings_PAGE_4 = '  PC SETPOINT  \x01\n  .  dBm (set) \x02'
                  Settings_PAGE_5 = None
                  Settings_nb = 4
                elif Param.has_PC_mode == False and Param.has_GC_mode == False:
                  Settings_PAGE_4 = None
                  Settings_PAGE_5 = None
                  Settings_nb = 3
	              EDFA1_MONITORING = Screen(Monitoring_PAGE_1,
                                            Monitoring_PAGE_2,
                                            Monitoring_PAGE_3,
                                            Monitoring_PAGE_4,
                                            Monitoring_PAGE_5,
                                            Monitoring_nb,
                                            True,
                                            False)
	              EDFA1_SETTINGS = Screen(Settings_PAGE_1,
                                          Settings_PAGE_2,
                                          Settings_PAGE_3,
                                          Settings_PAGE_4,
                                          Settings_PAGE_5,
                                          Settings_nb,
                                          True,
                                          True)	
	              EDFA1_ALARMS = Screen('ALAMRS:',None,None,None,None,1,True,False)
	              INFORMATIONS = Screen('SER\nPART',None,None,None,None,1,True,False)
	              tab = [EDFA1_MONITORING,None,EDFA1_SETTINGS,None,EDFA1_ALARMS,None,INFORMATIONS]

elif Param.number_of_edfa == 2:
  EDFA2 = EDFA()
  # need to read EDFA2 config here
	tab = [EDFA1_MONITORING,EDFA2_MONITORING,EDFA1_SETTINGS,EDFA2_SETTINGS,EDFA1_ALARMS,EDFA2_ALARMS,INFORMATIONS]

# MAIN
lcd.clear()
lcd.message(vendor_to_string(Product_info.vendor)) # "welcome" message // vendor name
time.sleep(2.0)
lcd.clear()








			
while True:

	check_buttons()
	if Param.screen_update == True:
	  draw_screen(tab[Param.current_screen])

	if (time.time() - Param.time_stamp) > 1:
		if Set.flag == False: 
		  read_edfa_param()
		  screen_update()
		  #print "current screen: %d" %Param.current_screen
		  #print "current page: %d" %tab[Param.current_screen].page_position
		  #print "--------------------------"
		  Param.time_stamp = time.time()




#lcd.clear()
#lcd.set_cursor(2,0)
#lcd.show_cursor(True)
#lcd.blink(True)
