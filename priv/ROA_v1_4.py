#!/usr/bin/python
"""BKTEL PHOTONICS
Title   = 16x2 characters display control
Author  = Cyril Le Goas
Date    = 04/05/2016"""
version = '1.4'
 
import time
import Adafruit_CharLCD as LCD
import RPi.GPIO as GPIO
import configparser

OFF 	= 0
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
lcd_columns = 16
lcd_rows    = 2
lcd = LCD.Adafruit_CharLCD(lcd_rs, lcd_en, lcd_d4, lcd_d5, lcd_d6, lcd_d7,
                           lcd_columns, lcd_rows, lcd_backlight)
lcd.create_char(1,[0,4,10,17,0,0,0,0])
lcd.create_char(2,[0,0,0,0,17,10,4,0])
# NAVIGATION BUTTONS INIT
Button_TOP	= 2
Button_BOT	= 3
Button_LEFT	= 4
Button_RIGHT	= 10
Button_SET	= 9

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
class Param:
	""".time_stamp: last time value
	   .screen_update: define if an update is needed or not
	   .current_screen: screen postition 
	   .has_PC_mode: define if the unit have a PC mode 
	   .has_GC_mode: define if the unit have a GC mode
	   .has_input_PD: define if the unit have an input PD or not
	   .has_output_PD: define is the unit have an output PD or not
	   .number_of_edfa: define the number of edfa in the rack"""

	def __init__(self):
		self.time_stamp        = time.time()
		self.screen_update     = True
		self.current_screen    = 0
		self.has_PC_mode       = True
		self.has_GC_mode       = True
		self.has_input_PD      = True
		self.has_output_PD     = True
		self.number_of_edfa    = 1

class Info:
	""".serialnum : unit serial number
	   .partnum : unit part number
	   .date : production date
	   .vendor : vendor name, string used during boot
	   .hard : hardware version
	   .soft : software version"""

	def __init__(self,soft):
		self.serialnum	= 'TBD'
		self.partnum	= 'TBD'
		self.date	= 'TDB'
		self.vendor	= '1'
		self.hard	= 'TDB'
		self.soft	= soft

class Screen:
	""".page_position: current page position
	   .string_page1: default string for page 1
	   .string_page2: default string for page 2
	   .string_page3: default string for page 3 
	   .string_page4: default string for page 4
           .string_page5: default string for page 5 
	   .number_of_page: define the number of page for the screen X
	   .cursor_X: cursor position X
	   .cursor_Y: cursor position Y
	   .TB_enabled: define if the top and bot buttons are enabled
	   .SET_enabled: define if the set button is enabled  """

	def __init__(self,string_page1,string_page2,string_page3,string_page4,string_page5,number_of_page,cursor_X,cursor_Y,TB_enabled,SET_enabled):
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

class EDFA:
	""".mode: edfa operating mode
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
           .GC_setpoint: gc mode setpoint"""

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
				lcd.set_cursor(1,1)
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
				lcd.set_cursor(1,1)
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
# INIT

Product_info = Info(version)
Param = Param()

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
	EDFA1_ALARMS = Screen('ALAMRS:',None,None,None,None,1,None,None,True,False)
	INFORMATIONS = Screen('SER\nPART',None,None,None,None,1,None,None,True,False)
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
	if GPIO.event_detected(Button_LEFT):
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
		Old_page = tab[Param.current_screen].page_position
		if tab[Param.current_screen].TB_enabled == True:
			tab[Param.current_screen].page_position -= 1
			if tab[Param.current_screen].page_position < 1:
				tab[Param.current_screen].page_position = tab[Param.current_screen].number_of_page
			if Old_page != tab[Param.current_screen].page_position:
				Param.screen_update = True
	elif GPIO.event_detected(Button_BOT):
		Old_page = tab[Param.current_screen].page_position
		if tab[Param.current_screen].TB_enabled == True:
			tab[Param.current_screen].page_position += 1
			if tab[Param.current_screen].page_position > tab[Param.current_screen].number_of_page:
				tab[Param.current_screen].page_position = 1
			if Old_page != tab[Param.current_screen].page_position:
				Param.screen_update = True


	if Param.screen_update == True:
		draw_screen(tab[Param.current_screen])

	if (time.time() - Param.time_stamp) > 1:
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
