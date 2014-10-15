#! /usr/bin/python

import serial
import sys
import time
import re
import os

def help(pname):
	print "ERROR: use ",pname," <USB device> <wrc.ram>"
	print "ERROR: use ",pname," <USB device> <old_wrc.ram> <new_wrc.ram>"
	print "ERROR: use ",pname," etherbone <ip> <port> <wrc.ram>"
	print "ERROR: use ",pname," etherbone <ip> <port> <old_wrc.ram> <new_wrc.ram>"
	print "Made by: Miguel Jimenez Lopez <klyone@ugr.es>, UGR"
	print

def input_verbose():
	verbose_ok = False

	while not verbose_ok:
		verbose_c = raw_input("Do you want to use a verbose mode (y/n)?: ")
	
		if verbose_c != None:
			verbose_c = verbose_c[0]
	
		if verbose_c == 'y':
			verbose_l = raw_input("What verbose level do you want (1,2,3 or 4)?: ")
			verbose_l = int(verbose_l)
		else:
			verbose_l = 1;
	
		if (verbose_c == 'y' or verbose_c == 'n') and (verbose_l > 0 and verbose_l < 5):
			verbose_ok = True
			
	return [verbose_c,verbose_l]

def time_convert(t):
	hours = t/3600
	minutes = (t-(hours*3600))/60
	secs = (t-minutes*60)
	
	res = [hours,minutes,secs]
	
	return res
	
def execute_file(f,port,tintchar=0.01,eol=0xd):
	for cmd in f:
		cmd = cmd[:-1]
		print "Executing "+cmd+"..."
		for c in cmd:
			port.write(c)
			time.sleep(tintchar)
			
		port.write(chr(eol))
		print "Press any key to continue..."
		wait = raw_input()

def execute_program(dev,interactive=False,tintchar=0.01,eol=0xd,fcmd="cmd.txt",cmd_exit="exit",cmd_load="load",pbaudrate=115200,pparity=serial.PARITY_NONE,pstopbits=serial.STOPBITS_ONE,pbytesize=serial.EIGHTBITS,pxonxoff=False,prtscts=False,pdsrdtr=False):
	if interactive == False:
		f = open(fcmd)
		port = serial.Serial(dev, baudrate=pbaudrate,parity=pparity,stopbits=pstopbits,bytesize=pbytesize,xonxoff=pxonxoff,rtscts=prtscts,dsrdtr=pdsrdtr)
		
		execute_file(f,port,tintchar=tintchar,eol=eol)
		
		port.close()
		f.close()
	else:	
		port = serial.Serial(dev, baudrate=pbaudrate,parity=pparity,stopbits=pstopbits,bytesize=pbytesize,xonxoff=pxonxoff,rtscts=prtscts,dsrdtr=pdsrdtr)
		end = False
		
		while not end:
			cmd = raw_input("wrc# ")
			
			if cmd == cmd_exit:
				break
			elif cmd == cmd_load:
				fload = raw_input("File: ")
				f2 = open(fload)
				execute_file(f2,port,tintchar=tintchar,eol=eol)
				f2.close()
			else:
				for c in cmd:
					port.write(c)
					time.sleep(tintchar)
					
				port.write(chr(eol))
				print "Press any key to continue..."
				wait = raw_input()
				
		port.close()

def load_programs(old,new,verbose=False,fverbose="dumpr.txt",itoken="write",otoken="write"):
	
	if new != None:
		fold = open(old)
		fnew = open(new)
	
		mold = {}
		mnew = {}
	
		for n in fold:
			n = n.lower()
			n_split = re.split(itoken+"\s",n)
			n_split = re.split("\s",n_split[1])
			mold[n_split[0]]=n_split[1]
	
		
		for n in fnew:
			n = n.lower()
			n_split = re.split(itoken+"\s",n)
			n_split = re.split("\s",n_split[1])
			mnew[n_split[0]]=n_split[1]
	
	
		fold.close()
		fnew.close()
		
		mf = {}
		
		for k in mold:
			if mnew.has_key(k) and mold[k] != mnew[k]:
				mf[k] = mnew[k]
			
		for k in mnew:
			if not mold.has_key(k):
				mf[k] = mnew[k]
	
		cmd = []
		
		lmf = mf.items()
		lmf.sort()
	
		for k in lmf:
			#cmd.append(otoken+" "+k[0]+" "+k[1]+"\n")
			cmd.append(otoken+" "+k[0]+" "+k[1])
		
	else:
		mold = {}
		
		fold = open(old)
		
		for n in fold:
			n = n.lower()
			n_split = re.split(itoken+"\s",n)
			n_split = re.split("\s",n_split[1])
			mold[n_split[0]]=n_split[1]
			
		fold.close()
			
		cmd = []
		
		lmf = mold.items()
		lmf.sort()
		
		for k in lmf:
			cmd.append(otoken+" "+k[0]+" "+k[1])
		
	if verbose:
		fdump = open(fverbose,"w")
		for l in cmd:
			fdump.write(l)
		fdump.close()

	return cmd

def etherbone_program(program_cmd,ip,port,proto="udp",token="write",etherbone_path="./tools",verbose=False,lverbose=1):
	
	m = {}
	
	for l in program_cmd:
		n_split = re.split(token+"\s",l)
		n_split = re.split("\s",n_split[1])
		m[n_split[0]]=n_split[1]
	
	for l in m:	
		os.system(etherbone_path+"/"+"eb-write "+"udp/"+ip+"/"+port+" "+l+"/4 "+m[l]+"/4")
		
		if verbose:
			print etherbone_path+"/"+"eb-write "+proto+"/"+ip+"/"+port+" "+l+"/4 "+m[l]
			

def wr_program(dev,program_cmd,tintchar=0.01,eol=0xd,pbaudrate=115200,pparity=serial.PARITY_NONE,pstopbits=serial.STOPBITS_ONE,pbytesize=serial.EIGHTBITS,pxonxoff=False,prtscts=False,pdsrdtr=False,verbose=False,lverbose=1):
	port = serial.Serial(dev, baudrate=pbaudrate,parity=pparity,stopbits=pstopbits,bytesize=pbytesize,xonxoff=pxonxoff,rtscts=prtscts,dsrdtr=pdsrdtr)

	#port.open()

	if not port.isOpen():
		print "ERROR: ",port,"can not be opened! \n"
		sys.exit(-2)
	
	tinit = time.time()
		
	ntw = len(program_cmd)
	cw = 1

	for cmd in program_cmd:
		for c in cmd:
			port.write(c)
			time.sleep(tintchar)
		port.write(chr(eol))
		
		if verbose:
			pcw = round(((cw*1.0)/ntw)*100.0,2)
			if lverbose == 1:
				print "Total progress: "+str(pcw)+"%"
			elif lverbose == 2:
				print str(cw)+"/"+str(ntw)+" ("+str(pcw)+"%)"
			elif lverbose == 3:
				print cmd+"\n"+str(cw)+"/"+str(ntw)+" ("+str(pcw)+"%)"
			else:
				tc = time.time();
				telapsed = int(tc-tinit)
				tv = time_convert(telapsed)
				hours = tv[0]
				minutes = tv[1]
				secs = tv[2]
				print "["+str(hours)+":"+str(minutes)+":"+str(secs)+"] "+cmd+"\n"+str(cw)+"/"+str(ntw)+" ("+str(pcw)+"%)"
		
		cw = cw+1
		
	tend = time.time()
	telapsed = int(tend-tinit)
	
	tv = time_convert(telapsed)
	
	hours = tv[0]
	minutes = tv[1]
	secs = tv[2]
	
	print("Time elapsed: "+str(hours)+" hours, "+str(minutes)+" minutes, "+str(secs)+" seconds")
	
	port.close()

print
print "============================================================================"
print "\t\t White-Rabbit Programer v1 (OBP) \t\t"
print "============================================================================"

print
print "Note: An OBP and/or Etherbone core is needed in your gateware for this script..."
print

if len(sys.argv) < 2:
	help(sys.argv[0])
	sys.exit(-1)
	
[verbose_c,verbose_l] = input_verbose()

dev = sys.argv[1]

dev_l = dev.lower()

if dev_l == "etherbone":
	if len(sys.argv) == 6:
		ip = sys.argv[2]
		port = sys.argv[3]
		program = sys.argv[4]
		program_old = program
		program_new = sys.argv[5]
		print "Checking changes and Loading program..."
	elif len(sys.argv) == 5:
		ip = sys.argv[2]
		port = sys.argv[3]
		program = sys.argv[4]
		program_old = program
		program_new = None
		print "Loading program..."
	else:
		help(sys.argv[0])
		sys.exit(-1)

else:	
	program = sys.argv[2]

	if len(sys.argv) == 4:
		program_old = program
		program_new = sys.argv[3]
		print "Checking changes and Loading program..."
	elif len(sys.argv) == 3:
		program_old = program
		program_new = None
		print "Loading program..."
	else:
		help(sys.argv[0])
		sys.exit(-1)
		

pcmd = load_programs(program_old,program_new)
print "Program loaded!"
print "Starting to program..."

if dev == "etherbone":
	etherbone_program(pcmd,ip,port,verbose=(verbose_c == 'y'),lverbose=verbose_l)
else:
	wr_program(dev,pcmd,tintchar=0.001,verbose=(verbose_c == 'y'),lverbose=verbose_l)

print "Device programed!"
print
