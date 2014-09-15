The Developers Candidate Exercise has been moved into a VirtualBox Appliance.

_Step-by-step instructions:_

Download VirtualBox
-------------------
from this URL:  http://www.virtualbox.org
Pick the latest one (or at least 4.3.16) for your host system.
Install it by following the instructions for you host system.


Download the VirtualBox Appliance
----------------------------------
from this URL: http://www.thejeeper.net/BatMachine.ova
Once this is done, it's time for the next step.


Import Appliance into VirtualBox
---------------------------------
* Start VirtualBox
* Select "File/Import Appliance" from dropdown menu
* Select the BatMachine.ova file that you have downloaed in the previous step
* You might need to adjust the "Virtual Disk Image" path in the screen that shows details about the Appliance
* Click "Import"
* After a short while the virtual machine for "BatMachine" is ready to be started in VirtualBox
* :) Relax and enjoy the moment "off"

Start the Appliance
-------------------
* Doubleclick "BatMachine" in VirtualBox


Logging into the Operating System
---------------------------------
The username for this exercise is

   babe

and the password is

   Ruth

NOTE: Capitalization IS significant.

Once you're logged in, type

   cat README

to run the virtual machine.


Reasoning:
----------
I moved the developers exercise into a VirtualBox Appliance for a few reasons:
* I find it easier to provide a turn-key solution
* My code and needed installation requirements do not interfere with your computer
* I can make sure that all requirements are in place and that there are no issues that are outside my control


Technologies Used:
------------------
Ubuntu 14.04.1 LTS
Erlang R15B03 64bit
MySQL 5.5.38-0ubuntu0.14.04.1 (Ubuntu)
Emysql 0.4.1 erlang MySQL driver
GNU Emacs 24.3.1 with Erlang Syntax support 


Stopping the VirtualMachine:
----------------------------
Execute

   sudo halt

on the command line

The password that the system is asking for is

  Ruth

NOTE: Capitalization IS significant.

Then you can turn off VirtualBox as well.
