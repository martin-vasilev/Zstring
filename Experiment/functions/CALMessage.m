function [] = CALMessage()
%   Detailed explanation goes here

Eyelink('Command', 'set_idle_mode');
Eyelink('Command', 'clear_screen 0');
status= Eyelink('ImageTransfer', 'CAL.bmp', 0, 0, 0, 0,0, 0, 16);

WaitSecs(4);

end

