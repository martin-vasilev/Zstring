function [ ] = sendScreenshot(window)
global Visual Monitor;

%Take a screen shot and send to Eye link
imageArray= Screen('GetImage', window, [0 0 Visual.resX Visual.resY]);      
imwrite(imageArray, 'disp.bmp');
Eyelink('Command', 'set_idle_mode');
Eyelink('Command', 'clear_screen 0');
status= Eyelink('ImageTransfer', 'disp.bmp', 0, 0, 0, 0,0, 0, 16);


end

