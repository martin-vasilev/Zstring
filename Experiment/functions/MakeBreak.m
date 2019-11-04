function [ ] = MakeBreak(break_time)
%MakeBreak Interupts the experiment to make a break
%   Press any key to abort and continue with experiment
% Martin R. Vasilev, 2019

global Monitor Visual el;

% clear screen just in case:
Screen('FillRect', Monitor.window, Visual.BGC); % clear subject screen
Screen('Flip', Monitor.window);

% base text: 
InstrText=  'Break time:';
ContinueText= 'Click the mouse if you prefer to skip the break.';

% plot things on screen:
Screen('DrawText', Monitor.window, InstrText,  Visual.resX/2-100, Visual.resY/2 -100, Visual.FGC);
Screen('DrawText', Monitor.window, ContinueText,  Visual.resX/2-315, Visual.resY/2+100, [163, 163, 163]);
Screen('Flip', Monitor.window);

%Take a screen shot and send to Eye link
imageArray= Screen('GetImage', Monitor.window, [0 0 Visual.resX Visual.resY]);      
imwrite(imageArray, 'disp.bmp');
Eyelink('Command', 'set_idle_mode');
Eyelink('Command', 'clear_screen 0');
status= Eyelink('ImageTransfer', 'disp.bmp', 0, 0, 0, 0,0, 0, 16);

% prep some variables:
breakDone= false;
Eyelink('Message', 'BREAK STARTED');
startTime= GetSecs;

while ~breakDone
    currentTime= GetSecs- startTime;
    [x,y,buttons] = GetMouse(Monitor.window);
    
    if buttons(1) || buttons(2)
        breakDone= true; % exit loop if terminated by user
    end
    
    if currentTime> break_time
        breakDone= true; % exit loop of break time is over
    end
    
    if ~breakDone % print time on screen until done
        
        % print remaining time to the screen:
        time_left= break_time- currentTime;
        minutes= floor(time_left/60);
        seconds= round(abs(minutes*60- time_left));
        
        if seconds<10
            time_string= [num2str(minutes) ':' '0' num2str(seconds) ' minutes left'];
        else
            time_string= [num2str(minutes) ':' num2str(seconds) ' minutes left'];
        end
        
        % Update remaining time in real time:
        Screen('DrawText', Monitor.window, InstrText,  Visual.resX/2-100, Visual.resY/2 -90, Visual.FGC);
        Screen('DrawText', Monitor.window, ContinueText,  Visual.resX/2-315, Visual.resY/2+100, [163, 163, 163]);
        Screen('DrawText', Monitor.window, time_string,  Visual.resX/2-130, Visual.resY/2, [139, 0, 0]);
        Screen('Flip', Monitor.window);
    
        WaitSecs(0.25); % wait a quarter second (so that we don't overload monitor)
    end
end

Eyelink('Message', 'BREAK ENDED');

% clear screen after break is finished:
Screen('FillRect', Monitor.window, Visual.BGC); % clear subject screen
Screen('Flip', Monitor.window);

% Re-calibrate after brake:
EyelinkDoTrackerSetup(el);

end
