function [] = Instruction(text, colour)
%Instruction Presents instruction to participants
%   Detailed explanation goes here

global Monitor Visual;

Screen('FillRect', Monitor.window, Visual.BGC); % clear subject screen
Screen('Flip', Monitor.window);

% Draw instruction on the screen:
DrawFormattedText(Monitor.window, text, round(Visual.resX- Visual.resX*0.8), ...
            round(Visual.resY- Visual.resY*0.8), colour, ...
            [], [], [], 2.95);


Screen('DrawText', Monitor.window, 'Click the mouse to continue',  Visual.resX/2-315, Visual.resY/2+180, [0, 0, 0]);
        
Screen('Flip', Monitor.window);

sendScreenshot(Monitor.window);

InstrEnd= false;

while ~InstrEnd
    [x,y,buttons] = GetMouse(Monitor.window);
     InstrEnd= buttons(1); % wait for mouse press
end

%KbEventFlush();
%KbWait();

% clear monitor again:
Screen('FillRect', Monitor.window, Visual.BGC); % clear subject screen
Screen('Flip', Monitor.window);

end

