
function new_text = OpenQuest(instr_text, colour)
% OpenQuest Presents an open-ended question for users to write text
%   Detailed explanation goes here

%settings;
global Monitor Visual;

%text= 'Your instruction goes here:';
%colour= Visual.FGC;

% 
% oldVisualDebugLevel = Screen('Preference', 'VisualDebugLevel', 3);
% oldSupressAllWarnings = Screen('Preference', 'SuppressAllWarnings', 1);
% 	
% % Find out how many screens and use largest screen number.
% whichScreen = max(Screen('Screens'));
% 
% % Setup window:
% Monitor.window = Screen('OpenWindow', whichScreen);
% Screen('FillRect', Monitor.window, Visual.BGC);


% draw instruction:
DrawFormattedText(Monitor.window, instr_text, round(Visual.resX- Visual.resX*0.8), ...
            round(Visual.resY- Visual.resY*0.9), [207 , 87, 121], ...
            [], [], [], 2.95);
Screen('Flip', Monitor.window);

string= '';
new_text= '';
typed_text= '';
Done= false;
FlushEvents('keyDown');

while ~ Done
  %% Listen to keyboard input and print to screen:
    
    char = GetChar;

    
    if abs(char)== 8
        if ~isempty(string)
            string = string(1:length(string)-1);
        end
        
    else
        len= Visual.offsetX*2 + strlength(string)*Visual.Pix_per_Letter;
        
        if len<= Visual.resX-600
            string = [string, char];
        else
            typed_text= [typed_text '' string '' '\n'];
            string= '';
            string = [string, char];
        end
        
    end
    
    new_text= [typed_text, string];
    
    % draw instruction:
    DrawFormattedText(Monitor.window, instr_text, round(Visual.resX- Visual.resX*0.8), ...
            round(Visual.resY- Visual.resY*0.9), [207 , 87, 121], ...
            [], [], [], 2.95);
    
    % draw typed text:
    DrawFormattedText(Monitor.window, new_text, round(Visual.resX- Visual.resX*0.8), ...
            round(Visual.resY- Visual.resY*0.6), colour, ...
            [], [], [], 2.95);
        
        
    % draw end instruction:
    DrawFormattedText(Monitor.window, 'Press ENTER to continue', round(Visual.resX- Visual.resX*0.8), ...
            round(Visual.resY- Visual.resY*0.1), [166, 166, 166], ...
            [], [], [], 2.95);
    
   
    Screen('Flip', Monitor.window);
    
    if  char==13
        Done= true; % exit loop if terminated by user ('ENTER')
    end
    
 
    
end


%Screen('CloseAll');

end



