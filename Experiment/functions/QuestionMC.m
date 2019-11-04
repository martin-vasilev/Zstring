function [answer]=QuestionMC(question, opts, corr_ans, item, cond,D)

% Present a multiple-choice comprehension question:

global Visual Monitor const;

% Find out how many screens and use largest screen number.
%whichScreen = max(Screen('Screens'));

% prepare string to print coords to edf file:
questionString= [question '\n' strjoin(opts, '\n')];

%%
% Setup window:
%Monitor.window = Screen('OpenWindow', whichScreen);
trialStart= GetSecs;

Screen('FillRect', Monitor.window, Visual.BGC);
Screen('Flip', Monitor.window);

%Screen('DrawText', Monitor.window, question , Visual.sentPos(1), ...
%    Visual.sentPos(2), Visual.FGC) % sentence

dim1= [Visual.sentPos(1)+50-10  Visual.sentPos(2)+Visual.LetterHeight*3-Visual.LetterHeight/2  Visual.sentPos(1)+50+40 Visual.sentPos(2)+Visual.LetterHeight*4+Visual.LetterHeight/2];
dim2= [Visual.sentPos(1)+50-10  Visual.sentPos(2)+Visual.LetterHeight*5+Visual.LetterHeight/2  Visual.sentPos(1)+50+40 Visual.sentPos(2)+Visual.LetterHeight*7+Visual.LetterHeight/2];
dim3= [Visual.sentPos(1)+50-10  Visual.sentPos(2)+Visual.LetterHeight*8+Visual.LetterHeight/2  Visual.sentPos(1)+50+40 Visual.sentPos(2)+Visual.LetterHeight*10+Visual.LetterHeight/2];
dim4= [Visual.sentPos(1)+50-10  Visual.sentPos(2)+Visual.LetterHeight*11+round(Visual.LetterHeight/2.3)  Visual.sentPos(1)+50+40 Visual.sentPos(2)+Visual.LetterHeight*13+round(Visual.LetterHeight/2.3)];

% draw answer boxes:
Screen('FillRect', Monitor.window , [210 210 210], dim1);
Screen('FillRect', Monitor.window , [210 210 210], dim2);
Screen('FillRect', Monitor.window , [210 210 210], dim3);
Screen('FillRect', Monitor.window , [210 210 210], dim4);

% draw answers: 
% Screen('DrawText', Monitor.window, char(opts(1)), dim1(1)+ 15, ...
%     dim1(2)+10, Visual.FGC);
% Screen('DrawText', Monitor.window, char(opts(2)), dim2(1)+ 15, ...
%     dim2(2)+10, Visual.FGC); 
% Screen('DrawText', Monitor.window, char(opts(3)), dim3(1)+ 15, ...
%     dim3(2)+10, Visual.FGC); 
% Screen('DrawText', Monitor.window, char(opts(4)), dim4(1)+ 15, ...
%     dim4(2)+10, Visual.FGC); 
DrawFormattedText(Monitor.window, questionString, Visual.sentPos(1)+50, Visual.sentPos(2), Visual.FGC, ...
                          [], [], [], Visual.TextSpacing*1.65);

% print text to asc file:
%get string to print:

% strPrint= [question ''];


% % draw answer number:
% Screen('DrawText', Monitor.window, '1' , dim1(1) + (dim1(3)-dim1(1))/2, ...
%     dim1(2) + 10, Visual.FGC); % sentence
% Screen('DrawText', Monitor.window, '2' , dim2(1) + (dim2(3)-dim2(1))/2, ...
%     dim2(2) + 10, Visual.FGC);
% Screen('DrawText', Monitor.window, '3' , dim3(1) + (dim3(3)-dim3(1))/2, ...
%     dim3(2) + 10, Visual.FGC); % sentence
% Screen('DrawText', Monitor.window, '4' , dim4(1) + (dim4(3)-dim4(1))/2, ...
%     dim4(2) + 10, Visual.FGC);

Screen('Flip', Monitor.window);

%SetMouse(Visual.resX/2,Visual.resY/2);
ShowCursor(0); 

imageArray= Screen('GetImage', Monitor.window, [0 0 Visual.resX Visual.resY]);
imwrite(imageArray, 'disp_Q.bmp');
        
        
Eyelink('Command', 'set_idle_mode');
Eyelink('Command', 'clear_screen 0');
status= Eyelink('ImageTransfer', 'disp_Q.bmp', 0, 0, 0, 0,0, 0, 16);

% Initial question stamps:
Eyelink('Message', ['TRIALID F' num2str(cond) 'I' num2str(item) 'D' num2str(D)]);
stim2edfML(questionString); % print question string to edf file:

Eyelink('Message', ['QUESTION_ANSWER ' num2str(corr_ans)]);
Eyelink('Message', 'DELAY 500 MS');
Eyelink('StartRecording');
WaitSecs(0.05);
Eyelink('command', ['record_status_message ' ['Question ' 'F' num2str(cond) 'I' num2str(item) 'D' num2str(D)]]);

WaitSecs(0.5);
Eyelink('Message', 'DISPLAY ON');
Eyelink('Message', 'SYNCTIME');


answer=-1; 
escapeKey= KbName('ESCAPE');
confirmKey= KbName('Y');

while answer<0
    trialTime= GetSecs- trialStart;
    if trialTime> const.TrialTimeout
        answer=0;
    end
    
    [x,y,buttons] = GetMouse(Monitor.window);
    
    [keyIsDown, seconds, keyCode]= KbCheck;
    keyCode= find(keyCode,1);
    if keyCode== escapeKey
  
        status= Eyelink('ReceiveFile');
        Eyelink('Shutdown');
        Screen('CloseAll');
        error('Experiment terminated by user');
    end
    
    if buttons(1)==1
        if IsInRect(x,y, dim1) %x> yes_dim(1) && y> yes_dim(2) && x< yes_dim(3) && y< yes_dim(4)
            %Screen('DrawText', Monitor.window, question , Visual.sentPos(1), Visual.sentPos(2), Visual.FGC) % sentence
            
            Screen('FillRect', Monitor.window , [189 255 183], dim1);
            Screen('FillRect', Monitor.window , [210 210 210], dim2);
            Screen('FillRect', Monitor.window , [210 210 210], dim3);
            Screen('FillRect', Monitor.window , [210 210 210], dim4);
            
            % draw answers:
            DrawFormattedText(Monitor.window, questionString, Visual.sentPos(1)+50, Visual.sentPos(2), Visual.FGC, ...
                          [], [], [], Visual.TextSpacing*1.65);
%             Screen('DrawText', Monitor.window, char(opts(1)), dim1(1)+ 15, ...
%                 dim1(2)+10, Visual.FGC);
%             Screen('DrawText', Monitor.window, char(opts(2)), dim2(1)+ 15, ...
%                 dim2(2)+10, Visual.FGC); 
%             Screen('DrawText', Monitor.window, char(opts(3)), dim3(1)+ 15, ...
%                 dim3(2)+10, Visual.FGC); 
%             Screen('DrawText', Monitor.window, char(opts(4)), dim4(1)+ 15, ...
%                 dim4(2)+10, Visual.FGC); 
            
            Screen('Flip', Monitor.window);
            WaitSecs(0.5);
            answer= 1;
        end
        
        if IsInRect(x,y, dim2) %x> no_dim(1) && y> no_dim(2) && x< no_dim(3) && y< no_dim(4)
            %Screen('DrawText', Monitor.window, question , Visual.sentPos(1), Visual.sentPos(2), Visual.FGC) % sentence
            
            Screen('FillRect', Monitor.window , [189 255 183], dim2);
            Screen('FillRect', Monitor.window , [210 210 210], dim1);
            Screen('FillRect', Monitor.window , [210 210 210], dim3);
            Screen('FillRect', Monitor.window , [210 210 210], dim4);
            
            % draw answers:
            DrawFormattedText(Monitor.window, questionString, Visual.sentPos(1)+50, Visual.sentPos(2), Visual.FGC, ...
                          [], [], [], Visual.TextSpacing*1.65);
%             Screen('DrawText', Monitor.window, char(opts(1)), dim1(1)+ 15, ...
%                 dim1(2)+10, Visual.FGC);
%             Screen('DrawText', Monitor.window, char(opts(2)), dim2(1)+ 15, ...
%                 dim2(2)+10, Visual.FGC); 
%             Screen('DrawText', Monitor.window, char(opts(3)), dim3(1)+ 15, ...
%                 dim3(2)+10, Visual.FGC); 
%             Screen('DrawText', Monitor.window, char(opts(4)), dim4(1)+ 15, ...
%                 dim4(2)+10, Visual.FGC); 
            
           Screen('Flip', Monitor.window);
            WaitSecs(0.5);
            answer= 2;
        end
        
        if IsInRect(x,y, dim3) %x> no_dim(1) && y> no_dim(2) && x< no_dim(3) && y< no_dim(4)
           % Screen('DrawText', Monitor.window, question , Visual.sentPos(1), Visual.sentPos(2), Visual.FGC) % sentence
            
            Screen('FillRect', Monitor.window , [189 255 183], dim3);
            Screen('FillRect', Monitor.window , [210 210 210], dim1);
            Screen('FillRect', Monitor.window , [210 210 210], dim2);
            Screen('FillRect', Monitor.window , [210 210 210], dim4);
            
            % draw answers:
            DrawFormattedText(Monitor.window, questionString, Visual.sentPos(1)+50, Visual.sentPos(2), Visual.FGC, ...
                          [], [], [], Visual.TextSpacing*1.65);
%             Screen('DrawText', Monitor.window, char(opts(1)), dim1(1)+ 15, ...
%                 dim1(2)+10, Visual.FGC);
%             Screen('DrawText', Monitor.window, char(opts(2)), dim2(1)+ 15, ...
%                 dim2(2)+10, Visual.FGC); 
%             Screen('DrawText', Monitor.window, char(opts(3)), dim3(1)+ 15, ...
%                 dim3(2)+10, Visual.FGC); 
%             Screen('DrawText', Monitor.window, char(opts(4)), dim4(1)+ 15, ...
%                 dim4(2)+10, Visual.FGC); 
            
           Screen('Flip', Monitor.window);
            WaitSecs(0.5);
            answer= 3;
        end
        
        if IsInRect(x,y, dim4) %x> no_dim(1) && y> no_dim(2) && x< no_dim(3) && y< no_dim(4)
           % Screen('DrawText', Monitor.window, question , Visual.sentPos(1), Visual.sentPos(2), Visual.FGC) % sentence
            
            Screen('FillRect', Monitor.window , [189 255 183], dim4);
            Screen('FillRect', Monitor.window , [210 210 210], dim1);
            Screen('FillRect', Monitor.window , [210 210 210], dim2);
            Screen('FillRect', Monitor.window , [210 210 210], dim3);
            
            % draw answers:
            DrawFormattedText(Monitor.window, questionString, Visual.sentPos(1)+50, Visual.sentPos(2), Visual.FGC, ...
                          [], [], [], Visual.TextSpacing*1.65);
%             Screen('DrawText', Monitor.window, char(opts(1)), dim1(1)+ 15, ...
%                 dim1(2)+10, Visual.FGC);
%             Screen('DrawText', Monitor.window, char(opts(2)), dim2(1)+ 15, ...
%                 dim2(2)+10, Visual.FGC); 
%             Screen('DrawText', Monitor.window, char(opts(3)), dim3(1)+ 15, ...
%                 dim3(2)+10, Visual.FGC); 
%             Screen('DrawText', Monitor.window, char(opts(4)), dim4(1)+ 15, ...
%                 dim4(2)+10, Visual.FGC); 
            
           Screen('Flip', Monitor.window);
            WaitSecs(0.5);
            answer= 4;
        end
        
    end
    
%questEnd= mouseClicked && answered;
end

if answer==corr_ans
    Eyelink('command', ['record_status_message ' 'CORRECT!']);
else
    Eyelink('command', ['record_status_message ' 'INCORRECT!']);
end
WaitSecs(0.5);

Eyelink('StopRecording');

% Print end of question stamps to edf:
Eyelink('Message', ['ENDBUTTON ' num2str(answer)]);
Eyelink('Message', 'DISPLAY OFF');
Eyelink('Message', ['TRIAL_RESULT ' num2str(answer)]);
Eyelink('Message', 'TRIAL OK');


Screen('FillRect', Monitor.window, Visual.BGC);
Screen('Flip', Monitor.window);
Eyelink('command', 'clear_screen 0'); % clear tracker screen
HideCursor;

%Screen('CloseAll'); 
