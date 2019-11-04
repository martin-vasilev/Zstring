global const Visual sent Monitor el Audio; 

%const.ntrials=5; % TEMPORARY!!!

HideCursor;

% Calibrate the eye tracker
EyelinkDoTrackerSetup(el);

% Blackbox toolkit testing:
%s= serial('COM11');
%set(s, 'BaudRate', 115200, 'DataBits', 8, 'StopBits', 1, 'Parity', 'none')
%fopen(s);
%fprintf(s, 'RR');
%fprintf(s,'FF');

breaks= [48,96, 144]; % scheduled break
Instr_break= [1, 98]; % point at which to present instructions

normal_reading= ['You will now see different short sentences. Your task is to\n' ...
                 'read them in your head for comprehension. Every now and then, you\n' ...
                 'will be asked an easy multiple-choice question. Sometimes, you \n'...
                 'will hear short sounds while you read. Do your best to ignore\n'...
                 'them and focus on reading the sentences.'];
             
zString_reading= ["You will now see a number of short letter strings. Your task is to\n"...
                  "scan the strings one by one and count how many times the\n" ...
                  "letter 's' appears in them. This letter can appear anywhere\n"...
                  "on the screen, so make sure you scan all strings carefully.\n"...
                  "Every now and then, you will be asked how many times the\n"...
                  "letter 's' appeared in the strings. Sometimes, you will hear\n"...
                  'short sounds while you do the task. Do your best to ignore\n'...
                  'them and focus on scanning the strings.'];
zString_reading=   char(strjoin(zString_reading, ''));


for i=1:const.ntrials
    
    % break:
    if ismember(i, breaks)
       MakeBreak(const.BreakDur);
    end
   

    trialEnd= false; 
	item= design.item(i);
	task= char(design.task(i));
    hasQuest= sent.hasQuest(item);
    
    
    % Instructions:
    if ismember(i, Instr_break)
        if task== 'reading'
            Instruction(normal_reading, Visual.FGC);
        else
            Instruction(zString_reading, Visual.FGC);
        end
    end
    
	% condition:
    if strcmp(char(design.sound(i)), 'PRAC')==1
        cond= 9; % practice
    end
    
    if strcmp(char(design.sound(i)), 'SLC')==1
        cond= 1; % silence
    end
    
    if strcmp(char(design.sound(i)), 'STD')==1
        cond= 2; % standard
    end
    
    if strcmp(char(design.sound(i)), 'DEV')==1
        cond= 3; % deviant
    end
    
	% position of sound (only applicable to deviant sounds):
	soundPos= design.pos(i);
    
    % sentence string:
    if strcmp(task, 'reading')==1 % natural reading
        sentenceString= char(sent.Sentence(item)); % normal reading
    else
        sentenceString= char(sent.Zstring(item)); % z-string reading
    end

    if cond<9 % if not practice

        % get word boundaries:
        Bnds= getBnds(sentenceString);

        boundary1= Bnds(2);
        boundary2= Bnds(4);
        boundary3= Bnds(6);
        boundary4= Bnds(8);
        boundary5= Bnds(10);

        boundary1Crossed= false;
        boundary2Crossed= false;
        boundary3Crossed= false;
        boundary4Crossed= false;
        boundary5Crossed= false;

        % time sound was played
        tPlay1= 0;
        tPlay2= 0;
        tPlay3= 0;
        tPlay4= 0;
        %tPlay5= 0;

        % time elapsed since sound was played
        tSound1= 0;
        tSound2= 0;
        tSound3= 0;
        tSound4= 0;
        %tSound5= 0;

        % if number of sounds to be played is not known in advance, take max boundaries for longest sentence.
        % Then, here, say: if boundary n doesn't exists: Boundary n crossed== True (i.e. nothing happens during trial)
    end
    
    % get sounds:
    if cond==2
		sound_type= {'STD', 'STD', 'STD', 'STD', 'STD'};
        
        sound1 = Audio.standard1;
        sound2 = Audio.standard2;
        sound3 = Audio.standard3;
        sound4 = Audio.standard4;
        sound5 = Audio.standard5;
    end
    
    
    if cond==3
        
        % Fill deviant buffer:
        [y, freq] = audioread([cd '\sounds\' 'novel' num2str(design.flnm(i)) '.wav']);
        wavedata = y';
        wavedata(2,:)= wavedata;
        nrchannels = size(wavedata,1); % Number of rows == number of channels.
        PsychPortAudio('FillBuffer', Audio.deviant, wavedata);
        
        % SOUND 1
        sound1= Audio.standard1;  % 1st sound is always standard
		
		% SOUND 2
        if soundPos==2
            sound2= Audio.deviant;
        else
            sound2= Audio.standard2;
        end
        
		% SOUND 3
		if soundPos==3
            sound3= Audio.deviant;
        else
            sound3= Audio.standard3;
        end
        
		% SOUND 4
		if soundPos==4
            sound4= Audio.deviant;
        else
            sound4= Audio.standard4;
        end
        
		% SOUND 5
		if soundPos==5
            sound5= Audio.deviant;
        else
            sound5= Audio.standard5;
        end
        
		sound_type= {'STD', 'STD', 'STD', 'STD', 'STD'};
		sound_type{soundPos}= 'DEV'; % replace only 1 deviant
    
    end
    
    % drift check:
    EyelinkDoDriftCorrection(el);
    
    %% Trial presentation:
	stimuliOn= false;
    
    while ~stimuliOn
        if item> const.Maxtrials % if practice
            Eyelink('Message', ['TRIALID ' 'P' num2str(cond) 'I' num2str(item) 'D0']);
			% print trial ID on tracker screen:
            Eyelink('command', ['record_status_message ' [ num2str(i) ':' 'P' num2str(cond) 'I' num2str(item) 'D0']]);
        else
			Eyelink('Message', ['TRIALID ' 'E' num2str(cond) 'I' num2str(item) 'D0']);
			% print trial ID on tracker screen:
			Eyelink('command', ['record_status_message ' [num2str(i) ':' 'E' num2str(cond) 'I' num2str(item) 'P' num2str(soundPos)]]); 
        end

		if cond<9
            Eyelink('Message', ['TASK: ' task]);
            Eyelink('Message', ['CRITICAL REGION 1 @ ' num2str(Bnds(2)) ' ' num2str(Bnds(2+1))]);
			Eyelink('Message', ['CRITICAL REGION 2 @ ' num2str(Bnds(4)) ' ' num2str(Bnds(4+1))]);
			Eyelink('Message', ['CRITICAL REGION 3 @ ' num2str(Bnds(6)) ' ' num2str(Bnds(6+1))]);
			Eyelink('Message', ['CRITICAL REGION 4 @ ' num2str(Bnds(8)) ' ' num2str(Bnds(8+1))]);
			Eyelink('Message', ['CRITICAL REGION 5 @ ' num2str(Bnds(10)) ' ' num2str(Bnds(10+1))]);
        end
        
        % print text stimuli to edf:
        stim2edf(sentenceString);
        
        % prepare Screens:
        Screen('FillRect', Monitor.buffer(1), Visual.FGC, [Visual.offsetX Visual.resY/2- Visual.GazeBoxSize/2 Visual.offsetX+Visual.GazeBoxSize ...
            Visual.resY/2+ Visual.GazeBoxSize]) % gazebox
        gazeBnds_x= [Visual.offsetX Visual.offsetX+Visual.GazeBoxSize];
		gazeBnds_y= [Visual.resY/2- Visual.GazeBoxSize/2 Visual.resY/2+ Visual.GazeBoxSize];
        
        
        Screen('FillRect', Monitor.buffer(2), Visual.BGC);
        Screen('DrawText', Monitor.buffer(2), sentenceString, Visual.sentPos(1), Visual.sentPos(2), Visual.FGC); % sentence
        
        if const.checkPPL
			lngth= length(sentenceString)*Visual.Pix_per_Letter;
            Screen('FrameRect', Monitor.buffer(2), Visual.FGC, [Visual.offsetX Visual.resY/2- Visual.GazeBoxSize/2 ...
                Visual.offsetX+lngth Visual.resY/2+ Visual.GazeBoxSize])
        end
        
        % Print stimuli to Eyelink monitor:
        % draw gaze box on tracker monitor:
        imageArray= Screen('GetImage', Monitor.buffer(2), [0 0 1920 1080]);
        if cond==3
            B= eval(['boundary' num2str(soundPos)]);
            imageArray(:, B-5:B+5)= 179;
        end
        
        imwrite(imageArray, 'disp.bmp');
        
        
        Eyelink('Command', 'set_idle_mode');
        Eyelink('Command', 'clear_screen 0');
        status= Eyelink('ImageTransfer', 'disp.bmp', 0, 0, 0, 0,0, 0, 16);
        
        Eyelink('Command', ['draw_filled_box ' num2str(Visual.offsetX) ' ' num2str(Visual.resY/2- Visual.GazeBoxSize/2) ' ' ...
            num2str(Visual.offsetX+Visual.GazeBoxSize) ' ' num2str(Visual.resY/2+ Visual.GazeBoxSize/2) '3']);
        

        WaitSecs(0.1);
        Eyelink('StartRecording');
        
        Screen('CopyWindow', Monitor.buffer(1), Monitor.window);
        Screen('Flip', Monitor.window);
        Eyelink('Message', 'GAZE TARGET ON');
        gazeBoxTriggered=false;
		onTarget= false;
		gazeTimeOut= false;
		gazeStart= GetSecs;

        % loop that triggers the gaze-box
		while ~gazeBoxTriggered && ~onTarget
            evt= Eyelink('NewestFloatSample');
            x = max(evt.gx); 
            y = max(evt.gy);
                
			%sample= tracker.sample(); % get current eye position
			elapsedTime= GetSecs-gazeStart; % time since gaze box appeared
			onTarget= x>= gazeBnds_x(1) && x<= gazeBnds_x(2) && y>= gazeBnds_y(1) && y<= gazeBnds_y(2);
            
			if onTarget % the eye is on the gaze box
				WaitSecs(Visual.gazeBoxDur/1000);
				onTarget= x>= gazeBnds_x(1) && x<= gazeBnds_x(2) && y>= gazeBnds_y(1) && y<= gazeBnds_y(2);
				if onTarget % eye still on gaze box after x ms
					gazeBoxTriggered= true;
					stimuliOn= true;
					%tracker.send_command("clear_screen %d" % (0))
                else
					onTarget= false;
                end
            end
			
			if elapsedTime> Visual.gazeBoxDisplayTime % gaze box timeout
                Eyelink('Message', 'TRIAL ABORTED');
				Eyelink('StopRecording');
				EyelinkDoTrackerSetup(el);
                % drift check:
                EyelinkDoDriftCorrection(el);
				onTarget= true;
				gazeBoxTriggered= true;
            end
        end

		Eyelink('Message', 'GAZE TARGET OFF');
        Eyelink('Message', 'DISPLAY ON');
        Eyelink('Message', 'SYNCTIME');
        
    end
    
    Screen('CopyWindow', Monitor.buffer(2), Monitor.window);
    Screen('Flip', Monitor.window);
	trialStart= GetSecs;
    
    %% Gaze contingent part:
    % I'm using MaxCross to limit the impact of blinks and track loses.
    % Sometimes, xpos will be > max Screen size right after the blink. 
    % This will by default trigger all boundaries that haven't been
    % crossed so far. Adding this condition limits this (sometimes). 
    
    while ~trialEnd
        trialTime= GetSecs- trialStart;
        [x,y,buttons] = GetMouse(Monitor.window);
        trialEnd= buttons(1); %KbCheck; % TEMPORARY
        
        evt= Eyelink('NewestFloatSample');
        xpos = max(evt.gx);

        
        if const.seeEye % for testing
            Screen('FillRect', Monitor.window, Visual.BGC);
            Screen('DrawText', Monitor.window, sentenceString, Visual.sentPos(1), Visual.sentPos(2), Visual.FGC); % sentence
            Screen('DrawDots', Monitor.window, [xpos, 540], 10, [0 0 0], [],2);
            Screen('Flip', Monitor.window);
        end

%       
        if cond==2 || cond==3
            
            % SOUND 1:
            if xpos> boundary1 && xpos<const.maxCross && ~boundary1Crossed
               Eyelink('Message', 'BOUNDARY CROSSED 1');
               boundary1Crossed= true;
               Eyelink('Message', ['I WAIT ' num2str(const.delay*1000) ' MS']);
               WaitSecs(const.delay);
               Eyelink('Message', ['PLAY SOUND ' char(sound_type(1))]);
               t1 = PsychPortAudio('Start', sound1, const.repetitons, 0, 1);

               %fprintf(s,'01');
               %fprintf(s,'00');
               %fclose(s);
               tPlay1= GetSecs;
            end
            tSound1= GetSecs- tPlay1;
            
            % SOUND 2:
            if xpos> boundary2 && xpos<const.maxCross && ~boundary2Crossed
               if tSound1> const.soundDur
                  Eyelink('Message', 'BOUNDARY CROSSED 2');
                  boundary2Crossed= true;
                  
                  Eyelink('Message', ['I WAIT ' num2str(const.delay*1000) ' MS']);
                  WaitSecs(const.delay);
                  Eyelink('Message', ['PLAY SOUND ' char(sound_type(2))]);
                  t2 = PsychPortAudio('Start', sound2, const.repetitons, 0, 1);
               else
                   WaitSecs(const.soundDur-tSound1)
                   Eyelink('Message', 'BOUNDARY CROSSED 2');
                   boundary2Crossed= true;
                   
                   Eyelink('Message', ['I WAIT ' num2str(const.delay*1000) ' MS']);
                   WaitSecs(const.delay);
                   Eyelink('Message', ['PLAY SOUND ' char(sound_type(2))]);
                   t2 = PsychPortAudio('Start', sound2, const.repetitons, 0, 1);

                   Eyelink('Message', 'SOUND_DELAYED 2');
               end
               tPlay2= GetSecs;

            end
            tSound2= GetSecs- tPlay2;
            
            % SOUND 3:
            if xpos> boundary3 && xpos<const.maxCross && ~boundary3Crossed
               if tSound2> const.soundDur
                   Eyelink('Message', 'BOUNDARY CROSSED 3');
                   boundary3Crossed= true;
                   
                   Eyelink('Message', ['I WAIT ' num2str(const.delay*1000) ' MS']);
                   WaitSecs(const.delay);
                   Eyelink('Message', ['PLAY SOUND ' char(sound_type(3))]);
                   t3 = PsychPortAudio('Start', sound3, const.repetitons, 0, 1);

               else
                   WaitSecs(const.soundDur-tSound2)
                   Eyelink('Message', 'BOUNDARY CROSSED 3');
                   boundary3Crossed= true;
                   
                   Eyelink('Message', ['I WAIT ' num2str(const.delay*1000) ' MS']);
                   WaitSecs(const.delay);
                   Eyelink('Message', ['PLAY SOUND ' char(sound_type(3))]);
                   t3 = PsychPortAudio('Start', sound3, const.repetitons, 0, 1);

                   Eyelink('Message', 'SOUND_DELAYED 3');
               end
               tPlay3= GetSecs;

            end
            tSound3= GetSecs- tPlay3;
            
            % SOUND 4:
            if xpos> boundary4 && xpos<const.maxCross && ~boundary4Crossed
               if tSound3> const.soundDur
                    Eyelink('Message', 'BOUNDARY CROSSED 4');
                    boundary4Crossed= true;
                    
                    Eyelink('Message', ['I WAIT ' num2str(const.delay*1000) ' MS']);
                    WaitSecs(const.delay);
                    Eyelink('Message', ['PLAY SOUND ' char(sound_type(4))]);
                    t4 = PsychPortAudio('Start', sound4, const.repetitons, 0, 1);

               else
                    WaitSecs(const.soundDur-tSound3)
                    Eyelink('Message', 'BOUNDARY CROSSED 4');
                    boundary4Crossed= true;
                    
                    Eyelink('Message', ['I WAIT ' num2str(const.delay*1000) ' MS']);
                    WaitSecs(const.delay);
                    Eyelink('Message', ['PLAY SOUND ' char(sound_type(4))]);
                    t4 = PsychPortAudio('Start', sound4, const.repetitons, 0, 1);
                    
                    Eyelink('Message', 'SOUND_DELAYED 4');
               end
               tPlay4= GetSecs;

            end
            tSound4= GetSecs- tPlay4;
            
            % SOUND 5:
            if xpos> boundary5 && xpos<const.maxCross && ~boundary5Crossed
               if tSound4> const.soundDur
                    Eyelink('Message', 'BOUNDARY CROSSED 5');
                    boundary5Crossed= true;
                    
                    Eyelink('Message', ['I WAIT ' num2str(const.delay*1000) ' MS']);
                    WaitSecs(const.delay);
                    Eyelink('Message', ['PLAY SOUND ' char(sound_type(5))]);
                    t5 = PsychPortAudio('Start', sound5, const.repetitons, 0, 1);

               else
                    WaitSecs(const.soundDur-tSound4)
                    Eyelink('Message', 'BOUNDARY CROSSED 5');
                    boundary5Crossed= true;
                    
                    Eyelink('Message', ['I WAIT ' num2str(const.delay*1000) ' MS']);
                    WaitSecs(const.delay);
                    Eyelink('Message', ['PLAY SOUND ' char(sound_type(5))]);
                    t5 = PsychPortAudio('Start', sound5, const.repetitons, 0, 1);

                    Eyelink('Message', 'SOUND_DELAYED 5');
               end

            end
        
        end
        
        if cond==1
 			% SOUND 1:
 			if xpos> boundary1 && xpos<const.maxCross && ~boundary1Crossed
                Eyelink('Message', 'BOUNDARY CROSSED 1');
 				boundary1Crossed= true;
                
                Eyelink('Message', ['I WAIT ' num2str(const.delay*1000) ' MS']);
                WaitSecs(const.delay);
                Eyelink('Message', 'PLAY SOUND SLC');
            end
             
 			% SOUND 2:
 			if xpos> boundary2 && xpos<const.maxCross && ~boundary2Crossed
                Eyelink('Message', 'BOUNDARY CROSSED 2');
 				boundary2Crossed= true;
                
                Eyelink('Message', ['I WAIT ' num2str(const.delay*1000) ' MS']);
                WaitSecs(const.delay);
                Eyelink('Message', 'PLAY SOUND SLC');
            end
             
 			% SOUND 3:
 			if xpos> boundary3 && xpos<const.maxCross && ~boundary3Crossed
                Eyelink('Message', 'BOUNDARY CROSSED 3');
 				boundary3Crossed= true;
                
                Eyelink('Message', ['I WAIT ' num2str(const.delay*1000) ' MS']);
                WaitSecs(const.delay);
                Eyelink('Message', 'PLAY SOUND SLC');
            end
             
 			% SOUND 4:
			if xpos> boundary4 && xpos<const.maxCross && ~boundary4Crossed
                Eyelink('Message', 'BOUNDARY CROSSED 4');
 				boundary4Crossed= true;
                
                Eyelink('Message', ['I WAIT ' num2str(const.delay*1000) ' MS']);
                WaitSecs(const.delay);
                Eyelink('Message', 'PLAY SOUND SLC');
            end
             
 			% SOUND 5:
			if xpos> boundary5 && xpos<const.maxCross && ~boundary5Crossed
                Eyelink('Message', 'BOUNDARY CROSSED 5');
 				boundary5Crossed= true;
                
                Eyelink('Message', ['I WAIT ' num2str(const.delay*1000) ' MS']);
                WaitSecs(const.delay);
                Eyelink('Message', 'PLAY SOUND SLC');
            end
        end
        
        
        if trialTime> const.TrialTimeout % end trial automatically if no response by participant
             trialEnd= true;
             %tracker.log('TRIAL ABORTED')
 			 Screen('FillRect', Monitor.window, Visual.BGC); % clear subject screen
             Screen('Flip', Monitor.window);
        end
        
     end
    
    Screen('FillRect', Monitor.window, Visual.BGC); % clear subject screen
    Screen('Flip', Monitor.window);
    Eyelink('command', 'clear_screen 0'); % clear tracker screen	
	
	% end of trial messages:
    Eyelink('Message', 'ENDBUTTON 5');
    Eyelink('Message', 'DISPLAY OFF');
    Eyelink('Message', 'TRIAL_RESULT 5');
    Eyelink('Message', 'TRIAL OK');

    Eyelink('StopRecording');
    
    
    %% Questions:
    if hasQuest
     
     if strcmp(task, 'reading')==1 % natural reading
        options= [ '1)  ' char(sent.O1(item)) '/n' '2)  ' char(sent.O2(item)) ...
               '/n' '3)  ' char(sent.O3(item)) '/n' '4)  ' char(sent.O4(item))];

        question= char(sent.Question(item));
        corr_answ= sent.answ(item);
        
        % present question:
        answer= QuestionMC(question, strsplit(options, '/n'), ...
              corr_answ, item, cond, 1);
          
     else % z-string reading
         
         options= '1)  5/n2)  6/n3)  7/n4)  8';
         question= 'How many times did the letter "s" appear in the string?';
         corr_answ= sent.z_answ(item);
         
         % present question:
         answer= QuestionMC(question, strsplit(options, '/n'), ...
              corr_answ, item, cond, 1);
     end
     

    end
    
    
end