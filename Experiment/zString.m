% Distraction by novel sounds during natural reading and
% z-string scanning. Sounds are played with 120 ms delay relative to
% fixation onset. Experiment 3 of series
% Using novel sounds on each deviant trial

% Martin R. Vasilev, 2019

global const; 

%% settings:
clear all;
clear mex;
clear functions;

cd('C:\Users\EyeTracker\Desktop\Martin Vasilev\zString');
addpath([cd '\functions'], [cd '\corpus'], [cd '\design'], [cd '\sounds']);

settings; % load settings
ExpSetup; % do window and tracker setup

%% Load stimuli and design:
importDesign;
load('sent.mat');

const.ntrials= height(design);

whichDEV= find(strcmp('DEV', design.sound));
a= zeros(size(design,1),1);
for i=1:length(whichDEV)
    a(whichDEV(i))= i; 
end
design.flnm= a;

save(['design/sub_matrix/sub' num2str(const.ID) '.mat'], 'design');

%% Run Experiment:
runTrials;

%% Save file & Exit:
status= Eyelink('ReceiveFile');
Eyelink('Shutdown');

Screen('CloseAll');
