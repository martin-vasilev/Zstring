function [text] = format_text(string, ResX, PixPerChar, Xoffset)

spaces= strfind(string, ' '); 
usable_scr= ResX- 2*Xoffset; 
usable_scr_letters= usable_scr/ PixPerChar;
n_line= length(string)/ usable_scr_letters; % number of lines to fit text
n_line= ceil(n_line); % round up to integer

%% break the string in lines:
text='';

for i=1:n_line    
    if i==1
        diff= abs(spaces-usable_scr_letters);
        [~,pos]= min(diff);
        if spaces(pos)>usable_scr_letters
            line_break= spaces(pos-1)-1; %-1 to remove empty space
        else
            line_break= spaces(pos)-1; %-1 to remove empty space
        end
        breaked= line_break+1; % to count correctly in string
       
        %generate text:
        tmp_txt= [string(1:line_break) '' '\n'];
        text= [text '' tmp_txt];
    else
        diff= abs(spaces- (breaked+ usable_scr_letters));
        [~,pos]= min(diff);
        
        if spaces(pos)> (breaked+ usable_scr_letters)
            line_break= spaces(pos-1)-1; %-1 to remove empty space
        else
            line_break= spaces(pos)-1; %-1 to remove empty space
        end
        breaked_new= line_break+1; % to count correctly in string
        if i== n_line && length(string) < breaked+usable_scr_letters 
            tmp_txt= [string(breaked+1:end) '' '\n'];
        elseif i== n_line && length(string) > breaked+usable_scr_letters
            tmp_txt= [string(breaked+1:line_break) '' '\n' string(line_break+2:end)];
        else
            tmp_txt= [string(breaked+1:line_break) '' '\n'];
        end
        
        text= [text '' tmp_txt];
        breaked= breaked_new;
    end
    
end

%text= [text '' string(breaked+1:end)]; 


end

