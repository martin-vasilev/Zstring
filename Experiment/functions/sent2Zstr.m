function [z_string] = sent2Zstr(string)
%z_string Converts a sentence to a z-string sentence
%   Detailed explanation goes here

%words= strsplit(string, ' ');
z_string= [];

for i=1:length(string)
    if string(i)== ' '
        z_string= [z_string ' '];
    else
        z_string= [z_string 'z'];
    end
end

idx= isstrprop(string, 'upper');

% change upper case letters:
z_string(idx)= 'Z';

end

