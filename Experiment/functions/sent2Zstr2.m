function [new_z_text] = sent2Zstr2(string, n)
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

% add random different letters:
z_words= strsplit(z_string, ' ');

which_words= randperm(length(z_words), n);

to_change= z_words(which_words);

for i=1:length(to_change)
    curr_word= char(to_change(i));
    which_letter= randperm(length(curr_word), 1);
    curr_word(which_letter)= 's';
    to_change(i)= {curr_word};
end

z_words(which_words)= to_change;

new_z_text= strjoin(z_words, ' ');

end

