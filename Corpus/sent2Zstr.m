function [new_string] = sent2Zstr(string, n)
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

words= strsplit(z_string, ' ');

word_num= randperm(length(words), n);

for i= 1:length(word_num)
    new_word= char(words(word_num(i)));
    which_letter= randperm(length(new_word), 1);
    new_word(which_letter)= 's';
    
    words(word_num(i))= {new_word};
end

new_string= strjoin(words, ' ');

if new_string(1)== 's'
    new_string(1)= 'S';
end

end

