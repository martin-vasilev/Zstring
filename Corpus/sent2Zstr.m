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

Reg1= [2, floor(length(words)/4)];
Reg2= [Reg1(2)+1, Reg1(2)+  floor(length(words)/4)];
Reg3= [Reg2(2)+1, Reg2(2) +  floor(length(words)/4)];
Reg4= [Reg3(2)+1, Reg3(2) +  floor(length(words)/4)];


Reg4(2)= length(words);


Regs= [Reg1;Reg2;Reg3;Reg4];

word_reg= randperm(length(Regs), n);

word_num= [];

for i=1:n
    word_num(i)= randi(Regs(word_reg(i),:), 1);
end

for i= 1:length(word_num)
    new_word= char(words(word_num(i)));
    which_letter= randperm(length(new_word), 1);
    new_word(which_letter)= 'o';
    
    words(word_num(i))= {new_word};
end

new_string= strjoin(words, ' ');

if new_string(1)== 'o'
    new_string(1)= 'O';
end

end

