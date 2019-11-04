
cd('D:\R\Zstring\Corpus');

targets=[];
targets= repmat([1,2,3,4],1, 192/4);

sent.zStr= sent.Sentence;

for i=1:height(sent)
   
    sent.zStr(i)= sent2Zstr(char(sent.Sentence(i)), targets(i));
    
end

writetable(sent);