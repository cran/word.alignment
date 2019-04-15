remove.punct<-function(text)
{
x1 = strsplit(text,' ')[[1]]
x2 = gsub('[!?,{}"();:]','',x1)
x3 = gsub("'s"," 's",x2)
x3 = unlist(strsplit(x3,' '))
x3 = gsub("'",'',x3)
x4 = gsub('[[]','',x3)
x5 = gsub('[]]','',x4)
x5 = x5[nzchar(x5)]
if(x5[length(x5)]==".")
x5 = x5[-length(x5)]
return (x5)
}
