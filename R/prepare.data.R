prepare.data <-
function(file.sorc, file.trgt, n = -1L, encode.sorc = 'unknown' , encode.trgt = 'unknown', min.len = 5, max.len = 40, remove.pt = TRUE, word.align = TRUE)
{
s_sen = t_sen = aa = t = c()

s_sen = readLines (con <- file(file.sorc), encoding = encode.sorc, n = n, warn = FALSE)
close(con)

t_sen = readLines (con <- file(file.trgt), encoding = encode.trgt, n = n, warn = FALSE)
close(con)

if (length(s_sen) == length(t_sen))
{
for (k1 in 1 : length (s_sen)) if (s_sen[k1] == '') {t_sen [k1+1] = paste (t_sen [k1], t_sen [k1+1]); t_sen [k1] = ''}

for (k2 in 1 : length (t_sen)) if (t_sen[k2] == '') {s_sen [k2+1] = paste (s_sen [k2], s_sen [k2+1]); s_sen [k2] = ''}
}

s_sen = s_sen [nzchar (s_sen)]
t_sen = t_sen [nzchar (t_sen)]

aa = cbind(s_sen,t_sen)
len1 = nrow(aa)

#------------------------- Tokenization --------------------------
    
aa[,1] = nfirst2lower (aa [,1])
aa[,2] = nfirst2lower (aa [,2])

rm (s_sen, t_sen)
gc ()

if(remove.pt) aa = sapply(1:(2*len1), function(x)remove.punct(aa[[x]]))

if(!remove.pt) aa = strsplit(aa,' ')

word2 = aa [1 : len1]
word3 = aa [ (len1+1) : (2 * len1)]

aa = cbind (sapply(word2,paste, collapse = ' '), sapply(word3, paste, collapse = ' '))
aa = aa [apply (aa, 1, function(x) prod (vapply (strsplit (x, ' '), length, FUN.VALUE=0) >= min.len)& prod (vapply (strsplit (x, ' '), length, FUN.VALUE=0) <= max.len) == 1) ,]


if(word.align) 
{
aa = list (len1, aa)
return(aa)
}

len2 = length(aa) / 2
if(remove.pt) aa = sapply(1:(2*len2), function(x)remove.punct(aa[[x]]))

if(!remove.pt) aa = strsplit(aa,' ')

list1 = list (initial = len1, used = len2, sorc.tok = aa [1 : len2], trgt.tok = aa[ (len2 + 1) : (2 * len2)] )

return (list1)
}
