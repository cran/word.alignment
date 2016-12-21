cons.agn <-
function (tst.set_sorc, tst.set_trgt, nrec = -1, minlen = 5, maxlen = 40, ul_s = FALSE, ul_t = TRUE, removePt = TRUE, all = FALSE,  null.tokens = TRUE, Format = c('R','Excel'), file_align = "alignment")
{
Format = match.arg(Format)

p1 = prepareData (tst.set_sorc, tst.set_trgt, nrec = nrec, minlen = minlen, maxlen = maxlen, ul_s = ul_s, ul_t = ul_t, removePt = removePt, all = all, word_align = FALSE)
len = p1 $ used
p1 = unlist (p1, recursive = FALSE)

if (null.tokens) {
   p1 = sapply(3 : length(p1), function(x) c('null', p1[[x]])); fg1 = "null"
} else {
   p1 = sapply(3 : length(p1), function(x) p1[[x]]);  fg1 = "nolink"
}

if (Format == 'R')
{
readline(paste("If you want to build a gold standard, please enter '1|2' for Sure|Possible alignments. \nIf you want to construct an alignment matrix which is computed by another software, please enter '1' for alignments.\nNow, press 'Enter' to continue.",sep=''))

mm = sapply (1 : len, function (x) {m = matrix (0, length (p1 [[x]]) + 1, length (p1 [[x + len]]) + 1);
     m [2 : nrow (m), 1] = p1 [[x]]; m [1, 2 : ncol(m)] = p1 [[x+len]]; m [1, 1] = ''; m})

fg = c()
for(sn in 1 : len)
{
fg2 = mm [[sn]]
fg2 = fix (fg2)
fg[[sn]] = fg2
}
save(fg, fg1, file = paste(file_align,'RData',sep='.'))
print(paste(getwd(), '/', file_align,'.RData',' created',sep=''))
}

if (Format == 'Excel')
{
file_align = paste(file_align,'xlsx',sep='.')
wb1 <- createWorkbook ("data")
for (j in 1 : len)
{
m1 = matrix (0, length (p1 [[j]]) + 1, length (p1 [[j + len]]) + 1)
m1 [2 : nrow (m1), 1] = p1 [[j]]; m1 [1, 2 : ncol (m1)] = p1 [[j + len]]; m1 [1, 1] = ''
addWorksheet (wb1, as.character(j))
writeData (wb1, sheet =j, m1)
saveWorkbook (wb1, file_align, overwrite = TRUE)
}
cat (paste("Now, please edit ","'", file_align,"'",".", "\nIf you want to build a gold standard, please enter 1|2 for Sure|Possible alignments.\nIf you want to construct an alignment matrix which is computed by another software, please enter '1' for alignments.\nImportant: In order to use the created excel file for Evaluation1 function,\ndon't forget to use ExcelToR function to convert the excel file into required R format.\n(ExceltoR is a function in the current package.)\n ",sep=''))
print(paste(getwd(), '/', file_align,' created',sep=''))
}
}
