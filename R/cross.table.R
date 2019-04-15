cross.table<-
function ( ..., null.tokens = TRUE, out.format = c('rdata','excel'), file.align = 'alignment')
{
out.format = match.arg(out.format)

p1 = prepare.data (..., word.align = FALSE)
len = p1 $ used
p1 = unlist (p1, recursive = FALSE)

if (null.tokens) {
   p1 = sapply(3 : length(p1), function(x) c('null', p1[[x]])); fg1 = "null"
} else {
   p1 = sapply(3 : length(p1), function(x) p1[[x]]);  fg1 = "nolink"
}

if (out.format == 'rdata')
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
save(fg, fg1, file = paste(file.align,'RData',sep='.'))
print(paste(getwd(), '/', file.align,'.RData',' created',sep=''))
}

if (out.format == 'excel')
{
file_align = paste(file.align,'xlsx',sep='.')
wb1 <- createWorkbook ("data")
for (j in 1 : len)
{
m1 = matrix (0, length (p1 [[j]]) + 1, length (p1 [[j + len]]) + 1)
m1 [2 : nrow (m1), 1] = p1 [[j]]; m1 [1, 2 : ncol (m1)] = p1 [[j + len]]; m1 [1, 1] = ''
addWorksheet (wb1, as.character(j))
writeData (wb1, sheet =j, m1)
saveWorkbook (wb1, file_align, overwrite = TRUE)
}
cat (paste("Now, please edit ","'", file.align,"'",".", "\nIf you want to build a gold standard, please enter 1|2 for Sure|Possible alignments.\nIf you want to construct an alignment matrix which is computed by another software, please enter '1' for alignments.\nImportant: In order to use the created excel file for evaluation function,\ndon't forget to use excel2rdata function to convert the excel file into required R format.\n(evaluation and excel2rdata are functions in the current package.)\n ",sep=''))
print(paste(getwd(), '/', file.align,' created',sep=''))
}
}
