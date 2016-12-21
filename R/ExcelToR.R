ExcelToR <- function(file_align = "alignment.xlsx", null.tokens = TRUE, len = TRUE)
{
fg = c()
if (len == TRUE) len = length(sheets(loadWorkbook(file_align)))

for(sn in 1 : len)
{
df1 <- read.xlsx (xlsxFile = file_align, sheet = sn)
df1 = as.matrix(df1)
fg[[sn]] = df1
}
if (null.tokens) {
   fg1 = "null"
} else {
   fg1 = "nolink"
}
save(fg, fg1, file = paste(file_align,'RData',sep='.'))
    
cat(paste(file_align,'.RData',' created','\n', sep=''))
    
}
