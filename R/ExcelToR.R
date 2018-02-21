ExcelToR <- function(file_align = 'alignment.xlsx', null.tokens = TRUE, len = len)
{
fg = c()
#wb = loadWorkbook(file_align)
for(sn in 1 : len)
{
df1 <- read.xlsx (xlsxFile = file_align, sheet = sn)
df1 = as.matrix(df1)
fg[[sn]] = df1
}
fg1 = ifelse(null.tokens, 'null', 'nolink')
save(fg, fg1, file = paste(file_align,'.RData',sep=''))
    
cat(paste(file_align,'.RData',' created','\n', sep=''))
    
}
