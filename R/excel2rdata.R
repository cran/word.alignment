excel2rdata <- function(file.align = 'alignment.xlsx', null.tokens = TRUE, len = len)
{
fg = c()
#wb = loadWorkbook(file.align)
for(sn in 1 : len)
{
df1 <- read.xlsx (xlsxFile = file.align, sheet = sn)
df1 = as.matrix(df1)
fg[[sn]] = df1
}
fg1 = ifelse(null.tokens, 'null', 'nolink')
save(fg, fg1, file = paste(file.align,'.RData',sep=''))
    
cat(paste(file.align,'.RData',' created','\n', sep=''))
    
}
