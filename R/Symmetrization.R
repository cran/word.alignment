Symmetrization <-
function (file_train1,file_train2, method = c ('union', 'intersection', 'grow-diag'), nrec = -1, encode.sorc = 'unknown', encode.trgt = 'unknown', iter = 4, minlen = 5, maxlen = 40, removePt = TRUE, all = FALSE, f1 = 'fa', e1 = 'en')
{
    date1 = as.POSIXlt (Sys.time(), 'Iran')
    
    method = match.arg (method)
    
    ef1 = word_alignIBM1 (file_train1, file_train2, nrec = nrec, encode.sorc = encode.sorc, encode.trgt = encode.trgt, iter = iter, minlen = minlen, maxlen = maxlen, removePt = removePt, f1 = f1, e1 = e1) $ number_align
    
    fe1 = word_alignIBM1 (file_train2, file_train1, nrec = nrec, encode.sorc = encode.trgt, encode.trgt = encode.sorc, iter = iter, minlen = minlen, maxlen = maxlen, removePt = removePt, f1 = e1, e1 = f1) $ number_align
    len = length (fe1)
    
    aa = prepareData (file_train1, file_train2, nrec = nrec, encode.sorc = encode.sorc, encode.trgt = encode.trgt, minlen = minlen, maxlen = maxlen, removePt = removePt, all = all, word_align = TRUE)
    
    aa = aa[[2]]
    
    aa[,1] = paste('null',aa[,1]); aa[,2] = paste('null',aa[,2])
    
    word2 = strsplit (aa,' ')[1 : len]
    word2=sapply(1:len,function(x)word2[[x]][word2[[x]] != ""])

    word3 = strsplit (aa,' ')[(len + 1) : (2 * len)]
    word3=sapply(1:len,function(x)word3[[x]][word3[[x]] != ""])
    
    lf = vapply (word2, length, FUN.VALUE = 0)
    le = vapply (word3, length, FUN.VALUE = 0)
    
    #---- position of matrix f to e (rows = the source language(e), columns = The target language(f))----
    
    fe = sapply (1 : len, function (x) (2 : lf[x]) * (le[x] + 2) + (fe1[[x]] + 2))  #column's position in added matrix (2 rows and 2 columns are added in the marginal of initial matrix)
    
    #---- position of matrix e to f (rows=the target language(e),columns=The source language(f))----
    
    ef = sapply (1 : len, function (x) (2 : le[x]) * (lf[x] + 2) + (ef1[[x]] + 2)) #row's position in added matrix (2 rows and 2 columns are added in the marginal of initial matrix)
    ef = sapply (1 : len, function (x) (ef [[x]] - (ef1 [[x]] + 2)) / (lf [x] + 2)  + (ef1 [[x]] + 1) * (le [x] + 2) + 1) #  computing column's position using row's positions
    
    #----------------------------------------------------------------
    #          Union Word Alignment without null
    #----------------------------------------------------------------
    if (method == 'union')
    {
        union = sapply (1 : len, function (x) unique (c (ef [[x]], fe [[x]])))
        pos_col = sapply (1 : len, function (x) floor (union [[x]] / (le [x] + 2))) # column's number related to the source language in the matrix
        pos_row = sapply (1 : len, function (x) union [[x]] - pos_col [[x]] * (le[x] + 2) - 1) # row's number related to the target language in the matrix
        
        align_un = sapply(1 : len, function(x) paste (word3 [[x]][pos_row[[x]]], word2 [[x]][pos_col[[x]]], sep = ' '))
        
        date2 = as.POSIXlt(Sys.time(), "Iran")
        
        mylist = list(time = date2 - date1, method = method, alignment = align_un, aa = sapply(1:len,function(x)paste(word2[[x]],sep='',collapse=' ')))
        
        save(mylist,file = paste('symmetric', method, nrec, iter, 'RData', sep = '.'))
        cat(paste(getwd(), '/', 'symmetric', '.', method, '.', nrec, '.', iter, '.RData',' created','\n',sep=''))
        
        attr(mylist, "class") <- "symmet"
        
        return (mylist)
    }
    #----------------------------------------------------------------
    #         Intersection Word Alignment without null
    #----------------------------------------------------------------
    
    if (method == 'intersection')
    {
        intersection = sapply (1 : len, function(x)fe [[x]][fe [[x]] %in% ef[[x]]])
        
        pos_col = sapply (1 : len, function (x) floor (intersection [[x]] / (le [x] + 2))) # column's number related to the source language in the matrix
        pos_row = sapply (1 : len, function (x) intersection [[x]] - pos_col [[x]] * (le[x] + 2) - 1) # row's number related to the target language in the matrix
        
        align_in = sapply(1 : len, function(x) paste ( word3 [[x]][pos_row[[x]]], word2 [[x]][pos_col[[x]]], sep = ' '))
        
        date2 = as.POSIXlt(Sys.time(), "Iran")
        
        mylist = list(time = date2 - date1, method = method, alignment = align_in, aa = sapply(1:len,function(x)paste(word2[[x]],sep='',collapse=' ')))
        
        save(mylist,file = paste('symmetric', method, nrec, iter, 'RData', sep = '.'))
        cat(paste(getwd(), '/', 'symmetric', '.', method, '.', nrec, '.', iter, '.RData',' created','\n',sep=''))
        
        attr(mylist, "class") <- "symmet"
        return(mylist)
    }
    #----------------------------------------------------------------
    #          GROW-DIAG Word Alignment without null
    #----------------------------------------------------------------
    if(method=='grow-diag')
    {
        g_d = sapply (1 : len, function(x) squareN (fe [[x]],ef [[x]],(le [x] + 2)))
        
        pos_col = sapply (1 : len, function (x) floor (g_d [[x]] / (le [x] + 2))) # column's number related to the source language in the matrix
        pos_row = sapply (1 : len, function (x) g_d [[x]] - pos_col [[x]] * (le[x] + 2) - 1) # row's number related to the target language in the matrix
        
        symmet = sapply(1 : len, function(x) paste ( word3 [[x]][pos_row[[x]]], word2 [[x]][pos_col[[x]]], sep = ':'))
        
        date2 = as.POSIXlt(Sys.time(), "Iran")
        
        mylist = list(time = date2 - date1, method = method, alignment = symmet, aa = sapply(1:len,function(x)paste(word2[[x]],sep='',collapse=' ')))
        
        save(mylist,file = paste('symmetric', method, nrec, iter, 'RData', sep = '.'))
        cat(paste(getwd(), '/', 'symmetric', '.', method, '.', nrec, '.', iter, '.RData',' created','\n',sep=''))

        attr(mylist, "class") <- "symmet"
        return(mylist)
    }
}



print.symmet <-
function(x, ...) 
{
cat("\n")
print(x $ time)
cat("Symmetrization method is", x[[2]], "\n")
cat("Symmetric word alignment for some sentence pairs are", "\n")
sapply(1:3,function(i){cat(paste(i,x $ aa[i],sep=': '),'\n');
print(noquote(x $ alignment[[i]]))})
cat("            ", ".", "\n")
cat("            ", ".", "\n")
cat("            ", ".", "\n")
sapply((length(x $ alignment) - 2) : length(x $ alignment),
function(i){cat(paste(i,x $ aa[i],sep=': '),'\n');
print(noquote(x $ alignment[[i]]))})
}
