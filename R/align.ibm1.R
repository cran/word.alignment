align.ibm1 <-
function(..., iter = 5, dtfile.path = NULL, name.sorc = 'f',name.trgt = 'e', result.file = 'result', input = FALSE)
{
    date1 = as.POSIXlt (Sys.time(), "Iran")
    a = b = count0 = count = total = i = j = e = f = g = c ()
    
    #-----------------------Translation:f to e ----------------------
    aa = prepare.data (...)
    n1 = aa[[1]]
    
    aa = cbind(paste('null', aa[[2]][,1]), aa[[2]][,2])
    
    len = nrow(aa)
    
    if(is.null(dtfile.path))
    {
        b = apply (aa, 1, function (x) {Vt1 = strsplit (as.character (x [1]), ' ') [[1]]; Vt2 = strsplit (as.character (x[2]), ' ') [[1]];
            Vt1 = Vt1 [Vt1 != '']; Vt2 = Vt2 [Vt2 != '']; cbind (Var1 = rep.int (Vt1, length (Vt2)), Var2 = rep (Vt2, each = length (Vt1)))})
        
        cc = vapply (b,length,FUN.VALUE=0)/2
        
        #-------------------------- main code ---------------------------
        dd1 = data.table (g = rep (1 : len, cc), f = unlist (sapply (b, function (x) x [,1])), e = unlist (sapply (b, function (x) x [,2])), t = as.numeric (rep (1 / cc, cc)))
        
        rm (b, cc)
        gc ()
        
        iteration = 0
        for (iiiii in 1 : iter)
        {
            iteration = iteration + 1
            dd1 [, count0 := t / sum(t), by = paste (g, e)]
            dd1 [, t := NULL]
            dd1 [, count := sum (count0), by = paste (e, f)]
            dd1 [, total := sum (count0), by = f]
            dd1 [, t := count/total]
            dd1 [, count0 := NULL]
            dd1 [, count := NULL]
            dd1 [, total := NULL]
        }
        save (dd1,iteration, file = paste(name.sorc, name.trgt, n1, iter, 'RData', sep = '.'))
        
        if (input) return (dd1)
        
        cat(paste(getwd(), '/', name.sorc,'.', name.trgt,'.', n1, '.', iter, '.RData',' created', '\n', sep=''))
    }
    # ------- Using saved file  ----
    if(! is.null(dtfile.path))
    if (file.exists(dtfile.path)){
        load(dtfile.path)
        if (input) return (dd1)
    }
    else{cat("Error: No such file or directory in dtfile.path.")}
    
    #--------------------- Best alignment --------------------------
    word = strsplit(aa,' ')
    
    word2 = word [1 : len]
    word2=sapply(1:len,function(x)word2[[x]][word2[[x]] != ""])

    word3 = word [(len+1):(2*len)]
    word3=sapply(1:len,function(x)word3[[x]][word3[[x]] != ""])

    
    lf = vapply(word2 ,length,FUN.VALUE=0)
    le = vapply(word3 ,length,FUN.VALUE=0)
    
    dd1 [, i := unlist (sapply (1 : len, function (x) rep (0 : (lf [x]-1), le [x])))]
    dd1 [, j := unlist (sapply (1 : len, function (x) rep (1 : (le[x]), each = lf [x])))]
    
    d1 = dd1 [, i [ which.max (t)], by = list (g, j)] [[3]]
    
    c1 = c (0, cumsum (le))
    
    ef_word = sapply (1 : len, function (x) paste (word3 [[x]], word2[[x]] [d1 [ (c1 [x] + 1) : c1 [x + 1]] + 1], sep = ' '))
    
    ef_init = sapply (1 : 3, function (x) paste (word3 [[x]], word2[[x]] [d1 [ (c1 [x] + 1) : c1 [x + 1]] + 1], sep = ' --> '))
    ef_end = sapply ((len - 2) : len, function (x) paste (word3 [[x]], word2[[x]] [d1 [ (c1 [x] + 1) : c1 [x + 1]] + 1], sep = ' --> '))

    ef_number = sapply (1 : len, function (x) d1 [ (c1 [x] + 1) : c1 [x + 1]])
    
    #------------- Expected Length of both languages----------------
    
    ex1 = mean (lf) - 1
    ex2 = mean (le)
    
    #------------- Vocabulary size of both languages----------------
    
    v.s1 = length (unique (unlist (word2)))
    v.s2 = length (unique (unlist (word3)))
    
    #----------------- Word Translation Probability ----------------
    dd2 = unique (dd1 [, t, by = list (e,f)])
    
    date2=as.POSIXlt(Sys.time(), "Iran")
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    mylist = list (model = 'IBM1', initial_n = n1, used_n = len, time = date2 - date1,
    iterIBM1 = iteration, expended_l_source = ex1, expended_l_target = ex2, VocabularySize_source = v.s1,
    VocabularySize_target = v.s2, word_translation_prob = dd2, word_align = ef_word, align_init = ef_init,
    align_end = ef_end, number_align = ef_number, 
    aa = sapply(1:len,function(x)paste(word2[[x]],sep='',collapse=' ')))
    
    save(mylist,file = paste(result.file, name.sorc, name.trgt, n1, iter, 'RData', sep = '.'))
    
    cat(result.file, '.', name.sorc, '.', name.trgt, '.', n1, '.', iter,'.RData',' created','\n',sep='')
    
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#
    attr(mylist, "class") <- "align"
    return (mylist)
}

###
align.symmet <-
function (file.sorc, file.trgt, n = -1L, iter = 4, method = c ('union', 'intersection', 'grow-diag'), encode.sorc = 'unknown', encode.trgt = 'unknown', name.sorc = 'f', name.trgt = 'e', ...)
{
    date1 = as.POSIXlt (Sys.time(), 'Iran')
    
    method = match.arg (method)
    
    ef1 = align.ibm1 (file.sorc, file.trgt, n = n, iter = iter, encode.sorc = encode.sorc, encode.trgt = encode.trgt, name.sorc = name.sorc, name.trgt = name.trgt, ... ) $ number_align
    
    fe1 = align.ibm1 (file.trgt, file.sorc, n = n, iter = iter, encode.sorc = encode.trgt, encode.trgt = encode.sorc, name.sorc = name.trgt, name.trgt = name.sorc, ... ) $ number_align
    len = length (fe1)
    
    aa = prepare.data (file.sorc, file.trgt, n = n, encode.sorc = encode.sorc, encode.trgt = encode.trgt, ...)
    
    aa = aa[[2]]
    
    aa[,1] = paste('null',aa[,1]); aa[,2] = paste('null',aa[,2])
    
    word = strsplit(aa,' ')
    
    word2 = word [1 : len]
    word2=sapply(1:len,function(x)word2[[x]][word2[[x]] != ""])

    
    word3 = word [(len+1):(2*len)]
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
        
	  align_un_int = sapply(1 : len, function(x) paste (pos_row[[x]], pos_col[[x]], sep = ' '))
        align_un = sapply(1 : len, function(x) paste (word3 [[x]][pos_row[[x]]], word2 [[x]][pos_col[[x]]], sep = ' '))
        
        align_init = sapply(1 : 3, function(x) paste (word3 [[x]][pos_row[[x]]], word2 [[x]][pos_col[[x]]], sep = ' --> '))
        align_end = sapply((len - 2) : len, function(x) paste (word3 [[x]][pos_row[[x]]], word2 [[x]][pos_col[[x]]], sep = ' --> '))



        date2 = as.POSIXlt(Sys.time(), "Iran")
        
        mylist = list(time = date2 - date1, model = paste('symmetric',method), initial_n = n, used_n = len, iterIBM1 = iter,
                      word_align = align_un, align_init = align_init, align_end = align_end,
                      align_un_int = align_un_int, aa = sapply(1:len,function(x)paste(word2[[x]],sep='',collapse=' ')))
        
        save(mylist,file = paste('result', method, n, iter, 'RData', sep = '.'))
        cat(paste(getwd(), '/', 'result', '.', method, '.', n, '.', iter, '.RData',' created','\n',sep=''))
        
        attr(mylist, "class") <- "align"
        
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
        
        align_in_int = sapply(1 : len, function(x) paste (pos_row[[x]], pos_col[[x]], sep = ' '))

        align_in = sapply(1 : len, function(x) paste ( word3 [[x]][pos_row[[x]]], word2 [[x]][pos_col[[x]]], sep = ' '))
        
        align_init = sapply(1 : 3, function(x) paste (word3 [[x]][pos_row[[x]]], word2 [[x]][pos_col[[x]]], sep = ' --> '))
        align_end = sapply((len - 2) : len, function(x) paste (word3 [[x]][pos_row[[x]]], word2 [[x]][pos_col[[x]]], sep = ' --> '))


        date2 = as.POSIXlt(Sys.time(), "Iran")
        
        mylist = list(time = date2 - date1, model = paste('symmetric',method), initial_n = n, used_n = len, iterIBM1 = iter,
                      word_align = align_in, align_init = align_init, align_end = align_end,
                      align_in_int = align_in_int, aa = sapply(1:len,function(x)paste(word2[[x]],sep='',collapse=' ')))

        
        save(mylist,file = paste('result', method, n, iter, 'RData', sep = '.'))
        cat(paste(getwd(), '/', 'result', '.', method, '.', n, '.', iter, '.RData',' created','\n',sep=''))
        
        attr(mylist, "class") <- "align"
        return(mylist)
    }
    #----------------------------------------------------------------
    #          GROW-DIAG Word Alignment without null
    #----------------------------------------------------------------
    if(method=='grow-diag')
    {
        g_d = sapply (1 : len, function(x) neighbor (fe [[x]],ef [[x]],(le [x] + 2)))
        
        pos_col = sapply (1 : len, function (x) floor (g_d [[x]] / (le [x] + 2))) # column's number related to the source language in the matrix
        pos_row = sapply (1 : len, function (x) g_d [[x]] - pos_col [[x]] * (le[x] + 2) - 1) # row's number related to the target language in the matrix
        
        align_gd_int = sapply(1 : len, function(x) paste (pos_row[[x]], pos_col[[x]], sep = ' '))

        symmet = sapply(1 : len, function(x) paste ( word3 [[x]][pos_row[[x]]], word2 [[x]][pos_col[[x]]], sep = ' '))
        
	  align_init = sapply(1 : 3, function(x) paste (word3 [[x]][pos_row[[x]]], word2 [[x]][pos_col[[x]]], sep = ' --> '))
        align_end = sapply((len - 2) : len, function(x) paste (word3 [[x]][pos_row[[x]]], word2 [[x]][pos_col[[x]]], sep = ' --> '))

        date2 = as.POSIXlt(Sys.time(), "Iran")
        
         mylist = list(time = date2 - date1, model = paste('symmetric',method), initial_n = n, used_n = len, iterIBM1 = iter,
                      word_align = symmet, align_init = align_init, align_end = align_end,
                      align_gd_int = align_gd_int, aa = sapply(1:len,function(x)paste(word2[[x]],sep='',collapse=' ')))

        
        save(mylist,file = paste('result', method, n, iter, 'RData', sep = '.'))
        cat(paste(getwd(), '/', 'result', '.', method, '.', n, '.', iter, '.RData',' created','\n',sep=''))

        attr(mylist, "class") <- "align"
        return(mylist)
    }
}

###
print.align <-
function(x, ...) 
{
print(x $ time)
cat("The model is",x$model , "\n")
cat("The number of input sentence pairs is", x $ initial_n, "\n")
cat("The number of used sentence pairs is", x $ used_n, "\n")
cat("The number of iterations for EM algorithm is", x$iterIBM1, "\n")
cat("Word alignment for some sentence pairs are", "\n")
sapply(1:3,function(i){cat(paste(i,x $ aa[i],sep=': '),'\n'); 
print(noquote(x $ align_init[[i]]))})
cat("            ", ".", "\n")
cat("            ", ".", "\n")
cat("            ", ".", "\n")
sapply((length(x $ word_align) - 2) : length(x $ word_align),
function(i){cat(paste(i,x $ aa[i],sep=': '),'\n');
print(noquote(x $ align_end[[i - (length(x $ word_align)- 3) ]]))})
}







