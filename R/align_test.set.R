align_test.set<-
function(file_train1, file_train2, tst.set_sorc, tst.set_trgt, nrec = -1, nlen = -1,  encode.sorc = 'unknown', encode.trgt = 'unknown', minlen1 = 5, maxlen1 = 40, minlen2 = 5, maxlen2 = 40, removePt = TRUE, all = FALSE, null.tokens = TRUE, iter = 3, f1 = 'fa', e1 = 'en', dtfile_path = NULL, file_align = 'alignment')
{
g = fe = f = e = ge = c()   
   #------- constructing a data.table using word_alignIBM1 function for the first time
    if(is.null(dtfile_path))
    {
        n = 'yn'
        while(n != 'y' & n != 'n')
        n = readline('Are you sure that you want to run the word_alignIBM1 function (It takes time)? (y/ n: if you want to specify word alignment path, please press "n".)')
        if (n == 'y') {
            dd1 = word_alignIBM1(file_train1,file_train2, nrec = nrec, encode.sorc = encode.sorc, encode.trgt = encode.trgt, iter = iter, minlen = minlen1, maxlen = maxlen1, input = TRUE, removePt = removePt, all = all)
            save (dd1,file = paste(f1, e1, nrec, iter, 'RData', sep = '.'))
            cat(paste(getwd(), '/', f1,'.', e1,'.', nrec, '.', iter, '.RData',' created', '\n', sep=''))
        }else{
            return("Error: No such file or directory in dtfile_path.")
        }
    }
    # ------- reading an already built data.table using word_alignIBM1 function ----
    if(! is.null(dtfile_path))
    if (file.exists(dtfile_path)){
        load(dtfile_path)
    }else{cat('Error: No such file or directory in dtfile_path.')}
    # -----------------
    aa = prepareData (tst.set_sorc, tst.set_trgt, nrec = nlen, encode.sorc = encode.sorc, encode.trgt = encode.trgt, minlen = minlen2, maxlen = maxlen2, removePt = removePt, all = all, word_align = TRUE)
    
    aa = aa[[2]]
    
    if (null.tokens) aa = cbind(paste('null',aa[,1]),aa[,2])
    
    len = nrow(aa)
    
    b = apply (aa, 1, function (x) {Vt1 = strsplit (as.character (x [1]), ' ') [[1]]; Vt2 = strsplit (as.character (x[2]), ' ') [[1]];
        Vt1 = Vt1 [Vt1 != '']; Vt2 = Vt2 [Vt2 != '']; cbind (Var1 = rep.int (Vt1, length (Vt2)), Var2 = rep (Vt2, each = length (Vt1)))})
    
    cc = vapply (b,length,FUN.VALUE=0)/2
    
    dd2 = data.table (g = rep (1 : len, cc), f = unlist (sapply (b, function (x) x [,1])), e = unlist (sapply (b, function (x) x [,2])))
    
    dd1[, g := NULL]
    dd1 = unique(dd1)
    
    dd1[,fe := paste(f,e)]
    dd1[,f := NULL]
    dd1[,e := NULL]
    
    dd2[,fe := paste(f,e)]
    
    dd1 = merge(dd1, dd2, by = 'fe', allow.cartesian = TRUE)
    dd1[, fe := NULL]
    dd2[, fe := NULL]
    
    dd4 = cbind(dd1[,g[which.max(t)],by = paste(g,e)],
    dd1[,f[which.max(t)],by = paste(g,e)][[2]],
    dd1[,e[which.max(t)],by = paste(g,e)][[2]])
    
    setnames(dd4,c('ge','g','f','e'))
    
    dd4[, ge := NULL]
    
    if (null.tokens) {
        dd = 'null'
    } else {
        dd = 'nolink'
    }
    save(dd, dd4,file = paste(file_align, f1, e1, nrec, iter,'RData',sep='.'))
    
    cat(file_align, '.', f1, '.', e1, '.', nrec, '.', iter, '.RData',' created','\n',sep='')
}
