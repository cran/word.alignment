mydictionary <-
function (file_train1,file_train2, nrec = -1, encode.sorc = 'unknown', encode.trgt = 'unknown', iter = 15, prob = 0.8, minlen=5, maxlen = 40, lang1 = 'Farsi', lang2 = 'English', removePt = TRUE, dtfile_path = NULL, f1 = 'fa', e1 = 'en', result_file='mydictionaryResults')
{
    date1 = as.POSIXlt (Sys.time(), 'Iran')
    
    e = f = c()
    if(is.null(dtfile_path))
    {
        n = 'yn'
        while(n != 'y' & n != 'n')
        n = readline('Are you sure that you want to run the word_alignIBM1 function (It takes time)? (y/ n: if you want to specify word alignment path, please press "n".)')
        if (n == 'y') {
            dd1 = word_alignIBM1 (file_train1, file_train2, nrec = nrec, encode.sorc = encode.sorc, encode.trgt = encode.trgt, minlen = minlen, maxlen = maxlen, iter = iter, input = TRUE, removePt = removePt)
            save(dd1,iter, file = paste(f1, e1, nrec, iter, 'RData',sep='.'))
            cat(paste(getwd(), '/', f1,'.', e1,'.', nrec, '.', iter, '.RData',' created','\n', sep=''))
        }else{
            return(cat('Error: No such file or directory in dtfile_path.', '\n'))
        }
    }
    if(! is.null(dtfile_path))
    if (file.exists(dtfile_path)) {
        load(dtfile_path)
    }else{
        return(cat('Error: No such file or directory in dtfile_path.', '\n'))
    }
    
    u1 = unique (dd1 [round (t, 1) > prob, f, e])
    fe = matrix (c (u1$f, u1$e), ncol = 2)
    fe = fe [order (fe [,1]),]
    fe = apply(fe,1,paste,collapse=':')
    
    date2 = as.POSIXlt (Sys.time(), 'Iran')
    ##################################################################
    mylist = list (time = date2 - date1, number_input = nrec, Value_prob = prob, iterIBM1 = iter, Source_Language = lang1, Target_Language = lang2, dictionary = fe)
    ##################################################################
    return (mylist)
}
