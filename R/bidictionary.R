bidictionary <-
function (..., n = -1L, iter = 15, prob = 0.8, dtfile.path = NULL, name.sorc = 'f', name.trgt = 'e')
{
    date1 = as.POSIXlt (Sys.time(), 'Iran')
    
    e = f = c()
    if(is.null(dtfile.path))
    {
        yn = 'yn'
        while(yn != 'y' & yn != 'n')
        yn = readline('Are you sure that you want to run the align.ibm1 function (It takes time)? (y/ n: if you want to specify word alignment path, please press "n".)')
        if (yn == 'y') {
            dd1 = align.ibm1 (...,n = n, iter = iter, input = TRUE)
            save(dd1,iter, file = paste(name.sorc, name.trgt, n, iter, 'RData',sep='.'))
            cat(paste(getwd(), '/', name.sorc,'.', name.trgt,'.', n, '.', iter, '.RData',' created','\n', sep=''))
        }else{
            return(cat('Error: No such file or directory in dtfile.path.', '\n'))
      }
    }
    if(! is.null(dtfile.path))
    if (file.exists(dtfile.path)) {
       load(dtfile.path)
    }else{
        return(cat('Error: No such file or directory in dtfile.path.', '\n'))
    }
    
    u1 = unique (dd1 [round (t, 1) > prob, f, e])
    fe = matrix (c (u1$f, u1$e), ncol = 2)
    fe = fe [order (fe [,1]),]
    fe = apply(fe,1,paste,collapse=':')
    
    date2 = as.POSIXlt (Sys.time(), 'Iran')
    ##################################################################
    mylist = list (time = date2 - date1, number_input = n, Value_prob = prob, iterIBM1 = iter, Source_Language = name.sorc, Target_Language = name.trgt, dictionary = fe)
    ##################################################################
    return (mylist)
}
