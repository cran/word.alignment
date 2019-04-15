align.test<-
function(file.sorc, file.trgt, test.sorc, test.trgt, n.train = -1, n.test = -1, minlen.train = 5, maxlen.train = 40, minlen.test = 5, maxlen.test = 40, null.tokens = TRUE, dtfile.path = NULL, file.align = 'alignment',name.sorc='f',name.trgt='e',iter = 3, ...)
{
g = fe = f = e = ge = c()   
   #------- constructing a data.table using align.ibm1 function for the first time
    if(is.null(dtfile.path))
    {
        yn = 'yn'
        while(yn != 'y' & yn != 'n')
        yn = readline('Are you sure that you want to run the align.ibm1 function (It takes time)? (y/ n: if you want to specify word alignment path, please press "n".)')
        if (yn == 'y') {
            dd1 = align.ibm1(file.sorc, file.trgt, n = n.train,  min.len = minlen.train, max.len = maxlen.train, input = TRUE,...)
        }else{
            return("Error: No such file or directory in dtfile_path.")
        }
    }
    # ------- reading an already built data.table using align.ibm1 function ----
    if(! is.null(dtfile.path))
    if (file.exists(dtfile.path)){
        load(dtfile.path)
    }else{cat('Error: No such file or directory in dtfile.path.')}
    # -----------------
    aa = prepare.data (test.sorc, test.trgt, n = n.test,  min.len = minlen.test, max.len = maxlen.test, ...)
    
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
    save(dd, dd4,file = paste(file.align, name.sorc, name.trgt, n.train, iter,'RData',sep='.'))
    
    cat(file.align, '.', name.sorc, '.', name.trgt, '.', n.train, '.', iter, '.RData',' created','\n',sep='')
}

