Evaluation1 <-
function(file_gold = "gold.RData", agn = c("my.agn","an.agn"), file_align = "alignment.-1.3.RData", alpha = 0.3)
{
    date1 = as.POSIXlt (Sys.time(), "Iran")
    e = f = g = fg1= fg = SP = dd = gfe = A = recall0 = n_AS = n_S = precision0 = n_AP = n_A = AER0 = c()
    
    agn = match.arg(agn)
    #------------------------Constructing a gold standard --------------------------
    readline(paste('Please ensure that you create the file(s) by cons.agn function in this package and you fill the "',  file_align, '" file by "1|2" for sure|possible. Then press "Enter" to continue.', sep=''))
    readline(paste('If you have an excel file, please convert it into required R format using ExcelToR function available in this package.\nThen press "Enter" to continue.', sep = ''))
    
    load(file = file_gold)
    null1 = fg1
    
    len = length(fg)

    sum1 = sapply(1 : len, function(x)sum(as.numeric(fg[[x]][-1,-1])))
    if( sum(sum1 == 0)!= 0) paste('Warning: sentence(s)', paste(which(sum1==0), collapse=','), 'has (have) not been aligned.')
    
    dd2 = c()
    for (sn in 1 : len)
    {
        dd3 = data.table(cbind( g = sn, expand.grid(f = fg[[sn]][-1,1], e = fg[[sn]][1,-1]), SP = c(fg[[sn]][-1,-1])))
        dd2 = rbind(dd2, dd3)
        dd2 $ SP[is.na(dd2 $ SP)] = 0
    }
    dd3 = dd2[,sum(SP == 1),by = g]
    dd2 = merge(dd3, dd2, by = 'g')
    setnames(dd2,c('g','n_S','f','e','SP'))
    
    rm(dd3)
    gc()
    #----- computing word alignment based on my IBM model1 using word_alignIBM1 function -----
    if(agn == 'my.agn')
    {
        load(file = file_align)
        
        if (dd != null1) {
            return(paste("Error: gold standard alignment and word alignment must be the same. But, the gold is including " , null1, " and the alignment is containing ", dd, ".", sep=''))
        } else {
            dd4[, gfe := paste(g,f,e)]
            dd2[, gfe := paste(g,f,e)]
            dd4[, g := NULL]
            dd4[, f := NULL]
            dd4[, e := NULL]
            dd4 = unique( merge(dd4, dd2, by = 'gfe'))
            dd4[, gfe := NULL]
        }
    }
    #-------------- reading computed word alignment using another software ---------------
    if (agn == 'an.agn')
    {
        readline(paste('Please ensure that you create the file(s) by cons.agn function in this package and you fill the",  file_align, "file by "1|2" for sure|possible. Then press "Enter" to continue.\nIf you have an excel file, please convert it into required R format using ExcelToR function available in this package. Then press "Enter" to continue.', sep = ''))
        
        load(file = file_align)
        
        if (fg1 != null1){
            return(paste("Error: gold standard alignment and word alignment must be the same. But, the gold is including " , null1, " and the alignment is containing ", dd, ".", sep=''))
        } else {
            dd1 = c()
            for (sn in 1 : len)
            {
                dd3 = data.table(cbind( g = sn, expand.grid(f = fg[[sn]][-1,1], e = fg[[sn]][1,-1]), A = c(fg[[sn]][-1,-1])))
                dd1 = rbind(dd1, dd3)
                dd1 $ A[is.na(dd1 $ A)] = 0
            }
            
            dd1 = cbind(dd2, A = dd1 $ A)
            dd4 = dd1[ A == 1]
            dd4[, A := NULL]
        }
    }
    ##################
    
    dd4[ , `:=`( n_A = .N  ) , by = g ]
    
    dd5 = sapply(1 : 2,function(x)dd4[,sum(SP == x),by = g])
    
    dd4 = merge(dd4,dd5[,1],by = 'g')
    dd4 = merge(dd4,dd5[,2],by = 'g')
    
    rm(dd5)
    gc()
    
    setnames(dd4,c('g','n_S','f','e', 'SP', 'n_A','n_AS','n_AP'))
    
    dd4 = unique(dd4[,by = g])
    
    dd4[,recall0 := as.numeric(n_AS) / as.numeric(n_S)]
    dd4 $ recall0[is.nan(dd4 $ recall0)] = 1
    dd4 $ recall0[is.infinite(dd4 $ recall0)] = 0
    
    dd4[,precision0 := as.numeric(n_AP) / as.numeric(n_A)]
    dd4 $ precision0[is.nan(dd4 $ precision0)] = 1
    dd4 $ precision0[is.infinite(dd4 $ precision0)] = 0
    
    dd4[,AER0 := (as.numeric(n_AP) + as.numeric(n_AS)) / (as.numeric(n_A) + as.numeric(n_S))]
    dd4 $ AER0[is.nan(dd4 $ AER0)] = 1
    dd4 $ AER0[is.infinite(dd4 $ AER0)] = 0
    
    dd4[,precisionS := as.numeric(n_AS)/as.numeric(n_A)]
    dd4 $ precisionS[is.nan(dd4 $ precisionS)] = 1
    dd4 $ precisionS[is.infinite(dd4 $ precisionS)] = 0
    
    #---------- recall, precision and accuracy measures-----------
    recall = dd4[,mean(recall0)]
    precision = dd4[,mean(precision0)]
    AER = dd4[,mean(AER0)]
    F_measure.PS = 1 / (alpha / precision + (1 - alpha)/recall)
    precisionS = dd4[,mean(precisionS)]
    F_measure.S = 1 / (alpha / precisionS + ( 1- alpha)/recall)
    
    date2 = as.POSIXlt (Sys.time(), "Iran")
    #############################################################
    list2 = list(time = date2 - date1, Recall = recall, Precision = precision, AER = AER, F_measure.PS = F_measure.PS, F_measure.S = F_measure.S)
    #############################################################
    return(list2)
}