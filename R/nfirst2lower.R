nfirst2lower <-
function(x, n = 1, first = TRUE, second = FALSE)
{
if (first)  x = paste (tolower (substr (x, 1, n)), substring (x, n+1), sep = '')
       
if (second) x [substring (x, 2, 2) %in% LETTERS] = tolower (x [substring (x, 2, 2) %in% LETTERS])
      
return (x)
}
