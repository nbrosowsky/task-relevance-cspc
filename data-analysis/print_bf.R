#' Print Bayes Factor results with error percentage
#'
#' @param bf 
#'
#' @return
#' @export
#'
#' @examples
print_bf <- function(bf){
    result <- list()
    for (i in 1:length(bf)){
        if(extractBF(bf[i])$bf < 1){
            result[i] <- paste0("$\\mathrm{BF}_{\\textrm{01}} = ", round(extractBF(1/bf[i])$bf, digits = 2),"$ $[\\pm ", round(extractBF(bf[i])$error*100,digits=2), "\\%]$")
        } else {
            result[i] <- paste0("$\\mathrm{BF}_{\\textrm{10}} = ", round(extractBF(bf[i])$bf, digits = 2),"$ $[\\pm ", round(extractBF(bf[i])$error*100,digits=2), "\\%]$")
            
        }
    }
    
    return(result)
}
