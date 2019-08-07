#'Print ANOVA output with effect size confidence intervals
#'
#' @param aov_table 
#'
#' @return
#' @export
#'
#' @examples
print_apa_ci <- function(aov_table){
    print_eta_ci <- function(Fval, conf = .90, df1, df2){
        limits <- apaTables::get.ci.partial.eta.squared(F.value=Fval, df1=df1, df2=df2, conf.level=conf)
        return(paste0(", 90\\% CI $[",round(limits$LL, 2),"$, $",round(limits$UL, 2),"]$"))
    }
    
    pap <- apa_print(aov_table, es = "pes", correction = "none")$full_result
    
    for(i in 1:length(pap)){
        pap[i] <- paste0(pap[i], print_eta_ci(Fval = aov_table$anova_table$`F`[i], df1 = aov_table$anova_table$`num Df`[i], df2 = aov_table$anova_table$`den Df`[i]))
        pap[i] <- gsub("p = .000", "p < .001", pap[i])
        pap[i] <- gsub(") = 0.00", ") < 0.01", pap[i])
    }
    return(pap)
}