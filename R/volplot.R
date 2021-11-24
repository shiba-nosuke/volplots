#' Calculating the Number of Business Days 
#' 
#' @param FC value of log2(fold change)
#' @param pvalue p-value
#' @param t_FC Threshold of log2(fold change) 
#' @param t_pvalue Threshold of p-value
#' @param mainname title of graph
#' @param xlabel label of x-axis
#' @param ylabel label of y-axis
#' @param upcol color of up-regulated genes (genes >= t-FC)
#' @param downcol color of down-regulated genes (genes <= t-FC)
#' @param tline line of threshold (If not necessary = F)
#' 
#' @export 
#'
volplot <- function(FC, pvalue, t_FC = 1, t_pvalue=0.05, mainname='Volcano plot', xlabel='log2(Fold Change)', ylabel='-log10(p-value)', upcol='red', downcol='blue', tline=T){
  logp <- -log10(pvalue)
  tpval <- -log10(t_pvalue)
  plot(FC, logp, pch = 20, cex = 0.5, main = mainname, xlab = xlabel, ylab = ylabel, col = ifelse(logp <= tpval,'black',ifelse(FC >= t_FC, upcol, ifelse(FC <= -t_FC,downcol,'black')))) 
  if(tline==T){
    abline(v = t_FC, col= 'darkgreen', lty=2) 
    abline(v = -t_FC, col= 'darkgreen', lty=2) 
    abline(h = tpval, col= 'darkgreen', lty=2)
  }else if(tline==F){
  }
}