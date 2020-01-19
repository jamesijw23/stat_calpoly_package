#' Graph Normal Distributions
#' @param radian @name normal_plots_check @description provides an easy way to view if the data is normal
#' @return value
#' @export



normal_plots_check <- function (df,variable,type_of_plot = "d",color) {
  
  vec = dplyr::pull(df,variable)
  
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  d <- data.frame(resids = vec)
  
  
  ## Plot Density
  p1 <- ggplot2::ggplot(d, ggplot2::aes(x=resids)) +
    ggplot2::geom_density(fill = color) +
    ggplot2::ggtitle(paste0("Density of ",variable, sep="")) +
    ggplot2::xlim(mean(vec)-3.5*sd(vec),mean(vec)+3.5*sd(vec)) +
    ggplot2::xlab(paste0(variable," Data")) +
    ggplot2::ylab("Probability") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size = 8, face = "bold"))
  
  
  ## Plot QQ
  p2 <- ggplot2::ggplot(d, ggplot2::aes(sample = resids)) +
    ggplot2::stat_qq(color = color) +
    ggplot2::ggtitle(paste0("QQ-Plot of ",variable," vs. Theorectical", sep="")) +
    ggplot2::xlab("Theoretical") +
    ggplot2::ylab(variable) +
    ggplot2::geom_abline(slope = slope, intercept = int) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size = 8, face = "bold"))
  
  
  if(type_of_plot == "d"){
    p1  
  } else if(type_of_plot == "q"){
    p2
  }
  
}





## OLD VERSION 1
# normal_plots_check <- function (vec,data_name,measure_name,color_desired) {
#   # following four lines from base R's qqline()
#   y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
#   x <- qnorm(c(0.25, 0.75))
#   slope <- diff(y)/diff(x)
#   int <- y[1L] - slope * x[1L]
# 
#   d <- data.frame(resids = vec)
# 
# 
#   ## Plot QQ
#   p1 <- ggplot2::ggplot(d, ggplot2::aes(sample = resids)) +
#     ggplot2::stat_qq(color = color_desired) +
#     ggplot2::ggtitle(paste0("QQ-Plot of ",data_name," vs. Theorectical", sep="")) +
#     ggplot2::xlab("Theoretical") +
#     ggplot2::ylab(measure_name) +
#     ggplot2::geom_abline(slope = slope, intercept = int) +
#     ggplot2::theme_bw() +
#     ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size = 8, face = "bold"))
# 
#   ## Plot Density
#   p2 <- ggplot2::ggplot(d, ggplot2::aes(x=resids)) +
#     ggplot2::geom_density(fill = color_desired) +
#     ggplot2::ggtitle(paste0("Density of ",data_name, sep="")) +
#     ggplot2::xlim(mean(vec)-3.5*sd(vec),mean(vec)+3.5*sd(vec)) +
#     ggplot2::xlab(paste0(data_name,"Data")) +
#     ggplot2::ylab("Probability") +
#     ggplot2::theme_bw() +
#     ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size = 8, face = "bold"))
# 
#   gridExtra::grid.arrange(p1, p2, ncol=2)
# }

