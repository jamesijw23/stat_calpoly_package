#' Graph box_plot
#' @param radian
#' @return value
#' @export


usethis::use_package("ggplot2")
usethis::use_package("gridExtra")
usethis::use_package("dplyr")



box_plot = function(df,xvar,yvar,main_title){
  df[, xvar] <- as.factor(df[, xvar])
  ggplot2::ggplot(df, ggplot2::aes_string(x=xvar, y=yvar)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(main_title) +
    ggplot2::xlab(xvar) +
    ggplot2::ylab(yvar) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size = 15, face = "bold"))
}
