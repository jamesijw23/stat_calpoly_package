#' Graph box_plot
#' @param radian @name box_plot @description easy way to box plot
#' @return value
#' @export


# usethis::use_package("ggplot2")
# usethis::use_package("gridExtra")
# usethis::use_package("dplyr")



box_plot = function(df,x_variable,y_variable,main_title){
  df[, x_variable] <- as.factor(df[, x_variable])
  ggplot2::ggplot(df, ggplot2::aes_string(x=x_variable, y=y_variable)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(main_title) +
    ggplot2::xlab(x_variable) +
    ggplot2::ylab(y_variable) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size = 15, face = "bold"))
}
