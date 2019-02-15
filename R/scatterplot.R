#' Graph scatter_plot
#' @param radian @name scatter_plot @description provides an easy way to view if data is norma;
#' @return value
#' @export


usethis::use_package("ggplot2")
usethis::use_package("gridExtra")
usethis::use_package("dplyr")



scatter_plot = function(df,xvar,yvar,point_color = "black",line_color = "red",main_title){
  ggplot2::ggplot(df, ggplot2::aes_string(x = xvar, y = yvar)) +
    ggplot2::geom_smooth(method='lm',formula=y~x,se = F,color=line_color) +
    ggplot2::ggtitle(main_title) +
    ggplot2::geom_point(color=point_color) +
    ggplot2::theme_bw() +
    ggplot2::xlab(xvar) +
    ggplot2::ylab(yvar) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size = 15, face = "bold"))
}
