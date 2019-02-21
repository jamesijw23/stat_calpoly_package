#' Graph scatter_plot
#' @param radian @name scatter_plot @description provides an easy way to view if data is norma;
#' @return value
#' @export
#'


time_series_plot = function(df,stock_date, x_variable, line_color = "red",main_title){
ggplot2::ggplot(df, ggplot2::aes_string(stock_date, x_variable)) +
  ggplot2::geom_line(color = line_color) +
  ggplot2::theme_bw() +
  ggplot2::ggtitle(main_title) +
  ylab("Stock Price") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size = 15, face = "bold"))
}
