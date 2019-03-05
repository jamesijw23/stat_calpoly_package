#' Graph time_series_analysis
#' @param radian @name scatter_plot @description provides an easy way to view if data is norma;
#' @return value
#' @export
#'


time_series_plot = function(df, y_variable, line_color = "red",main_title, start_date,end_date,by_n = 1){
  s_d = as.Date(start_date)
  e_d = as.Date(end_date)
  
  
  df = dplyr::filter(df,stock_date %in% seq(s_d, e_d,by_n))
  ggplot2::ggplot(df, ggplot2::aes_string(x = "stock_date", y = y_variable)) +
    ggplot2::geom_line(color = line_color) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(paste0(main_title,' from ',s_d,' to ',e_d,sep='')) +
    ggplot2::ylab(paste0(y_variable," Stock Price")) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size = 15, face = "bold"))
  
}
