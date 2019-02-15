#' Graph Histograms
#' @param radian @name histogram_plot @description provides an easy way to view histograms
#' @return value
#' @export



histogram_plot = function(df,x_variable,outline_color = "black", bin_color = "white",main_title){
  ggplot2::ggplot(df,ggplot2::aes_string(x = x_variable)) +
    geom_histogram(color = outline_color, fill=bin_color) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(main_title) +
    ggplot2::xlab(x_variable) +
    ggplot2::ylab('Frequency') +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size = 15, face = "bold"))
}
