#' Graph bar_plot
#' @param radian @name bar_plot @description easy way to bar plot
#' @return value
#' @export


bar_plot = function(df,x_variable,bar_color = "black",main_title){
  df[, x_variable] <- as.factor(df[, x_variable])
  ggplot2::ggplot(df, ggplot2::aes_string(x=x_variable))+
    ggplot2::geom_bar(stat="count", width=0.7, fill=bar_color)+
    ggplot2::theme_bw() +
    ggplot2::ggtitle(main_title) +
    ggplot2::xlab(x_variable) +
    ggplot2::ylab('Count') +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size = 15, face = "bold"))
}
