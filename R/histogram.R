#' Graph Histograms
#' @param radian @name histogram_plot @description provides an easy way to view histograms
#' @return value
#' @export



histogram_plot = function(df,x_variable,y_variable='',outline_color = "black", bin_color = "white",main_title = '',pair = FALSE){
  if(pair == FALSE){
    ggplot2::ggplot(df,ggplot2::aes_string(x = x_variable)) +
      ggplot2::geom_histogram(color = outline_color, fill=bin_color) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(main_title) +
      ggplot2::xlab(x_variable) +
      ggplot2::ylab('Frequency') +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size = 15, face = "bold"))
  } else if(pair == TRUE){
    numRowVec = nrow(df) ## Number of rows
    id = c(1:(numRowVec/2),1:(numRowVec/2)) ## Create Id
    tmp1_df = dplyr::rename(df,exp_var = x_variable,res_var = y_variable) ## Change Names
    tmp2_df = dplyr::select(tmp1_df,exp_var,res_var) ## Select Variables
    tmp3_df = dplyr::mutate(tmp2_df,id =  id) ## Create an Id Variable
    tmp4_df = tidyr::spread(tmp3_df,"exp_var","res_var") ## Transform
    tmp4_df$Diff = tmp4_df[,3] - tmp4_df[,2] ## Find Diff
    
    
    ggplot2::ggplot(tmp4_df, ggplot2::aes(x=Diff)) +
      ggplot2::geom_histogram(color = outline_color, fill=bin_color) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle("Histogram of Differences") +
      ggplot2::xlab("Differences") +
      ggplot2::ylab("Frequency") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size = 15, face = "bold"))
    
    
  }
  
}
