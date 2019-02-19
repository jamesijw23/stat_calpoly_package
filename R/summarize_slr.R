#' summarize_slr
#' @param radian @name ss slr @description
#' @return value
#' @export





summarize_slr = function(df, x_variable, y_variable){

  df2 = dplyr::select(df, x_variable, y_variable)

  colnames(df2) = c('x','y')
  sum_table = dplyr::summarise(df2,sample_size = n(),
                     mean_x = mean(x),
                     sd_x = sd(x),
                     mean_y = mean(y),
                     sd_y = sd(y),
                     cor_xy =cor(x,y))

  return(sum_table)
}
