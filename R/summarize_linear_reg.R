#' summarize_slr
#' @param radian @name ss slr @description
#' @return value
#' @export




summarize_linear_reg = function(df, x_variable, y_variable){


  df2 = dplyr::select(df, x_variable, y_variable)

  colnames(df2) = c('x','y')
  sum_table = dplyr::summarise(df2,sample_size = n(),
                     mean_x = mean(x,na.rm = T),
                     sd_x = sd(x,na.rm = T),
                     mean_y = mean(y,na.rm = T),
                     sd_y = sd(y,na.rm = T),
                     cor_xy =cor(x,y, use="complete.obs"))

  
  return(sum_table)
}

