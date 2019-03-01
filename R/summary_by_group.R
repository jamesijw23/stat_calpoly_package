#' summary_by_group
#' @param radian @name ss after_anova @description
#' @return value
#' @export



summary_by_group = function(df,x_variable,y_variable){
  df1 = dplyr::select(df,x_variable,y_variable)
  
  colnames(df1) = c('v1','v2')
  
  sum_table_1 = dplyr::group_by(df1,v1) 
  sum_table_2 = dplyr::summarise(sum_table_1, n_group = dplyr::n(),
                                 mean_group = round(mean(v2),4),
                                 sd_group = round(sd(v2),4)) 
  print.data.frame(sum_table_2)
}