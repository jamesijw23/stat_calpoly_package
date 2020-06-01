#' summarize_anova_
#' @param radian @name ss anova @description
#' @return value
#' @export



options(dplyr.print_max = 1e9)
summarize_anova_ = function(df, x_variable, y_variable){

  df2 = dplyr::select(df, x_variable, y_variable)

  colnames(df2) = c('v1','v2')
  df2$v1 = as.factor(df2$v1)

  sum_table = dplyr::group_by(df2,v1)
  sum_table = dplyr::summarise(sum_table,
                               sample_size_by_group = dplyr::n(),
                               mean_by_group = mean(v2,na.rm = T),
                               sd_by_group = sd(v2,na.rm = T))
  print.data.frame(sum_table)

}
