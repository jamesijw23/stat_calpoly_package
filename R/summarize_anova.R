#' summarize_anova
#' @param radian @name ss anova @description
#' @return value
#' @export




summarize_anova = function(df, x_variable, y_variable){

  df2 = dplyr::select(df,x_variable, y_variable)

  colnames(df2) = c('v1','v2')
  df2$v1 = as.factor(df2$v1)

  sum_table = dplyr::group_by(df2,v1)
  sum_table = dplyr::summarise(sum_table,
                               sample_size_by_group = n(),
                               mean_by_group = mean(v2),
                               sd_by_group = sd(v2))

  return(sum_table)
}
