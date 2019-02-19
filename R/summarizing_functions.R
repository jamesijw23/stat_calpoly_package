#' Graph plot
#' @param radian @name ss @description
#' @return value
#' @export


summarize_one_variable = function(df,x_variable){
  vec = df %>%
    select(x_variable) %>%
    describe()
  return(vec)
}

summarize_two_qual_variable = function(df, x_variable, y_variable){
  df2 = data.frame(table(df[, x_variable],df[, y_variable]))
  df3 = df2[df2$Var1 != '',]
  df3 = df3[df3$Var2 != '',]
  colnames(df3) = c(x_variable,y_variable,'Frequency')
  return(df3)

}

summarize_anova = function(df, x_variable, y_variable){

  df2 = df %>%
    select(x_variable, y_variable)

  colnames(df2) = c('v1','v2')
  df2$v1 = as.factor(df2$v1)

  sum_table = df2 %>%
    dplyr::group_by(v1) %>%
    dplyr::summarise(sample_size_by_group = n(),
              mean_by_group = mean(v2),
              sd_by_group = sd(v2))

  return(sum_table)
}

summarize_slr = function(df, x_variable, y_variable){

  df2 = df %>%
    dplyr::select(x_variable, y_variable)

  colnames(df2) = c('v1','v2')
  sum_table = df2 %>%
    dplyr::summarise(sample_size = n(),
              mean_x = mean(v1),
              sd_x = sd(v1),
              mean_y = mean(v2),
              sd_y = sd(v2),
              cor_xy =cor(v1,v2))

  return(sum_table)
}
