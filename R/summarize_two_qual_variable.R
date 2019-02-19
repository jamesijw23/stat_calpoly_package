#' summarize_2_qualitative
#' @param radian @name ss 2 qualitative @description
#' @return value
#' @export



summarize_two_qual_variable = function(df, x_variable, y_variable){
  df2 = data.frame(table(df[, x_variable],df[, y_variable]))
  df3 = df2[df2$Var1 != '',]
  df3 = df3[df3$Var2 != '',]
  colnames(df3) = c(x_variable,y_variable,'Frequency')
  return(df3)

}
