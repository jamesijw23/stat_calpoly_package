#' summarize_two_quant_groups 
#' @param radian @name ss anova @description
#' @return value
#' @export



options(dplyr.print_max = 1e9)
summarize_two_quant_groups = function(df, x_variable, y_variable,pair ){
  
  df2 = dplyr::select(df, x_variable, y_variable)
  
  if(pair == FALSE) { 
    colnames(df2) = c('v1','v2')
    df2$v1 = as.factor(df2$v1)
    
    sum_table = dplyr::group_by(df2,v1)
    sum_table = dplyr::summarise(sum_table,
                                 sample_size_by_group = dplyr::n(),
                                 mean_by_group = mean(v2,na.rm = T),
                                 sd_by_group = sd(v2,na.rm = T))
  } else if(pair == TRUE){
    size_group = nrow(df2)/2
    df3 = data.frame(mcases = c(seq(1,size_group),seq(1,size_group)),
                     df2)
    colnames(df3) = c('v0','v1','v2')
    df4 = tidyr::spread(df3,v1,v2)
    The_Diff = paste0(colnames(df4)[2],' - ',colnames(df4)[3])
    colnames(df4)= c('v0','v1','v2')
    df5 = dplyr::mutate(df4,diff = v2 - v1)
    sum_table = data.frame(round(summarize_one_quant_variable(df5,'diff'),4),
                           The_Diff)
    rownames(sum_table) = NULL
  }
  return(sum_table)
  
}



