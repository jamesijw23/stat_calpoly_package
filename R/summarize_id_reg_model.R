#' summarize_id_reg_model
#' @param radian @name ss SummarizationlinearReg @description
#' @return value
#' @export


summarize_id_reg_model = function(df,categorical_expl_variable,response_variable){
  ## Group by Function
  gb_df = dplyr::group_by_at(df,categorical_expl_variable)
  ## Summarize Data
  sum_table = dplyr::summarise_at(gb_df,.vars = dplyr::vars(response_variable), 
                             .funs = list(
                               sample_size = ~dplyr::n(),
                               sample_mean = ~mean(., na.rm = TRUE),
                               sample_sd = ~sd(., na.rm = TRUE),
                               sample_min = ~min(., na.rm = TRUE),
                               sample_max = ~max(., na.rm = TRUE))) 
  ## Print on Table
  print.data.frame(sum_table)
  
}