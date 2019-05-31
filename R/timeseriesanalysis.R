#' Graph time_series_plot
#' @param radian @name scatter_plot @description provides an easy way to view if data is norma;
#' @return value
#' @export
#'



time_series_analysis = function(df,y_variable,smooth_technique,start_date,end_date,by_n =1,main_title,w = 0.2,L = 5){
  s_d = as.Date(start_date)
  e_d = as.Date(end_date)
  
  ## Subset data
  df = dplyr::filter(df,stock_date %in% seq(s_d, e_d,by_n))
  vec_info = dplyr::select(df,y_variable)[,1]
  
  
  
  ## SMoothing Method
  if(smooth_technique=="E"){
    
    
    y_1 = vec_info[1]
    
    y_2 = w*vec_info[2] + (1-w)*y_1
    
    
    sm_fun = c(y_1,y_2)
    
    for(i in 1:(length(vec_info)-2)){
      tmp_value = w*vec_info[i+2]  + (1-w)*sm_fun[i+1]
      sm_fun = c(sm_fun,tmp_value)
    }
    df$Smoothed = sm_fun
    smooth_method = paste0("Exponential Smoothing w = ",w,sep='')
  } else if (smooth_technique =="MA"){
    
    df$Smoothed = c(rep(NA,L-1),zoo::rollmean(vec_info,k=L))
    smooth_method = paste0("Moving Average L = ",L,sep='')
  }
  
  
  
  ## Select appropriate 
  df = dplyr::select(df,Smoothed,y_variable,stock_date)
  
  df_mod = reshape2::melt(df, id="stock_date")  
  
  p = ggplot2::ggplot(df_mod, ggplot2::aes_string(x="stock_date", y="value",color="variable")) +
    ggplot2::geom_line()+
    ggplot2::theme_bw() +
    ggplot2::ggtitle(paste0(main_title,' from ',s_d,' to ',e_d,'\n',smooth_method,sep='')) +
    ggplot2::ylab(" Stock Price") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size = 15, face = "bold")) +
    ggplot2::scale_color_manual("Lines",breaks = c(y_variable, "Smoothed"),
                       values=c("red", "black"))
  
  colnames(df)[2]='var1'
  
  ds = dplyr::summarise(df,MSE = mean((Smoothed - var1)^2, na.rm = TRUE),
                        MAPE = mean(abs((Smoothed - var1)/var1), na.rm = TRUE)*100,
                        MAD = mean(abs((Smoothed - var1)), na.rm = TRUE))
  
  return(list(p,ds))
}

