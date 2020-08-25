#' it apply
#' df data frame
#' col is colour to line plot

#' @export



library(dplyr)
library(ggplot2)

p_K<- function(df,col){
  
  
  data=df
  plot1<-ggplot(data) +
    aes(x = K, y = TP) +
    geom_line(size = 0.64, colour = "#440154") +
    labs(title = "Capital Unit VS Total Production",  caption = "Cobb-Douglas Production Function") +
    geom_point()+
    scale_x_continuous(breaks = K) +xlab("Capital Units")+ylab("Total Production")
  
 plot11<- plotly::ggplotly(plot1)
  return(plot11)
}
