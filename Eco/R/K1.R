#' it apply
#' df data frame
#' col : colour to line plot character

#' @export

library(dplyr)
library(ggplot2)
library(patchwork)
P_K_M<-function(df,col="red"){
  data=df
  data<- data %>% mutate(LM=c(0,(diff(K)/2)+K[-length(K)]))
  plot1<-ggplot(data) +
    aes(x = K, y = AP) +
    geom_line(size = 0.64, colour =col) +
    labs(title = "Labour Unit VS Average Production", 
         caption = "Cobb-Douglas Production Function") +
    geom_point(colour="black")+
    scale_x_continuous(breaks = K) +
    xlab("Capital Units")+
    ylab("Average Production")
  plot2<-data %>% #filter(LM!=0) %>% 
    ggplot() +
    aes(x = LM, y = MP) +
    geom_line(size = 0.64,colour="Red") +
    labs(title = "Labour Unit VS Marginal Production",  
         caption = "Cobb-Douglas Production Function") +
    geom_point(colour="Red")+
    scale_x_continuous(breaks = data$LM) +
    theme(legend.position="none") +
    xlab("Capital Units")+
    ylab("Marginal Production")
  
  #plotly::ggplotly(plot1)
  return(plot1/plot2)
  
  
  
}
