#' it apply
#' L vector of labour unit
#' A
#' K fixed capital
#' @export



l_table<-  function(L,A,K,unit_cost=4)
  {


  library(dplyr)
if(length(L)>1 & length(K)==1)
{

  TP= A* sqrt(K) *sqrt(L)

  data = data.frame(L,TP)
  data <- data %>% mutate(AP= TP/L)
  data<- data %>% mutate(MP= c(0,diff(TP)))
  data<- data %>% mutate(LM=c(0,(diff(L)/2)+L[-length(L)]))

  data <- data %>% mutate(TR=MP*unit_cost)
  data<- data %>% mutate(int=case_when(
    TR > 56~"Profit",
    round(TR,0) == 56 ~"Balance",
    TRUE~"Loss"
  ))
  table1<-data %>% select(L,TP,MP,AP,TR)


  return(table1)



}

else
{


  TP= A* sqrt(K) *sqrt(L)

  data = data.frame(K,TP)
  data <- data %>% mutate(AP= TP/K)
  data<- data %>% mutate(MP= c(0,diff(TP)))
  data<- data %>% mutate(LM=c(0,(diff(K)/2)+K[-length(K)]))
  table2<-data %>% select(K,TP,MP,AP)
  return(table2)
}

}

