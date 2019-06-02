get_stratified_positions <- function (input_label_list, percentage
                                     
){
 
  set.seed(123)
  temp_df <- data.frame(label = input_label_list)
 
  label_distribution <- group_by(temp_df, label) %>%
    summarise(total_count = n()) %>%
    arrange(desc(total_count))
 
 
  label_distribution$train_count <-  as.integer(label_distribution$total_count * percentage)
  label_distribution$test_count <- label_distribution$total_count - label_distribution$train_count
 
 
  train_position <- c()
  test_position <- c()
 
  for (i in 1:nrow(label_distribution)){
   
   
    position_list   <- which(temp_df$label == label_distribution[i,]$label )
    train_position1 <- sample(position_list  , label_distribution[i,]$train_count )
    test_poisition1 <- setdiff(position_list , train_position1)
   
    train_position <- c(train_position1 , train_position)
    test_poisition <- c(test_poisition1 , test_position)
   
  }
 
  return(train_position)
 
}
