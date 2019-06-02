library(dplyr)
library(foreach)
library(heplots)


xyz_numeric_cor <- function(model_data)
{
  column_property <- data.frame(class = sapply(model_data , class))
  column_property$class <- as.character(column_property$class )

  factor_cols <- rownames(column_property)[column_property$class == "factor"]
  numeric_cols <- rownames(column_property)[!column_property$class == "factor"]

  temp_df <- model_data[, c(numeric_cols)]
  temp_df[is.na(temp_df)] <- 0

  # Simple Co-relation table
  df_numeric_numeric <- as.data.frame( cor(temp_df ,
                                           method = "pearson"
                                          )
                                      )
}

# Both factors
xyz_chi_cor <- function(model_data)
{

  column_property <- data.frame(class = sapply(model_data , class))
  column_property$class <- as.character(column_property$class )

  factor_cols <- rownames(column_property)[column_property$class == "factor"]

  # Chisquare table
  df_ctg_ctg <- data.frame(row.names = factor_cols )
  df_ctg_ctg[ , c(factor_cols)] <- NA

  # Chisquare table
  ### Association Strength
  for( i_col in colnames(df_ctg_ctg)){
  
    row_data <- foreach(j_col = rownames(df_ctg_ctg)) %do% {
    
      ## Pearson CHI-squared Test for Count Data
      ## Null Hypothesis - Independence
      ## Crammer's V
      chi2 = chisq.test(model_data[, c(i_col)],
                        model_data[, c(j_col)],
                        correct = F
      )
    
      normalize_factor = min(length(unique(model_data[, c(i_col)])) ,
                             length(unique(model_data[, c(j_col)]))
      )
      v = sqrt(chi2$statistic / (length(model_data[, c(i_col)]) * normalize_factor)  )
    } # For each statement
  
    df_ctg_ctg[,i_col] <- unlist(row_data)
  
  } # 1st for Loop

  df_ctg_ctg
}



xyz_anova_cor <- function(model_data)
{
  column_property <- data.frame(class = sapply(model_data , class))
  column_property$class <- as.character(column_property$class )

  factor_cols <- rownames(column_property)[column_property$class == "factor"]
  numeric_cols <- rownames(column_property)[!column_property$class == "factor"]


  model_data[, numeric_cols][is.na(model_data[, numeric_cols])] <- 0

  # ANOVA table
  df_ctg_numeric <- data.frame(row.names = numeric_cols )
  df_ctg_numeric[ , c(factor_cols)] <- NA


  # ANOVA TABLE
  for( i in colnames(df_ctg_numeric)){
  
    write(paste("Column : ",i) , file = "text.log", append = TRUE)
    if (length(unique(model_data[ , i])) > 1) {
      row_data <- foreach(j = rownames(df_ctg_numeric)) %do% {
      
        ## 1 way ANOVA --
        ## Null Hypotheis is Mean is same
        ## The "proportion of variance explained" measure R2R2 for multiple regression has an
        #  ANOVA equivalent,  (eta squared)
        #  j ~ i  Numeric ~ Categorical
      
        formula1 <- paste(j , "~" , i, sep = " ")
        aov2 <- (aov(as.formula(formula1) , data = model_data[, c(i, j)] ))
      
        #  R-square
        # "https://stats.stackexchange.com/questions/78808/how-to-compute-eta2-in-anova-by-hand"
        v = 1 - var(aov2$residuals)/var(model_data[, c(j)] , na.rm = TRUE)  
      
      } # If values present
      df_ctg_numeric[,i] <- unlist(row_data)
    } ## For each

  
  } # 1st For loop

  df_ctg_numeric
}
