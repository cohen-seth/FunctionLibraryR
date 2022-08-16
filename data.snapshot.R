# data.snapshot.v3 (version 3)
# version 3 includes the summary's for numeric variables (from add_stats input)
data.snapshot <- function(df, add_labels, add_stats){
  
  output <- c()
  
  df.name = deparse(substitute(df))
  
  if(is.null(add_labels) == FALSE){
    labels = var_label(df)
  }
  
  df=as.data.frame(df)
  
  
  merge.all <- function(x, y) {
    merge(x, y, by =c("Variable","type_of"), all=TRUE)
  }
  
  
  # Distinct observations for each Variable
  a = as.data.frame(
    t(
      df %>%
        summarise(across(where(is.character), ~ n_distinct(.x)))
    )
  ) %>% set_colnames(c("n_distinct")) %>%
    add_column(.before = 1, "Variable" = row.names(.)) %>%
    add_column(.after = 1, "type_of" = "character")
  # Distinct observations for each Variable
  b = as.data.frame(
    t(
      df %>%
        summarise(across(where(is.character), ~ sum(!is.na(.x))))
    )
  ) %>% set_colnames(c("nonNA")) %>%
    add_column(.before = 1, "Variable" = row.names(.)) %>%
    add_column(.after = 1, "type_of" = "character")
  
  # Number of NA's for each Variable
  c = as.data.frame(
    t(
      df %>%
        summarise(across(where(is.character), ~ sum(is.na(.x))))
    )
  ) %>% set_colnames(c("is.na")) %>%
    add_column(.before = 1, "Variable" = row.names(.)) %>%
    add_column(.after = 1, "type_of" = "character")
  
  # Number of blanks for each Variable
  d = as.data.frame(
    t(
      df %>%
        summarise(across(where(is.character), ~sum(.x == "")))
    )
  ) %>% set_colnames(c("n_blank")) %>%
    add_column(.before = 1, "Variable" = row.names(.)) %>%
    add_column(.after = 1, "type_of" = "character")
  
  
  # # Percent NA for each Variable
  # e = as.data.frame(
  #   t(
  #     df %>%
  #       summarise(across(where(is.character), ~sum(is.na(.x))/n()))
  #   )
  # ) %>% set_colnames(c("is.na (%)")) %>%
  #   add_column(.before = 1, "Variable" = row.names(.)) %>%
  #   add_column(.after = 1, "type_of" = "character")
  #
  # Merge output describing character Variables
  output.chr <- Reduce(merge.all, list(a,b,c,d))
  
  
  # Distinct observations for each Variable
  a2 = as.data.frame(
    t(
      df %>%
        summarise(across(where(is.numeric), ~ n_distinct(.x)))
    )
  ) %>% set_colnames(c("n_distinct")) %>%
    add_column(.before = 1, "Variable" = row.names(.)) %>%
    add_column(.after = 1, "type_of" = "numeric")
  # Distinct observations for each Variable
  b2 = as.data.frame(
    t(
      df %>%
        summarise(across(where(is.numeric), ~ sum(!is.na(.x))))
    )
  ) %>% set_colnames(c("nonNA")) %>%
    add_column(.before = 1, "Variable" = row.names(.)) %>%
    add_column(.after = 1, "type_of" = "numeric")
  
  c2 = as.data.frame(
    t(
      df %>%
        summarise(across(where(is.numeric), ~ sum(is.na(.x))))
    )
  ) %>% set_colnames(c("is.na")) %>%
    add_column(.before = 1, "Variable" = row.names(.)) %>%
    add_column(.after = 1, "type_of" = "numeric")
  
  
  # Number of blanks for each Variable
  d2 = as.data.frame(
    t(
      df %>%
        summarise(across(where(is.numeric), ~sum(.x == "")))
    )
  ) %>% set_colnames(c("n_blank")) %>%
    add_column(.before = 1, "Variable" = row.names(.)) %>%
    add_column(.after = 1, "type_of" = "numeric")
  #
  # e2 = as.data.frame(
  #   t(
  #     df %>%
  #       summarise(across(where(is.numeric), ~sum(is.na(.x))/n() * 100)) %>% round(1)
  #   )
  # ) %>% set_colnames(c("is.na (%)")) %>%
  #   add_column(.before = 1, "Variable" = row.names(.)) %>%
  #   add_column(.after = 1, "type_of" = "numeric")
  #
  
  # Merge output describing numeric Variables
  output.num <- Reduce(merge.all, list(a2,b2,c2,d2))
  # }
  
  # Merge output describing character Variables with output describing numeric Variables
  output = merge(output.chr, output.num, all = T)
  
  if(is.null(add_stats) == FALSE){
    
    # means
    i = as.data.frame(
      t(
        df %>%
          summarise(across(where(is.numeric), ~summary(.x))) %>% round(2)
      )
    )[,1:6] %>% set_colnames(c("Min", "1st Qu", "Median", "Mean", "3rd Qu","Max")) %>%
      add_column(.before = 1, "Variable" = row.names(.)) %>%
      add_column(.after = 1, "type_of" = "numeric")
    
    
    
    # sd's
    j = as.data.frame(
      t(
        df %>%
          summarise(across(where(is.numeric), ~sd(.x, na.rm = TRUE))) %>% round(2)
      )
    ) %>% set_colnames(c("sd")) %>%
      add_column(.before = 1, "Variable" = row.names(.)) %>%
      add_column(.after = 1, "type_of" = "numeric")
    
    
    # Merge output describing numeric Variables
    output.stats <- Reduce(merge.all, list(i,j))
    # }
    
    # Merge output describing character Variables with output describing numeric Variables
    output = merge(output, output.stats, all = T)
  }
  # add original df name as new Variable. Adding this for when all are combined to produce the plots
  output = output %>% add_column(.before = 1, "DataFrame" = df.name)
  output = as.data.table(output)
  
  
  # reording output to match how the input data was ordered
  variable_order = c(colnames(df))
  output$Variable = factor(output$Variable,levels=variable_order,ordered=TRUE)
  output = output %>% arrange(Variable)
  
  
  if(is.null(add_labels) == FALSE){
    output = output %>% add_column(.after = 2, "Label" = labels)
  }
  
  
  return(output)
}

