ezr.auc_plot = function (dataset, truth, prediction, title = NULL, plt_text=TRUE, PRAUC=FALSE) {
  # colors 
  color_mapping  = c("#1f77b4",
                     "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b",
                     "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
  # begin with generating dataframe for plot
  
  iteration_n = 0
  plt_data = data.frame()
  plt_data_text = data.frame()
  for(each_pred in prediction){
    
    if (PRAUC==FALSE){
      
      
      
      plt_data_n = yardstick::roc_curve(dataset, !!rlang::sym(truth), 
                                        !!rlang::sym(each_pred)) %>% 
        mutate(FPR = 1 - specificity)
      
      AUC_n =  dataset %>% yardstick::roc_auc(!!rlang::sym(truth), 
                                              !!rlang::sym(each_pred))
      names(plt_data_n) = c("threshold", "specificity", "TPR", "FPR")
      
      plt_data_n$auc = AUC_n$.estimate
      
      plt_data_n = plt_data_n %>% mutate(threshold = ifelse(
        threshold==Inf,1,
        ifelse(threshold==-Inf, 0, threshold)
      ))
      
      plt_data_n$feature = each_pred
      
      
      plt_data = bind_rows(plt_data, plt_data_n)
      plt_data_text = bind_rows(plt_data_text, plt_data_n %>% dplyr::select(feature, auc))
      
    } 
    
    if (PRAUC==TRUE){
      
      plt_data_n = yardstick::pr_curve(dataset, !!rlang::sym(truth), 
                                       !!rlang::sym(each_pred)) 
      
      # these names get switched at the end.  kept these in order to keep plotting easier later on in function call
      names(plt_data_n) = c('threshold','recall','precision')   
      
      
      PRAUC_n =  dataset %>% yardstick::pr_auc(!!rlang::sym(truth), 
                                               !!rlang::sym(each_pred))
      plt_data_n$prauc = PRAUC_n$.estimate
      plt_data_n$feature = each_pred
      
      
      plt_data = bind_rows(plt_data, plt_data_n)
      plt_data_text = bind_rows(plt_data_text, plt_data_n %>% dplyr::select(feature, prauc))
      
    }
    
  }
  
  plt_data_text = plt_data_text %>% base::unique()
  
  if (PRAUC==FALSE){
    final_plt = plt_data %>% ggplot(aes(
      x=FPR, y=TPR, color=feature
    ))+geom_path(size=1.25)+theme_Publication() + labs(title=title)
  }
  if (PRAUC==TRUE){
    final_plt = plt_data %>% ggplot(aes(
      x=recall, y=precision, color=feature
    ))+geom_path(size=1.25)+theme_Publication() + labs(title=title)
    
  }
  
  records_to_plot = nrow(plt_data_text)
  
  # plot text if needed.
  if(plt_text==TRUE){
    text_iterator = 0
    if(PRAUC==FALSE){
      # AUC plt text
      for(each_record in prediction){
        text_iterator = text_iterator+1
        
        
        final_plt = final_plt + annotate("text",x=.75, y=text_iterator * .08, label=paste0(
          each_record, ": ", round(as.numeric(plt_data_text[text_iterator,]$auc),3)
        ), color =color_mapping[text_iterator],fontface='bold' )
      }
    } else {
      for(each_record in prediction){
        text_iterator = text_iterator+1
        
        
        final_plt = final_plt + annotate("text",x=.75, y=text_iterator * .08, label=paste0(
          each_record, ": ", round(as.numeric(plt_data_text[text_iterator,]$prauc),3)
        ), color =color_mapping[text_iterator] , fontface='bold')
      }
    }
    # here show PRAUC instead 
  }
  
  result = final_plt + theme_Publication()+scale_colour_Publication() + theme(legend.title = element_blank())
  
  if(PRAUC==FALSE){
    result = result + geom_abline(lty = 3, intercept = 0, slope=1)} 
  
  if(PRAUC==TRUE){
    occurence_rate = as.numeric(sum(dataset[[truth]]==1, na.rm = TRUE)  /nrow(dataset))
    
    result = result+geom_hline(yintercept = occurence_rate, lty=3)
    
    
  }
  
  return(result)
}


