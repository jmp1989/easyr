ezr.plot_gainslift_compare=function (dataset, binary_target, fields, higher_morelikely = c(TRUE), 
          plot_type = "capture", return_tbl = TRUE, y_title = NULL,        plot_title = NULL, exclude_NA=TRUE) {
  
  
  if (length(higher_morelikely) !=  length(fields) ){
    stop("Mismatch in length of 'higher_more_likely' and number of predictors")
  }


  plot_type = tolower(plot_type)
  if (plot_type %in% c("capture", "response", "gains", "lift")) {
    print(paste0("Generating a ", plot_type, " plot."))
  }else {
    stop("Invalid plot_type: Should be capture, response, gains, lift")
  }
  
  iteration_n = 0
  master_data = data.frame()
  for (each_field in fields){
    iteration_n = iteration_n + 1
    
    if (exclude_NA==TRUE){
      
      tmp_dataset = dataset %>% filter(is.na(!!rlang::sym(each_field))==FALSE)
      
      if(nrow(tmp_dataset)!=nrow(dataset)){
    print_string = paste0(each_field, " had ", nrow(dataset)-nrow(tmp_dataset), " records or ", round((nrow(dataset)-nrow(tmp_dataset))/nrow(tmp_dataset),2)*100,"% of fields with NULL, which were excluded from plotting and metrics.")
      print(print_string)
      }
    } else {
      print('Retaining NULLs in predictor field.  They are sorted towards the bottom of the scoring')
      tmp_dataset = dataset
    }
    higher_morelikely_n = higher_morelikely[iteration_n]
    initial_data = ezr.gainslift(df = tmp_dataset, 
                                 binary_target = binary_target, 
                                 higher_morelikely = higher_morelikely_n,
                                 concise_result = FALSE, 
                                 prediction = each_field)
    initial_data$predictor = each_field
    master_data = bind_rows(master_data, initial_data)
    
  }
  
  
  if (plot_type == "capture") {
    plot_fields = 'cum_capture_rate'
  }
  if (plot_type == "gains") {
    plot_fields = 'cum_gain'
  }
  if (plot_type == "response") {
    plot_fields = "cum_response_rate"
  }
  if (plot_type == "lift") {
    plot_fields = "cum_lift"
  }
  
  
  result = master_data %>% ggplot(aes(color=predictor, x=cumulative_data_fraction, y = !!rlang::sym(plot_fields)))+geom_line(size=1.25) + labs(title=plot_title, x="Fraction Tested", y_title=y_title)+theme_Publication()+scale_colour_Publication() + scale_x_continuous(breaks = c(0,.1, .2, .3, .4, .5, .6, .7, .8, .9, 1))  + scale_y_continuous(breaks = c(0,.1, .2, .3, .4, .5, .6, .7, .8, .9, 1)) + theme(legend.title = element_blank()) + theme(panel.background = element_rect(fill = "white",   colour = NA), panel.grid = element_line(colour = "grey92"),   panel.grid.minor = element_line(size = rel(0.5)), panel.grid.major  = element_line(size = rel(0.5)),   strip.background = element_rect(fill = "grey85",   colour = "grey20"))
  
  if (plot_type == "capture") {
    result = result + geom_abline(lty=3, slope=1, intercept = 0)
  }
  
  
  # if( return_tbl==TRUE ){
  #   result = list(plot = result, data = master_data)
  #   print('Returning a list of "plot" & "data"')
  # }
  return(result)
}



ezr.plot_gainslift_compare(all_data, binary_target = 'fraud_flg', fields=c('income_monthly_net','emailage_score','time_since_last_ea_verification_epoch','time_since_first_ea_verification_epoch','idanalytics_idscore','lexis_nexis_riskview_score'), higher_morelikely = c(TRUE,TRUE,FALSE,FALSE,FALSE,TRUE))#+theme_bw()
