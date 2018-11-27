

#' Plot Various Classification Metrics at Once
#'
#' Plot various classification metrics:
#'
#' 1.  Basic distribution and counts
#' 2.  AUC
#' 3.  PRAUC
#' 4.  Cum Lift
#' 5.  Cum Capture Rate
#' 6.  Cum Gain (n) over a random guess
#'
#' @param dataset
#' @param truth The truth variable
#' @param target The target variable
#' @param higher_more_likely Higher prediction values correspond with greater chance of 1 for the truth
#' @return
#' @export
#'
#' @examples ezr.plot_clf_metrics(dataset = easyr::dataset_telco_churn_from_kaggle, truth = 'Churn','TotalCharges',higher_more_likely = FALSE)

ezr.plot_clf_metrics = function(dataset, truth, prediction, higher_more_likely=TRUE){


  gainslift_data  =ezr.gainslift(df = dataset,binary_target = truth ,prediction =prediction , higher_morelikely = higher_more_likely)

  # cum_lift:

  plt_cum_lift = gainslift_data %>% ggplot(aes(x=cumulative_data_fraction, y=cum_lift))+geom_path()+coord_equal()+theme_classic()+labs(x= '% Tested', y=element_blank(), title = 'Cumulative Lift')

  # precision_recall:

  pr_data=dataset %>% yardstick::pr_curve(!!rlang::sym(truth), !!rlang::sym(prediction))

  pr_data_value=dataset %>% yardstick::pr_auc(!!rlang::sym(truth), !!rlang::sym(prediction))

  plt_pr = pr_data %>% ggplot(aes(x=recall, y=precision))+geom_path()+coord_equal()+theme_classic()+labs(title='Precision vs Recall')+annotate(geom='text', x = .50, y=.25, label = paste0('PRAUC: ' ,round(pr_data_value$.estimate,2)))

  # auc:

  plt_auc=ezr.auc_plot(dataset = dataset, truth = truth, prediction = prediction)


  # distribution: count and percent

  plt_distribution = ezr.plot_ordered_bar(dataset=dataset, column = truth,vertical = TRUE)

  # percent
  #gain_capture = dataset %>% yardstick::gain_capture(!!rlang::sym(truth),!!rlang::sym(prediction)) %>% select(.estimate) %>% as.numeric()

  capt5 = gainslift_data %>% filter(cumulative_data_fraction==0.05) %>% select(cum_capture_rate) %>% as.numeric() %>% round(2)
  capt15 = gainslift_data %>% filter(cumulative_data_fraction==0.15) %>% select(cum_capture_rate) %>% as.numeric() %>% round(2)
  capt25 = gainslift_data %>% filter(cumulative_data_fraction==0.25) %>% select(cum_capture_rate) %>% as.numeric() %>% round(2)


  plt_capt_rate=autoplot(dataset %>% yardstick::gain_curve(!!rlang::sym(truth),!!rlang::sym(prediction)))+theme_classic() + ggplot2::annotate(geom='text', x=75, y=25, label = paste0('Capture Rate 5%: ', capt5, '\n Capture Rate 15%: ',capt15 , '\n Capture Rate 25%: ', capt25), size=3)+labs(title = 'Cumulative Capture Rate') + geom_abline(intercept = 0, slope = 1, lty=3)

  gridExtra::grid.arrange(plt_capt_rate, plt_distribution, plt_auc, plt_pr, plt_cum_lift)

  best_stopping_value=gainslift_data %>% filter(cum_gain == max(cum_gain, na.rm = TRUE)) %>% select(cumulative_data_fraction) %>% as.numeric()


  plt_gain = gainslift_data %>% ggplot(aes(x=cumulative_data_fraction, y = cum_gain))+geom_path()+theme_classic()+labs(title = 'Cumulative Gain vs Guess', y = 'Gain (n)')+geom_vline(xintercept   = best_stopping_value, lty=3)



  result =cowplot::plot_grid(plt_capt_rate, plt_distribution, plt_auc, plt_pr, plt_cum_lift, plt_gain)

  return(result)
}


