


#' Plot Number Against  Binary Target
#'
#' Plot a column against a binary target.
#'
#' Generates %s by bin style plot
#' Counts by Bin Style plot
#' Cumulative plot
#' Density Plot
#'
#' Useful for understanding how a value relates to a target.
#'
#'
#'
#' @param dataset dataframe
#' @param predictor numerical value
#' @param binary_target a column that only has 0s and 1s.
#' @param style Values are 'fixed','equal','quantile','pretty','percentile'.  Default is EQUAL with n_breaks=10
#' @param n_breaks Default is 10.  How many bins you want.
#' @param fixed_breaks If style=FIXED then you want a vector of values such as seq(0,1000,100)
#' @param return_as_1plot Default is TRUE. If TRUE, this returns a singple plot.  Otherwise returns list of plots
#' @param add_text  Future work...add text to bar plots.
#' @param default_bar_color TRUE or FALSE.  Default is FALSE.  This means plot comes out as black.  If True then color is orange.
#' @param include_response_rate.  Default TRUE.  Add the response rate plot to the capture rate plot
#' @param higher_morelikely.  Default is TRUE.  Higher prediction value = more likely to be a 1.  Needs to be set in order for gainslift to be sorted correctly.
#' @return Returns
#' @export
#'
#' @examples
ezr.plot_against_target = function(dataset, predictor ,binary_target, style='equal', n_breaks=10, fixed_breaks=NULL, return_as_1plot=TRUE, add_text = TRUE, default_bar_color=FALSE, include_response_rate=TRUE, higher_morelikely=TRUE){

    n_distinct_in_target = dplyr::n_distinct(dataset[[binary_target]] )
    if(n_distinct_in_target >2){
        stop('ERROR!: The binary column should only have two values in it.  Check for NULLs if you think there is just two values. ')
    }

    # call ezr.add_buckets for binning....

    if(is.numeric(dataset[[binary_target]])==TRUE){
        dataset[[binary_target]]=as.factor(dataset[[binary_target]])
    }


    density_plot = dataset %>% ggplot(aes(x=!!rlang::sym(predictor), color=!!rlang::sym(binary_target)))+ggplot2::geom_density(size=1.5)+ theme_Publication() + scale_colour_Publication()+labs(title=paste0('Density Plot: ', predictor,' vs. ', binary_target))+scale_y_continuous(breaks = scales::pretty_breaks())

    cum_density_plot  = ezr.plot_cum_density(dataset, numeric_field = predictor, grouping_field = binary_target)+labs(title=paste0('Cumulative Density Plot: ', predictor,' vs. ', binary_target))+scale_y_continuous(breaks = scales::pretty_breaks())

    # other plots....


    dataset = dataset %>% dplyr::select(predictor, binary_target)

    dataset =  easyr::ezr.add_bins(dataset = dataset, column = predictor, style = style, n_breaks = n_breaks,
                                      fixed_breaks = fixed_breaks) #+

    # just renaming...
    dataset2= dataset %>% dplyr::select(2:3)
    names(dataset2)[2]=predictor


    metrics_for_plotting = dataset2  %>% group_by(!!rlang::sym(predictor))%>% summarise(
        total_obs = n(),
        count = sum(!!rlang::sym(binary_target)=='1', na.rm = TRUE),
        count_0 = total_obs - count
    ) %>% ungroup() %>% mutate(
        pct = round(100 * (count / total_obs))
    )


    hjust = 0.5
    vjust = -0.5

    if(default_bar_color==FALSE){
        fill_color='black'
    } else {
        fill_color = '1f77b4'
    }


    pct_plot = metrics_for_plotting %>% ggplot(aes(x=!!rlang::sym(predictor), y = pct))+
        geom_bar(stat='identity',fill=fill_color)+theme_Publication()+labs(title=paste0('Percent Target By Bin: ', predictor,' vs. ', binary_target), y ='Target %')

    if (add_text==TRUE){
        pct_plot =pct_plot + geom_text(aes(y=pct, label=paste0(pct, "%")), position = position_dodge(width= 1), size=2.5, hjust=hjust, vjust=vjust)+scale_y_continuous(breaks = pretty_breaks(n=5), limits = c(0, max(metrics_for_plotting$pct * 1.07)))
    }

    #### count plot
    # looks messy, but just getting data for easier creation of bar-plot w/ labels.
    metrics_for_plotting2 = bind_rows(metrics_for_plotting %>% mutate(!!binary_target :=1),
                                      metrics_for_plotting %>% mutate(!!binary_target :=0, count = total_obs-count)) %>% mutate(!!binary_target := factor(!!rlang::sym(binary_target)))


    count_plot = metrics_for_plotting2 %>% ggplot(aes(x=!!rlang::sym(predictor), y=count, fill=!!rlang::sym(binary_target))) + geom_bar(stat='identity', position = 'dodge')+
        theme_Publication()+scale_fill_Publication() +
    scale_y_continuous(breaks = scales::pretty_breaks(n=10)) +
    labs(title=paste0('Count Target By Bin: ', predictor,' vs. ', binary_target))


    if (add_text==TRUE){
       count_plot =  count_plot + geom_text(aes(y=count, label=paste0(count)), position = position_dodge(width=.5), size=2.5, hjust=hjust, vjust=vjust)
    }


    # threshold plot

    data_for_threshold_plt = ezr.get_clf_metric_table(dataset = dataset, binary_response = binary_target, predictor = predictor)

    plt_thresholds = data_for_threshold_plt %>% ggplot(aes(x=threshold))+
        geom_line(aes(y=accuracy, color='accuracy'), size=1.5) +
        geom_line(aes(y=recall, color='recall'), size=1.5)+
        geom_line(aes(y=precision, color = 'precision'), size=1.5) +
        geom_line(aes(y=f1_score, color='f1_Score'),size=1.5)+
        theme_Publication()+scale_colour_Publication(name='') +
        labs(y='%', title=title) +
        scale_y_continuous(breaks=scales::pretty_breaks(n=6))

    # Precision vs AUC Plot

    plt_prauc = data_for_threshold_plt %>% ggplot(aes(x=recall, y=precision))+geom_line(size=1.5, color='#1f77b4')+theme_Publication()+scale_y_continuous(limits = c(0,1), breaks=scales::pretty_breaks(n=6)) + scale_x_continuous(limits = c(0,1), breaks=scales::pretty_breaks(n=6))+scale_colour_Publication() + annotate('text', x = .9, y = .9, label=' ')


    # Capture Rate & Response Plot

    data_for_gainslift = ezr.gainslift(df=dataset, binary_target = binary_target, prediction = predictor, higher_morelikely = higher_morelikely, concise_result=FALSE)

    # value for horizontal dotted line...
    baseline_guess_rate = data_for_gainslift %>% slice(100:100)%>% mutate(random_guess=cum_bads / n_records)%>%dplyr::select(random_guess)%>% as.numeric()

    if (include_response_rate==TRUE){
    plt_capt_rate = data_for_gainslift %>% ggplot(aes(x=cumulative_data_fraction, y=cum_capture_rate, color="cum_capture_rate"))+geom_line(size=2)+theme_Publication()+ labs(x='% Tested', y='% Captured & Response Rate', title=title)+geom_line(aes(y=response_rate, color="response_rate")) + geom_smooth(aes(y=response_rate),se=FALSE, color='darkgrey')+
        scale_color_manual(name="",
                           values = c("cum_capture_rate"="#1f77b4", "response_rate"="#ff7f0e")) + geom_abline(intercept=0, slope=1, lty=3)+geom_hline(yintercept = baseline_guess_rate, lty=3) } else {
        plt_capt_rate = data_for_gainslift %>% ggplot(aes(x=cumulative_data_fraction, y=cum_capture_rate, color="cum_capture_rate"))+geom_line(size=2)+theme_Publication()+ labs(x='% Tested', y='% Captured', title=title)+scale_color_manual(name="",  values = c("cum_capture_rate"="#1f77b4")) +  geom_abline(intercept=0, slope=1, lty=3)
    }

    # bar plot for Freq

    data_for_freq_plot = dataset %>% group_by(!!rlang::sym(binary_target)) %>% summarise(n = n(), pct = round(n/(nrow(dataset)),2) )%>% ungroup()

    freq_plt = data_for_freq_plot %>% ggplot(aes(x=!!rlang::sym(binary_target), y = n, fill=!!rlang::sym(binary_target)))+geom_bar(stat='identity')+scale_fill_Publication()+theme_Publication()+scale_y_continuous(breaks=scales:::pretty_breaks())+geom_text(aes(x=!!rlang::sym(binary_target), label = paste0(pct*100,'%, (',n,')'), y=n),vjust = -0.5, fontface=2) +labs(y='N', title=title)















    if(return_as_1plot==TRUE){
        result = ggpubr::ggarrange(
            pct_plot+labs(title=NULL),
            count_plot+labs(title=NULL),
            cum_density_plot+labs(title=NULL),
            density_plot+labs(title=NULL),
            freq_plt+labs(title=NULL),
            plt_capt_rate+labs(title=NULL),
            plt_thresholds + labs(title=NULL),
            plt_prauc + labs(title=NULL),
            common.legend = FALSE, legend='top')

    } else {
        result = list(pct_plot = pct_plot,
                      count_plot = count_plot,
                      cum_density_plot = cum_density_plot,
                      density_plot = density_plot,
                      freq_plt = freq_plt,
                      plt_capt_rate = plt_capt_rate,
                      plt_thresholds = plt_thresholds ,
                      plt_prauc = plt_prauc )
    }

    return(result)
}















