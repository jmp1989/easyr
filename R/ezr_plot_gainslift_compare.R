#' Compare Gainslift Using Multiple Predictors
#'
#'
#' Compare and plot  Cumulative Capture Rate, Gains, Lift, or Response Rate across 1 or more predictors.   Optionally returns the underlying dataframe.
#'
#' @param dataset  The dataset.  Can be an h2o dataframe
#' @param binary_target the target.  Should be 0/1.
#' @param fields The fields that
#' @param higher_morelikely Is a higher value more likely of a value of '1'?  Default is TRUE.   If multiple columns are provided, it is assumed they all have the same direction.   If they don't have the same direction, manipulate the data prior to using this function.
#' @param plot_type Default is 'capture'.   Valid values are 'capture', 'response', 'gains', or 'lift'.   All of these values are cumulative.
#' @param return_tbl Return the dataframe of values?
#' @param plot_title = Name of plot
#'
#'
#' @return
#' @export
#'
#' @examples
ezr.plot_gainslift_compare=function(dataset, binary_target, fields, higher_morelikely=TRUE, plot_type='capture', return_tbl=TRUE,y_title=NULL, plot_title=NULL ){

    plot_type = tolower(plot_type)

    if (plot_type %in% c('capture','response','gains','lift')){
        print(paste0('Generating a ', plot_type, ' plot.'))
    } else {
        stop('Error!  Invalid plot_type')
    }


    if(plot_type=='capture'){
        plot_fields = paste0(fields,'_cum_capture_rate')
    }
    if(plot_type=='gains'){
        plot_fields = paste0(fields,'_cum_gain')
    }
    if(plot_type=='response'){
        plot_fields = paste0(fields,'_cum_response_rate')
    }
    if(plot_type=='lift'){
        plot_fields = paste0(fields,'_cum_lift')
    }



    if(length(fields)>1){
        initial_data  = ezr.gainslift(df  =dataset, binary_target = binary_target, higher_morelikely = higher_morelikely, concise_result = FALSE, prediction =fields[1] )

        names(initial_data)[2:ncol(initial_data)] = paste0(fields[1],'_',names(initial_data)[2:ncol(initial_data)])  # renaming everything except threshold column


        for(each_field in fields[2:length(fields)]){


            tmp =  ezr.gainslift(df  =dataset, binary_target = binary_target, higher_morelikely = higher_morelikely, concise_result = FALSE, prediction = each_field )

            names(tmp)[2:ncol(tmp)] = paste0(each_field,'_',names(tmp)[2:ncol(tmp)])  # renaming everything except threshold column

            initial_data = initial_data %>% inner_join(tmp, by = 'cumulative_data_fraction')

        }
        result = initial_data
    } else {
        result  = ezr.gainslift(df  =dataset, binary_target = binary_target, higher_morelikely = higher_morelikely, concise_result = FALSE, prediction =fields[1] )
    }

    # now plot...



    # gather data for plotting...

    if(length(fields)>1){
    plot_data = result %>% select(cumulative_data_fraction, plot_fields)
    plot_data = gather(plot_data, field, value, -cumulative_data_fraction)

    # renaming for clarity when plotting...
    for(field_seq in seq(1,length(plot_fields))){

        field_to_rename = plot_fields[field_seq]
        new_field_name = fields[field_seq]

        plot_data = plot_data %>% mutate(
            field = ifelse(field==field_to_rename, new_field_name,field )
        )
    }

    plt = plot_data %>% ggplot(aes(x=cumulative_data_fraction, y=value, color=field))+
        geom_line()+
        theme_Publication()+scale_colour_Publication()+
        labs(x='% Tested', y=y_title, title=plot_title) + theme(legend.title=element_blank())
    }

    if(length(fields)==1){

        if(plot_type=='capture'){
            plot_fields = 'cum_capture_rate'
        }
        if(plot_type=='gains'){
            plot_fields = 'cum_gain'
        }
        if(plot_type=='response'){
            plot_fields = 'cum_response_rate'
        }
        if(plot_type=='lift'){
            plot_fields = 'cum_lift'
        }


        plt = result %>% ggplot(aes(x=cumulative_data_fraction, y=!!rlang::sym(plot_fields)))+geom_line()+theme_Publication()+scale_colour_Publication()+
            labs(x='% Tested', y=y_title, title=plot_title)
    }

    if(plot_type=='capture'){
        plt = plt + geom_abline(intercept = 0, slope=1, lty=3)
    }



    if(return_tbl == TRUE){
        print('Returning both the dataframe of metrics and the plot')
        result = list(data_tbl = result,
                      plt = plt)
    } else {
        result = plt
        print('Returning the comparative plot.   Dataframe of metrics is not returned.')
    }
    return(result)



}



ezr.plot_gainslift_compare(dataset = easyr::dataset_telco_churn_from_kaggle,binary_target = 'Churn',fields = 'TotalCharges')




