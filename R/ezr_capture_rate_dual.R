#' Capture Dual Values at a time
#'
#' Get the capture rate depending upon the value of a score.  Requires that the target be 0 or 1.  It can be either numeric, character, or factor.  Target2 is a 2nd option where you can compare two metrics side by side.
#'
#' Almost duplicates gains-lift table, but allows for side by side comparison.
#'
#' @param dataset  Dataframe
#' @param target   Target.  Should be 0 or 1.
#' @param score    Score that sorts for the capture rate.
#' @param n_perc   what percentile do you want to examine?
#' @param target2  should be 0 or 1.  A secondary thing to measure.
#' @param higher_morelikely  TRUE by default.  This is generally what you expect.
#' @param return_entire_table Return ntiles from 0-100.  Not yet implemented
#' @return
#' @export
#'
#' @examples
ezr.capture_rate_dual = function(dataset, target, score, n_perc = .10, target2=NULL, higher_morelikely=TRUE, return_entire_table=FALSE){


    if (is.null(target2)==FALSE){
        dataset = dataset %>% select(target, target2, score)
    } else{
        dataset = dataset %>% select(target, score)
    }

    if(higher_morelikely==TRUE){
        dataset = dataset %>% arrange(desc(!!rlang::sym(score)))
    } else{
        dataset = dataset %>% arrange(!!rlang::sym(score))
    }
    # record counts

    total_records  =nrow(dataset)

    ntile_frame = data.frame(ntile = seq(0,1,0.01)) %>% mutate(ntile_records = floor(ntile *total_records))

    # get cumsums
    if(is.null(target2)==FALSE){
        dataset = dataset %>% mutate(!!target := parse_number(!!rlang::sym(target)),
                                     !!target2 := parse_number(!!rlang::sym(target2)))

        total_target =
            dataset %>% summarise(total_target = sum(!!rlang::sym(target), na.rm = TRUE)) %>% dplyr::select(total_target) %>% as.numeric()

        total_target2 =
            dataset %>% summarise(total_target2 = sum(!!rlang::sym(target2), na.rm = TRUE)) %>% dplyr::select(total_target2) %>% as.numeric()


        dataset = dataset %>% mutate(
            seqid = seq(1, total_records,1),
            cum_target=cumsum(!!rlang::sym(target)),
            target_capt_rate = round(cum_target/total_target,2),
            cum_target2=cumsum(!!rlang::sym(target2)),
            target_capt_rate2 = round(cum_target2/total_target2,2)
        ) } else {
        dataset = dataset %>% mutate(!!target := parse_number(!!rlang::sym(target)))

            total_target =
            dataset %>% summarise(total_target = sum(!!rlang::sym(target), na.rm = TRUE)) %>% dplyr::select(total_target) %>% as.numeric()

        dataset = dataset %>% mutate(
            seqid = seq(1, total_records,1),
            cum_target=cumsum(!!rlang::sym(target)),
            target_capt_rate = round(cum_target/total_target,2)
        )

    }

    dataset_fields = setdiff(names(dataset), c(target, target2))
    ntile_frame = ntile_frame %>% inner_join(dataset %>% dplyr::select(dataset_fields), by = c('ntile_records'='seqid'))

    #rename



    if(is.null(target2)==FALSE){
        names(ntile_frame) = c('ntile',
                               'nrecords',
                               paste0('min_',score),
                               paste0('n_',target),
                               paste0('perc_',target),
                                paste0('n_',target2)
                               , paste0('perc_',target2))
    } else{
        names(ntile_frame) = c('ntile',
                               'nrecords',
                               paste0('min_',score),
                               paste0('n_',target),
                               paste0('perc_',target)
                              )
        }


    return(ntile_frame)
}



