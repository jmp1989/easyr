#'  Gains, Lift, and KS Calculator
#'
#' Outputs a dataframe that has the cumulative capture rate, gains, and KS metrics at each 1% increment of the score.   Also produces the lower threshold of the score.
#'
#' @param df Dataframe of prediction results
#' @param binary_target Should work for either numeric or factor.  Must be 0s and 1s though.
#' @param prediction The prediction value.   Higher values = higher chance of obtaining a 1
#' @param higher_morelikely Default is TRUE.  If a higher prediction value is more likely to be a 1.  Set to false if higher is going to be a 0.
#' @param round_value Default is 2.  Rounds values to 2 decimal places for a nicer user experience.
#' @param concise_result Default is TRUE.  Won't print every record of gainslift table.
#' @return Returns a dataframe with various metrics at each progression into the scorecard.
#'
#' @examples ezr.gainslift(df = dataset_telco_churn_from_kaggle, binary_target = 'Churn', prediction ='TotalCharges',higher_morelikely = FALSE )
ezr.gainslift = function (df, binary_target, prediction, higher_morelikely = TRUE,
                          round_value = 6, concise_result = TRUE) {
    if (class(df)[1] == "H2OFrame") {
        retain_vars = c(binary_target, prediction)
        df = as.data.frame(df[retain_vars])
    }
    df = df %>% dplyr::select(c(binary_target, prediction))
    df = df %>% mutate(`:=`(!!binary_target, readr::parse_number(as.character(!!rlang::sym(binary_target)))))
    if (higher_morelikely == TRUE) {
        df = df %>% arrange(desc(!!rlang::sym(prediction)))
    }
    else {
        df = df %>% arrange(!!rlang::sym(prediction))
    }
    total_records = nrow(df)
    df$rowid = seq(1, total_records, 1)
    total_bads = sum(df[binary_target], na.rm = TRUE)
    df["cum_bads"] = cumsum(df[binary_target])
    df = df %>% mutate(cum_response_rate = cum_bads/rowid, cum_capture_rate = cum_bads/total_bads,
                       cum_expected_bads_baseline = (rowid/total_records) *
                           total_bads, cum_lift = cum_bads/cum_expected_bads_baseline,
                       cum_gain = cum_bads - cum_expected_bads_baseline)
    gains_lift_table = data.frame(cumulative_data_fraction = seq(0.01,
                                                                 1, 0.01))
    gains_lift_table = gains_lift_table %>% mutate(n_records = base::floor(cumulative_data_fraction *
                                                                               total_records))
    gains_lift_table = gains_lift_table %>% inner_join(df, by = c(n_records = "rowid")) %>%
        dplyr::rename(min_score = !!prediction)
    gains_lift_table = gains_lift_table %>% mutate(cum_nonresp_rate = (n_records -
                                                                           cum_bads)/(total_records - total_bads), ks_split = cum_capture_rate -
                                                       cum_nonresp_rate, new_records = n_records - lag(n_records,
                                                                                                       n = 1), new_bads = cum_bads - lag(cum_bads, n = 1), response_rate = round(new_bads/new_records,
                                                                                                                                                                                 2), new_bads = NULL, new_records = NULL, response_rate = ifelse(is.na(response_rate) ==
                                                                                                                                                                                                                                                     TRUE, cum_response_rate, response_rate)) %>% select(-!!rlang::sym(binary_target)) %>% mutate_all(.funs = funs(parse_number(as.character(.)))) %>%
        mutate_all(.funs = funs(round(., round_value)))
    if (concise_result == TRUE) {
        gains_lift_table = gains_lift_table %>% filter(cumulative_data_fraction %in%
                                                           c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08,
                                                             0.09, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6,
                                                             0.7, 0.8, 0.9, 1))
    }
    return(gains_lift_table)
}


