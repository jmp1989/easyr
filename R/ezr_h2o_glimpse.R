
#' H2O Version of Glimpse
#'
#' H2o's version of glimpse.  You control the <N> to be shown from header
#'
#' @param h2odataset  h2odataframe
#' @param n_head_values   Number of values to concatenate into a comma seperated list. Keep at 0 to turn off
#' @param show_extra_info Shows Min, Max, Sigma.  Default is FALSE
#' @return dataframe of column name, type, and col-index position.  Onptionally returns the first N rows
#' @export
#'
#' @examples
ezr.h2o_glimpse = function(h2odataset, n_head_values=0, show_extra_info=FALSE){




  if(n_head_values > 10){
    print('Value entered is too large. Defaulting head values to 10')
    n_head_values =10
  }

  head=as.data.frame(h2o::h2o.head(h2odataset,n_head_values))
  head= head %>% summarise_all(., toString) %>% gather('variable', 'head_values')





  summary_info=h2o.describe(h2odataset) %>% mutate(Type = as.character(Type),
                                                   Label = as.character(Label))
  summary_info['Missing_Perc'] = round(summary_info['Missing']/nrow(h2odataset),2)
  summary_info = summary_info %>% mutate(
      PosInf=NULL,
      NegInf = NULL
  ) %>% dplyr::rename(
      Variable = Label
  ) %>% dplyr::select(
      Variable, Type, Missing, Missing_Perc, Zeros, dplyr::everything()
  ) %>% mutate(
      Type = ifelse(Type=='enum','factor', Type)
  )

  names(summary_info) = tolower(names(summary_info))


  summary_info = summary_info %>% inner_join(
      head, by = 'variable'
  ) %>% mutate(cardinality=NULL)


  # count NULLs


  uniques = c()
  for (each_record in c(summary_info$variable) ){

    uniques = append(uniques, h2o.nlevels(h2odataset[each_record]))

  }

  summary_info['n_factor_lvls']=uniques


  summary_info = summary_info %>% select(variable,type, n_factor_lvls,missing,missing_perc, zeros, everything()) %>% mutate(
      n_factor_lvls = ifelse(type !='factor'  , NA, n_factor_lvls)
  )


if(show_extra_info==FALSE){
    summary_info = summary_info %>% mutate(
        min=NULL,
        max=NULL,
        sigma=NULL
    )
}
  if(n_head_values <1 | is.null(n_head_values)==TRUE){
      summary_info = summary_info %>% mutate(
          head_values=NULL
      )
  }



  return(summary_info)
}
