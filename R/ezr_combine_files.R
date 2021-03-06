
#Load & Concatenate Multiple CSVs into same dataframe

#' Title Concatenate Mutliple CSVs or Pipe txts into same dataframe
#'
#' @param directory  Default directory is working directory.   This is where all your files are.
#' @param pattern  What is the pattern of the files that you want to load?  Ideally only these files are in the folders.  This makes it easy to match with.  Enter *.csv for all csvs or *.txt for all pipe delimited txt
#' @param keep_label_file_name_origin Default is FALSE.  This preserves the file name as a column in the file so you can know where it came from
#' @param type default is 'csv' (lowercase).  Other valid value is 'txt'.  Will assume txt is a pipe delimited file.
#' @param max_guess How many records to guess before reading.  Default is 5000 which is larger than the  default readr::read_xxx values.   Set lower to increae speed.
#' @return Returns a concatenated dataframe of all the files
#'
#' @examples
ezr.combine_files = function(directory = getwd(), pattern="*.csv" , keep_label_file_name_origin = FALSE, type='csv', max_guess = 5000){

    print(paste0('reading files from', directory))

    setwd(directory)

    if(type=='csv'){
    funct_read_and_name = function(file_nm){
        readr::read_csv(file_nm, guess_max = min(max_guess, Inf)) %>% mutate(filename = file_nm) %>% mutate_all(as.character)
    }
    }
    if(type=='txt'){
        funct_read_and_name = function(file_nm){
            readr::read_delim(file_nm, delim = '|', guess_max = min(max_guess, Inf)) %>% mutate(filename = file_nm)  %>% mutate_all(as.character)
        }
    }

    my_pattern = pattern
    result = list.files(pattern = my_pattern, full.names = TRUE) %>% purrr::map_df(~funct_read_and_name(.)) %>% mutate_all(parse_guess)

    if (keep_label_file_name_origin == FALSE) {
        result$filename = NULL
    }

    return(result)
}
