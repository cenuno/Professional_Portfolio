#
# Author:   Cristian E. Nuno
# Purpose:  Convert 2017-2022 NAICS and SIC crosswalk PDF 
#           to a tibble and export as .csv
# Date:     September 16, 2018
#

# load necessary packages ---------
library(here)
library(pdftools)
library(tidyverse)

# load necessary functions --------
EvenStevens <- function(x) {
  # Input:  a character vector
  # Output: a character vector that is divisble by 2
  #         with the newly added element being "1234 test"
  #         and inspects for a case of "8811" and adds "test" to it
  if (length(x) %% 2 == 1) {
    append(x, "1234 test")
  } else if ("8811" %in% x) {
    # in the case a value is "8811"
    # place "test" immediately after it is found
    sub("8811", replacement = "8811 test", x)
  } else {
    x
  }
}

LineBreakAdditions <- function(x) {
  # Input: a character vector
  # Output: a character vector that
  #         will contain less records than 'x' because
  #         1) it pastes together consecutive non-digit words 
  #         2) deletes the second element in the consecutive non-digit words
  #         3) pastes together the non-digit word with the second-element behind that one
  #         4) eliminates non-digit words from 'x'
  
  # Thank you, wp78de, for your help with this function
  # https://stackoverflow.com/questions/52357694/paste-together-consecutive-non-digit-elements
  
  # create an empty character vector
  output_text <- c()
  
  # create a for loop
  for (i in x) {
    if (grepl("^\\D*$", i, perl = TRUE)) {
      # if i in x does not contain a digit
      output_text[length(output_text)-1] <- 
        # add it to the previous element
        paste(output_text[length(output_text)-1], i)
    } else {
      # if i in x does contain a digit
      # append i to output_text
      output_text <- c(output_text, i)
    }
  }
  # return output_text to the Global Environment
  return(output_text)
}

# convert .pdf into dataframe ------
# one record per page
# we want to end up with 4 columns
# 1 for NAICS, NAICS Description, SIC and SIC Description
df <- 
  # reads .pdf as a character vector
  pdf_text(pdf = here("raw_data", "naics_sic_crosswalk.pdf")) %>%
  set_names(nm = paste0( "page_", seq_len(length.out = length(.)))) %>%
  imap_chr(.f = ~ if (.y == "page_1") {
    #eliminate the header that appears only on the first page
    str_sub(string = .x
            , start = 1L
            , end = 142L) %>%
      str_replace(string = .x
                  , pattern = .
                  , replacement = "")
  } else { 
    # otherwise, return all elements in the vector
    .x 
  } ) %>%
  # note: choosing base R's string split method
  #       because I wish to keep the name of each 
  #       element in my list
  #
  #       splitting each character vector into multiple
  #       character vectors based on the presense of 
  #       line breaks (i.e. \n)
  strsplit(split = "\\n" ) %>%
  # now for all character vectors within each page
  # split them by two by removing all white space
  # that is at least of length two and at no more than
  # length 100
  map(.f = ~ 
        str_split(string = .x, pattern = "\\s{2,100}") %>%
        unlist() %>%
        # remove all "" or "Aux" elements within the vector
        discard(.p = ~ . %in% c("", "Aux")) %>%
        # split vector by the pattern of one space, 4 digits, and one space
        str_split(pattern = "(?<=.)(?=\\s\\d{4}\\s)") %>%
        unlist() %>%
        trimws(which = "both") %>%
        LineBreakAdditions() %>%
        EvenStevens() %>%
        # split each string into two elements:
        # one for the digits and one for the words
        str_split("(?<=\\d)\\s{1}") %>%
        unlist() %>%
        # transfrom character vector into
        # a 4 column matrix
        matrix(nrow = length(.) / 4
               , ncol = 4
               , byrow = TRUE
               , dimnames = list(NULL, c("naics_id"
                                         , "naics_description"
                                         , "sic_id"
                                         , "sic_description"))) %>%
        as_tibble() %>%
        # clean up the sic_id and sic_description
        mutate(sic_id = ifelse(sic_id == "1234"
                               , NA
                               , sic_id)
               , sic_description = ifelse(sic_description == "test"
                                          , NA
                                          , sic_description))) %>%
  # collapse the list into one tibble
  # with a column that indicates the page_number 
  # the record was taken from
  bind_rows(.id = "page_number")

# export data frame as .csv -------
write_csv(df, here("write_data", "naics_sic_2017_2022_crosswalk.csv"))

# end of script #
