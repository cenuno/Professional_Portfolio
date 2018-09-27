#
# Author:   Cristian E. Nuno
# Purpose:  Convert NAICS 2 and 4-digit .pdf document into a .csv
# Date:     September 27, 2018
#

# load necessary packages ---------
library(here)
library(pdftools)
library(tidyverse)

# transfrom .pdf into a table ----
df <- 
  # reads .pdf as a character vector
  pdf_text(pdf = here("raw_data", "2012naics4-digitcodes_0.pdf")) %>%
  # name each element its corresponding page number
  set_names(nm = paste0( "page_", seq_len(length.out = length(.)))) %>%
  imap_chr(.f = ~ if (.y == "page_1") {
    # eliminate the header that appears only on the first page
    str_sub(string = .x
            , start = 1L
            , end = 39L) %>%
      str_replace(string = .x
                  , pattern = .
                  , replacement = "")
  } else { 
    # otherwise, return the element as is to the vector
    .x 
  }) %>%
  # split vector into lists of vectors
  strsplit(split = "\\n") %>%
  map(.f = ~ .x %>%
        # split each string into two elements:
        # one for the digits and one for the words
        # note: this regex contains one capturing group with 
        #       3 alternatives positive look behind expressions
        #       1. 2 digits
        #       2. 4 digits
        #       3. Or 2 digits and 2 digits separated by a dash
        str_split("((?<=\\d{2})|(?<=\\d{4})|(?<=\\d{2}-\\d{2}))\\s") %>%
        map(.f = trimws) %>%
        unlist()) %>%
  unlist() %>%
  # transfrom character vector into
  # a 2 column matrix
  matrix(nrow = length(.) / 2
         , ncol = 2
         , byrow = TRUE
         , dimnames = list(NULL, c("naics_id"
                                   , "naics_description"))) %>%
  as.tibble()

# export results ----
write_csv(df, path = here("write_data", "naics_2and4_digit_desciptions.csv"))

# end of script #
