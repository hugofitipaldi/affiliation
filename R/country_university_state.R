#' Find country on affiliation field based on country name
#'
#' Matches country names on free text affiliation field
#'
#' This function was designed to internal use in the package. The aim is to find a match of a country name on the free-text affiliation field.
#'
#' @param x free-text (affiliation field)
#'
#' @author Hugo Fitipaldi
#'
#'
#' @return This function returns a character string with the name of the country
#' @export detecting_country_country
#' @examples
#' detecting_country_country("Lund University Diabetes Centre, Malmö, Skåne, Sweden")
#'
detecting_country_country <- function(x) {

  text <- x
  text <- tolower(text)
  text <- stringi::stri_trans_general(str = text, id = "Latin-ASCII")
  text <- gsub('[[:digit:]]+', '', text)
  text <- qdap::rm_stopwords(text, separate = FALSE)
  text <- gsub('[[:punct:] ]+',' ', text)
  text <- gsub('centre','', text)
  text <- gsub('university','', text)

  countries <- affiliation::countries_list

  countries$country_name <- countries$name
  countries$name <- tolower(countries$name)
  countries$name <- stringi::stri_trans_general(str = countries$name, id = "Latin-ASCII")
  countries$name <- paste0('\\b' ,countries$name, '\\b')

  return(toString(unique(countries[stringr::str_detect(text, countries$name) == TRUE,]$country_name)))

}

#' Find country on affiliation field based on university name
#'
#' Find country names on free text affiliation field based on university names
#'
#' This function was designed to internal use in the package. The aim is to find the country name by matchin with the university location.
#'
#' @param x free-text (affiliation field)
#'
#' @author Hugo Fitipaldi
#'
#'
#' @return This function returns a character string with the name of the country
#' @export detecting_country_university
#' @examples
#' detecting_country_university("Lund University Diabetes Centre, Malmö, Skåne, Sweden")
#'

detecting_country_university <- function(x) {

  text <- x
  text <- tolower(text)
  text <- stringi::stri_trans_general(str = text, id = "Latin-ASCII")
  text <- gsub('[[:digit:]]+', '', text)
  text <- gsub('[[:punct:] ]+',' ', text)

  universities <- affiliation::universities_list


  universities$university_name <- tolower(universities$university_name)
  universities$university_name <- stringi::stri_trans_general(str = universities$university_name, id = "Latin-ASCII")

  return(toString(unique(universities[stringr::str_detect(text, universities$university_name) == TRUE,]$country_name)))
}

#' Find country on affiliation field based on name of state
#'
#' Find country name on free text affiliation field based on state name
#'
#' This function was designed to internal use in the package. The aim is to find the country name by matching with a name of state.
#'
#' @param x free-text (affiliation field)
#'
#' @author Hugo Fitipaldi
#'
#'
#' @return This function returns a character string with the name of the country
#' @export detecting_country_state
#' @examples
#' detecting_country_state("Lund University Diabetes Centre, Malmö, Skåne, Sweden")
#'

detecting_country_state <- function(x) {

  text <- x
  text <- tolower(text)
  text <- stringi::stri_trans_general(str = text, id = "Latin-ASCII")
  text <- gsub('[[:digit:]]+', '', text)
  text <- qdap::rm_stopwords(text, separate = FALSE)
  text <- gsub('[[:punct:] ]+',' ', text)
  text <- gsub('centre','', text)
  text <- gsub('university','', text)

  states <- affiliation::states_list

  states$name <- tolower(states$name)
  states$name <- stringi::stri_trans_general(str = states$name, id = "Latin-ASCII")
  states$name <- paste0('\\b' ,states$name, '\\b')

  return(toString(unique(states[stringr::str_detect(text, states$name) == TRUE,]$country_name)))

}

#' Detect US states abbreviates
#'
#' Finds US state abbreviations.
#'
#' This function was designed to detect US state abbreviations. Internal use only.
#'
#' @param x free-text (affiliation field)
#'
#' @author Hugo Fitipaldi
#'
#'
#' @return This function returns a character string with the name of the detected US state.
#' @export detecting_us_states
#' @examples
#' detecting_us_states("OH is a United Stated state.")
#'

detecting_us_states <- function(x) {

  text <- x
  text <- stringi::stri_trans_general(str = text, id = "Latin-ASCII")
  text <- gsub('[[:digit:]]+', '', text)
  text <- gsub('[[:punct:] ]+',' ', text)
  text <- gsub('centre','', text)
  text <- gsub('university','', text)

  abbreviation    <- c("AL",
             "AK", "AZ", "KS", "UT", "CO", "CT",
             "DE", "FL", "GA", "HI", "ID", "IL",
             "IN", "IA", "AR", "KY", "LA", "ME",
             "MD", "MA", "MI", "MN", "MS", "MO",
             "MT", "NE", "NV", "NH", "NJ", "NM",
             "NY", "NC", "ND", "OH", "OK", "OR",
             "PA", "RI", "SC", "SD", "TN", "TX",
             "CA", "VT", "VA", "WA", "WV", "WI",
             "WY", "DC")
  state_name    <- c("Alabama",
             "Alaska", "Arizona", "Kansas",
             "Utah", "Colorado", "Connecticut",
             "Delaware", "Florida", "Georgia",
             "Hawaii", "Idaho", "Illinois",
             "Indiana", "Iowa", "Arkansas",
             "Kentucky", "Louisiana", "Maine",
             "Maryland", "Massachusetts", "Michigan",
             "Minnesota", "Mississippi", "Missouri",
             "Montana", "Nebraska", "Nevada",
             "New Hampshire", "New Jersey", "New Mexico",
             "New York", "North Carolina", "North Dakota",
             "Ohio", "Oklahoma", "Oregon",
             "Pennsylvania", "Rhode Island", "South Carolina",
             "South Dakota", "Tennessee", "Texas",
             "California", "Vermont", "Virginia",
             "Washington", "West Virginia", "Wisconsin",
             "Wyoming", "District of Columbia")

  US_states <- data.frame(abbreviation, state_name)

  US_states$abbreviation <- paste0('\\b' ,US_states$abbreviation, '\\b')

  return(toString(unique(US_states[stringr::str_detect(text, US_states$abbreviation) == TRUE,]$state_name)))

}


#' Detect and replace US states abbreviations to full state name.
#'
#' Finds US state abbreviations and replace them to the states full name within a text.
#'
#' This function was designed to replace US states abbreviations to its full name within a text.
#'
#' @param x free-text (affiliation field)
#'
#' @author Hugo Fitipaldi
#'
#'
#' @return
#' @export replace_us_state
#' @examples
#' replace_us_state("OH is a United Stated state.")
#'

replace_us_state <- function(x) {
  text <- x
  text <- stringi::stri_trans_general(str = text, id = "Latin-ASCII")
  text <- gsub('[[:digit:]]+', '', text)
  #text <- qdap::rm_stopwords(text, separate = FALSE)
  text <- gsub('[[:punct:] ]+',' ', text)
  text <- gsub('centre','', text)
  text <- gsub('university','', text)

  text_df <- data.frame(str_split(text, " "))
  names(text_df) <- "splitted"
  for (i in 1:nrow(text_df)) {
    text_df$new_text[i] <- affiliation::detecting_us_states(text_df$splitted[i])
  }
  text_df[text_df$new_text == "",]$new_text <- text_df[text_df$new_text == "",]$text_df$splitted

  return(paste(text_df$new_text, collapse = " "))

}
