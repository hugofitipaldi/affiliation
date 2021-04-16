#' Retrieve and recognize affiliation and country of affiliation from PubMed
#'
#' Identifies country based on affiliation free text field
#'
#' This function was designed to
#'
#' @param query selects which data set from CSSS you want to download: "national" (default), "county" or "postcode"
#' @param email
#' @param format.long
#'
#' @author Hugo Fitipaldi
#'
#' @return This function returns a \code{data.frame}
#' @export
#'
#' @examples
#' get_affiliations(query = '30237159')
#' get_affiliations(query = '30237159', format.long = TRUE)
#'

get_affiliations <- function(query, email = NULL, format.long = FALSE) {

  #get the xml of the articles
  doc <- easyPubMed::get_pubmed_ids(query)
  doc <- easyPubMed::fetch_pubmed_data(doc)
  doc <- XML::xmlParse(doc)
  #get the authors
  authors <- XML::getNodeSet(doc,"//Author")
  #turning the xml into a dataframe
  au <- list()
  for(i in 1:length(authors)){
    au[[i]] <- data.frame(XML::xmlToList(authors[[i]]))
  }
  author.aff <- data.table::rbindlist(au, fill = TRUE)

  # Coliding affiliations
  col_matches_aff <- dplyr::select(author.aff, starts_with('Affiliation'))
  pubmed_df <- cbind(author.aff[,1:3], col_matches_aff)
  pubmed_df <- pubmed_df[!duplicated(pubmed_df),]
  Position <- 1:nrow(pubmed_df)
  pubmed_df <- cbind(Position, pubmed_df)

  pubmed_df_original <- tidyr::unite(pubmed_df, col = 'Affiliations', 5:ncol(pubmed_df), sep = "_", remove = TRUE)

  Original_Affiliation <- pubmed_df_original$Affiliations

  author.aff2 <- pubmed_df
  data_long <- tidyr::gather(author.aff2, aff_number, Affiliation, 5:ncol(author.aff2), factor_key=TRUE)

  data_long <- data_long[!duplicated(data_long),]
  Original_Affiliation_long <- data_long$Affiliation

  if (!is.null(email)){

    for(i in 1:nrow(data_long)) {
      if (!is.na(data_long$Affiliation[i]) & !is.na(affiliation:::geocode_nominatim(data_long$Affiliation[i], email = email)$country_match)){
        data_long$Affiliation[i] <-  affiliation:::geocode_nominatim(data_long$Affiliation[i], email = email)$country_match}
    }

    for(i in 1:nrow(data_long)) {
      if (!is.na(data_long$Affiliation[i]) & affiliation:::detecting_country_country(data_long$Affiliation[i]) != ""){
        data_long$Affiliation[i] <-  affiliation:::detecting_country_country(data_long$Affiliation[i])}
    }

    for(i in 1:nrow(data_long)) {
      if (!is.na(data_long$Affiliation[i]) & affiliation:::detecting_country_university(data_long$Affiliation[i]) != ""){
        data_long$Affiliation[i] <-  affiliation:::detecting_country_university(data_long$Affiliation[i])}
    }

    for(i in 1:nrow(data_long)) {
      if (!is.na(data_long$Affiliation[i]) & affiliation:::detecting_country_state(data_long$Affiliation[i]) != ""){
        data_long$Affiliation[i] <-  affiliation:::detecting_country_state(data_long$Affiliation[i])}
    }

    data_wide <- tidyr::spread(data_long, aff_number, Affiliation)
    pubmed_df <- data_wide
    pubmed_df$Position <- NULL

    pubmed_df <- tidyr::unite(pubmed_df, col = 'Affiliations', 4:ncol(pubmed_df), sep = "_", remove = TRUE)
    pubmed_df$Original_Affiliation <- Original_Affiliation
    pubmed_df$Initials <- NULL
    names(pubmed_df) <- c("author_lastname", "author_firstname", "country_of_affiliation", "affiliation_freetext")

    data_long$Original_Affiliation <- Original_Affiliation_long
    data_long <- dplyr::filter(data_long, !is.na(Affiliation))
    data_long$Initials <- NULL
    data_long$aff_number <- NULL
    names(data_long) <- c("author_position", "author_lastname", "author_firstname", "country_of_affiliation", "affiliation_freetext")

    if (format.long == FALSE) {
      return(pubmed_df)
      } else {
        return(data_long)
        }

    } else {

      for(i in 1:nrow(data_long)) {
        if (!is.na(data_long$Affiliation[i]) & affiliation:::detecting_country_country(data_long$Affiliation[i]) != ""){
          data_long$Affiliation[i] <-  affiliation:::detecting_country_country(data_long$Affiliation[i])}
        }

      for(i in 1:nrow(data_long)) {
        if (!is.na(data_long$Affiliation[i]) & affiliation:::detecting_country_university(data_long$Affiliation[i]) != ""){
          data_long$Affiliation[i] <-  affiliation:::detecting_country_university(data_long$Affiliation[i])}
        }

      for(i in 1:nrow(data_long)) {
        if (!is.na(data_long$Affiliation[i]) & affiliation:::detecting_country_state(data_long$Affiliation[i]) != ""){
          data_long$Affiliation[i] <-  affiliation:::detecting_country_state(data_long$Affiliation[i])}
        }

      data_wide <- tidyr::spread(data_long, aff_number, Affiliation)
      pubmed_df <- data_wide
      pubmed_df$Position <- NULL

      pubmed_df <- tidyr::unite(pubmed_df, col = 'Affiliations', 4:ncol(pubmed_df), sep = "_", remove = TRUE)
      pubmed_df$Original_Affiliation <- Original_Affiliation
      pubmed_df$Initials <- NULL
      names(pubmed_df) <- c("author_lastname", "author_firstname", "country_of_affiliation", "affiliation_freetext")

      data_long$Original_Affiliation <- Original_Affiliation_long
      data_long <- dplyr::filter(data_long, !is.na(Affiliation))
      data_long$Initials <- NULL
      data_long$aff_number <- NULL
      names(data_long) <- c("author_position", "author_lastname", "author_firstname", "country_of_affiliation", "affiliation_freetext")

      if (format.long == FALSE) {
        return(pubmed_df)
        } else {
          return(data_long)
        }
    }
  }

