#' Retrieve and recognize affiliation and country of affiliation from PMC PubMed Central
#'
#' Creates a dictionary-like author-affiliation and identifies country based on affiliation free-text field
#'
#' This function was desinged to be applied to PubMed indexed studies published before 2014 (PubMed did not include all author affiliation information in their metadata) and for a few studies > 2014 with non-indexed affiliations.
#'
#' @param authors_name Authors list copied and pasted from PMC PubMed Central
#' @param affiliation_dict List of affiliation copied and pasted from PMC PubMed Central
#'
#' @author Hugo Fitipaldi
#'
#' @return This function returns a \code{data.frame} with the list of authors of a queried scientific publication and detected country of affiliations
#' @export auth_aff_dict
#'
#' @examples
#' authors_names <- "John Doe,1 Jane Doe,2,3"
#' authors_names <- gsub("([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-9][0-9])","\\1,",authors_names)
#' authors_name <- gsub(",",",",authors_names)
#' affiliation_dict <- "1 Affiliation ONE, Recife, Brazil\n2 Affiliation TWO, Kent, Ohio, United States\n3 Affiliation THREE, Malmo, Sweden"
#' auth_aff_dict(authors_names, affiliation_dict)


auth_aff_dict <- function (authors_names, affiliation_dict) {

  authors_names <- stringr::str_split(authors_names, ", ")
  authors_df <- data.frame(authors_names)
  names(authors_df) <- "Authors"
  authors_df$Affiliations <- stringr::str_match_all(authors_df$Authors,"[0-9]+")

  affiliation_dict <- stringr::str_split(affiliation_dict, "[.] [0-9]")
  affiliation_dict <- stringr::str_split(affiliation_dict, "\n[0-9]+ ")
  affiliation_dict <- data.frame(affiliation_dict)
  names(affiliation_dict) <- 'Affiliations'
  affiliation_dict$Keys <- 1:nrow(affiliation_dict)
  affiliation_dict <- affiliation_dict[,c(2,1)]
  affiliation_dict$Affiliations <- as.character(affiliation_dict$Affiliations)


  authors_df$Affiliations <- as.vector(authors_df$Affiliations)
  authors_df <- tidyr::separate(authors_df, col= Affiliations, sep = ',', into = toupper(letters[1:10]))
  authors_df[is.na(authors_df)] <- 0

  authors_df$A <- as.numeric(gsub("[^0-9]", "", authors_df$A))
  authors_df$B <- as.numeric(gsub("[^0-9]", "", authors_df$B))
  authors_df$C <- as.numeric(gsub("[^0-9]", "", authors_df$C))
  authors_df$D <- as.numeric(gsub("[^0-9]", "", authors_df$D))
  authors_df$E <- as.numeric(gsub("[^0-9]", "", authors_df$E))
  authors_df$F <- as.numeric(gsub("[^0-9]", "", authors_df$F))
  authors_df$G <- as.numeric(gsub("[^0-9]", "", authors_df$G))
  authors_df$H <- as.numeric(gsub("[^0-9]", "", authors_df$H))
  authors_df$I <- as.numeric(gsub("[^0-9]", "", authors_df$I))
  authors_df$J <- as.numeric(gsub("[^0-9]", "", authors_df$J))

  authors_df_sub <- authors_df[2:ncol(authors_df)]
  authors_df_sub <- data.frame(t(authors_df_sub))
  authors_df_sub$ROWSUM <- rowSums(authors_df_sub)
  authors_df_sub <- authors_df_sub[which(authors_df_sub$ROWSUM > 0),]
  authors_df_sub$ROWSUM <- NULL
  authors_df_sub <- data.frame(t(authors_df_sub))

  authors_df <- cbind(authors_df[,1], authors_df_sub)

  aff_hash <- hash::hash(keys = affiliation_dict$Keys, values = affiliation_dict$Affiliations)
  hash::.set(aff_hash, 0, 'NA')
  if("A" %in% colnames(authors_df)){
    authors_df$A <- hash::values(aff_hash, keys = authors_df$A)
  }
  if("B" %in% colnames(authors_df)){
    authors_df$B <- hash::values(aff_hash, keys = authors_df$B)
  }
  if("C" %in% colnames(authors_df)){
    authors_df$C <- hash::values(aff_hash, keys = authors_df$C)
  }
  if("D" %in% colnames(authors_df)){
    authors_df$D <- hash::values(aff_hash, keys = authors_df$D)
  }
  if("E" %in% colnames(authors_df)){
    authors_df$E <- hash::values(aff_hash, keys = authors_df$E)
  }
  if("F" %in% colnames(authors_df)){
    authors_df$F <- hash::values(aff_hash, keys = authors_df$F)
  }
  if("G" %in% colnames(authors_df)){
    authors_df$G <- hash::values(aff_hash, keys = authors_df$G)
  }
  if("H" %in% colnames(authors_df)){
    authors_df$H <- hash::values(aff_hash, keys = authors_df$H)
  }
  if("I" %in% colnames(authors_df)){
    authors_df$I <- hash::values(aff_hash, keys = authors_df$I)
  }
  if("J" %in% colnames(authors_df)){
    authors_df$J <- hash::values(aff_hash, keys = authors_df$J)
  }

  authors_df <- tidyr::unite(authors_df, col = 'Affiliations', 2:ncol(authors_df), sep = "_", remove = TRUE)
  names(authors_df) <- c('Authors', 'Affiliations')
  rownames(authors_df) <- 1:nrow(authors_df)

  authors_df$Affiliations <- gsub("_NA", "", authors_df$Affiliations)

  for (i in 1:nrow(authors_df)) (
    if (authors_df$Affiliations[i] == "NA")
      authors_df$Affiliations[i] <- NA
  )

  authors_df$to_delete <- "do not delete"
  for (i in 1:nrow(authors_df)) (
    if (is.na(authors_df$Affiliations[i]))
      authors_df$to_delete[i + 1] <- "Delete this row"
  )

  authors_df <- authors_df %>%
    tidyr::fill(Affiliations, .direction = "up")

  authors_df <- dplyr::filter(authors_df, to_delete != "Delete this row")

  authors_df$to_delete <- NULL

  Position <- 1:nrow(authors_df)
  authors_df <- cbind(Position, authors_df)

  authors_df <- authors_df %>%
    dplyr::mutate(Affiliations = strsplit(as.character(Affiliations), "_")) %>%
    tidyr::unnest(Affiliations)

  authors_df$countries_of_affiliation <- ""

  for(i in 1:nrow(authors_df)) {
    if (!is.na(authors_df$Affiliations[i]) & detecting_country_country(authors_df$Affiliations[i]) != ""){
      authors_df$countries_of_affiliation[i] <-  detecting_country_country(authors_df$Affiliations[i])}
  }

  for(i in 1:nrow(authors_df)) {
    if (authors_df$countries_of_affiliation[i] == "" & !is.na(authors_df$Affiliations[i]) & detecting_country_university(authors_df$Affiliations[i]) != ""){
      authors_df$countries_of_affiliation[i] <-  detecting_country_university(authors_df$Affiliations[i])}
  }

  for(i in 1:nrow(authors_df)) {
    if (authors_df$countries_of_affiliation[i] == "" & !is.na(authors_df$Affiliations[i]) & detecting_country_state(authors_df$Affiliations[i]) != ""){
      authors_df$countries_of_affiliation[i] <-  detecting_country_state(authors_df$Affiliations[i])}
  }

  authors_df <- authors_df %>%
    dplyr::group_by(Authors) %>%
    dplyr::mutate(Affiliations = paste0(Affiliations, collapse = "_"), countries_of_affiliation = paste0(countries_of_affiliation, collapse = "_")) %>%
    dplyr::slice(1L) %>%
    dplyr::arrange(Position)

  authors_df$Position <- NULL

  names(authors_df) <- c("author_fullname", "affiliation_freetext", "country_of_affiliation")

  authors_df$author_fullname <- gsub("[0-9]+", "", authors_df$author_fullname)
  authors_df$author_fullname <- gsub(",", "", authors_df$author_fullname)

  authors_df <- authors_df[,c(1,3,2)]

  return(authors_df)
}


