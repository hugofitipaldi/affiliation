
auth_aff_dict <- function (authors_list, affiliation_list) {

  authors_list <- gsub(" and ", ", ", authors_list)
  authors_list <- gsub(",\\*", replacement = "", authors_list)
  authors_list <- stringr::str_split(authors_list, ", ")
  authors_df <- data.frame(authors_list)
  names(authors_df) <- "Authors"
  authors_df$Affiliations <- stringr::str_match_all(authors_df$Authors,"[0-9]+")

  affiliation_list <- stringr::str_split(affiliation_list, "[.] [0-9]")
  affiliation_list <- stringr::str_split(affiliation_list, "\n[0-9]+ ")
  affiliation_list <- data.frame(affiliation_list)
  names(affiliation_list) <- 'Affiliations'
  affiliation_list$Keys <- 1:nrow(affiliation_list)
  affiliation_list <- affiliation_list[,c(2,1)]
  affiliation_list$Affiliations <- as.character(affiliation_list$Affiliations)

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

  aff_hash <- hash::hash(keys = affiliation_list$Keys, values = affiliation_list$Affiliations)
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
  dictionary_df <- authors_df

  #-------------------- To be continued ------------------------#

}


authors_list <- "Soo Heon Kwak,1 and Kyong Soo Park,1,2,3,*"
affiliation_list <- "1 Department of Internal Medicine, Seoul National University Hospital, Seoul, Korea
2 Department of Internal Medicine, Seoul National University College of Medicine, Seoul, Korea
3 Department of Molecular Medicine and Biopharmaceutical Sciences, Graduate School of Convergence Science and Technology, Seoul National University, Seoul, Korea"
