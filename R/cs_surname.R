#' Impute Race Based on Census Last Name Datasets
#' 
#' Includes data from 2000 and 2010 census datasets
#'
#' 
#' @param surname Required; vector of strings
#' 
#' @return Data frame with all the details that are in the census datasets
#' 
#' @export
#' 
#' @examples \dontrun{
#' cs_surname(surname="Smith")
#' }

cs_surname <- function(surname = NULL){
		
	surname_upper <- toupper(surname)

	# Results data.frame 
	surname_2000 <- cs2000[match(surname_upper, cs2000$name),]
	surname_2010 <- cs2010[match(surname_upper, cs2010$name),]

	names(surname_2000) <- paste0("cs2000_", names(surname_2000))
	names(surname_2010) <- paste0("cs2010_", names(surname_2010))

	cs_all <- merge(surname_2000, surname_2010, by.x="cs2000_name", by.y="cs2010_name")
	cs_all$cs2000_name <- tolower(cs_all$cs2000_name)
	names(cs_all)[match("cs2000_name", names(cs_all))] <- "name"

	return(cs_all)
}
