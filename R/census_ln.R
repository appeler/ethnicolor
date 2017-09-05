#' Impute Race Based on Census Last Name Datasets
#' 
#' Appends data on percentage whites, blacks, asians, hispanics, 
#' mixed-race, and native americans with the last name from the 
#' 2000 and 2010 census datasets
#'
#' See more at: \url{https://github.com/soodoku/ethnicolor/tree/master/data-raw/census}
#' 
#' @param surname Required; vector of strings
#' 
#' @return Data frame with all the details that are in the census datasets
#' 
#' @examples 
#' census_ln(surname = "Smith")
#' top5 <- census_ln(c("Smith", "Johnson", "Williams", "Brown", "Jones"))
#' sum(top5$cs2010_count)
#' @export

census_ln <- function(surname = NULL){

  surname_upper <- toupper(surname)

  # Results data.frame
  surname_2000 <- cs2000[match(surname_upper, cs2000$name), ]
  surname_2010 <- cs2010[match(surname_upper, cs2010$name), ]

  names(surname_2000) <- paste0("cs2000_", names(surname_2000))
  names(surname_2010) <- paste0("cs2010_", names(surname_2010))

  cs_all <- merge(surname_2000, surname_2010, by.x = "cs2000_name",
                  by.y = "cs2010_name")
  cs_all$cs2000_name <- tolower(cs_all$cs2000_name)
  names(cs_all)[match("cs2000_name", names(cs_all))] <- "name"

  cs_all
}

#' Print a census_ln object
#'
#' @inheritParams census_ln
#' @param x The object to print.
#' @param ... Further arguments to passed on to other print methods.
#' 
#' @export
#' 

print.census_ln <- function(x, ...) {

  f <- getOption("census_ln.print")
  if (is.function(f)) {
    f(x, ...)
  } else {
    NextMethod()
  }
}

#' Last name classifier based on Florida Voter Registration Data
#' 
#' @param surname required; string; vector of surnames
#' @description Uses a validated ML model that uses bi- and tri-char tokens in the last name to predict probability that the person is white, black, asian, and hispanic. For more information see \url{https://github.com/soodoku/ethnicolor/tree/master/data-raw/fl_voter_reg}
#' @examples 
#' pred_fl_reg_ln(surname = "Smith")
#' @return data.frame with original list and predicted probability the person is white, Asian, black, and hispanic.
#' @export
pred_fl_reg_ln <- function(surname = NULL){
  return(surname)
}
