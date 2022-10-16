#' @importFrom magrittr "%>%"
#' @importFrom stringi stri_rand_strings
generate_hash <- function(size = 12, seed = Sys.time) {
  seed() %>%
    as.integer() %>%
    paste0(stri_rand_strings(1, size))
}