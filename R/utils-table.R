.define_table_rounding_commands <- function(x) {
  digits <- x[[.significantDigits]]
  c(
    ".columns <- vapply(tab, is.numeric, FUN.VALUE = logical(1))",
    sprintf("tab[, .columns] <- round(tab[, .columns], digits = %i)", digits)
  )
}
