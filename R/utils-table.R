.define_table_rounding_commands <- function(x) {
  c(
    ".columns <- vapply(tab, is.numeric, FUN.VALUE = logical(1))",
    "tab[, .columns] <- round(tab[, .columns], digits = 3)"
  )
}
