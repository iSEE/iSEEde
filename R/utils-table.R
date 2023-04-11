.define_table_rounding_commands <- function(x) {
  if (!x[[.roundDigits]]) {
    return(NULL)
  }
  
  digits <- x[[.significantDigits]]
  
  cmds <- c(
    ".columns <- vapply(tab, is.numeric, FUN.VALUE = logical(1))",
    sprintf("tab[, .columns] <- round(tab[, .columns], digits = %i)", digits)
  )
  
  cmds
}
