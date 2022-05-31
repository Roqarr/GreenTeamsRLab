# DemeteR ----
# Written by Chris Snyder, MPH (Roqarr)
## Documentation ----
# Patches the Numform package to be compatible with kable, when using escapes
# Not addressed, due to note from package author:
# https://github.com/trinker/numform/issues/26
## Arguments ----
# na
## Returns ----
# na

# Numform Patch ----
## f_percent ----
# Patch f_percent for use with kableExtra where kable(escape=F)
# Allows for kableExtra formatting to be used (which requires escape characters to add extra headings)
# Adds appropriate number of leading escapes to be parsed by LaTeX engine
f_percent = function(x, digits = getOption("numformdigits"), less.than.replace = FALSE, kable.escape = TRUE, ...) {
  
  if (kable.escape == TRUE) {out = f_num(x, digits = digits, s="%", ...)}
  if (kable.escape == FALSE) {out = f_num(x, digits = digits, s = paste0("\\", "%"), ...)}
  
  if (isTRUE(less.than.replace)){
    if (is.null(digits)) digits = 1
    repl = replace_less_than(digits, percent = TRUE)
    out[x < repl[['prop_cut']][1] & x >= 0] = repl[['replacement']][1]
    out[x > repl[['prop_cut']][2] & x < 0] = repl[['replacement']][2]
  }
  
  out
}

## f_dollar ----
# Patch f_dollar for use with kableExtra where kable(escape=F)
# Allows for kableExtra formatting to be used (which requires escape characters to add extra headings)
# Adds appropriate number of leading escapes to be parsed by LaTeX engine
f_dollar = function(x, leading_zero = TRUE, digits = 2, kable.escape = TRUE, ...) {
  
  if (kable.escape == TRUE) {out = f_num(x, digits = digits, p="$", ...)}
  if (kable.escape == FALSE) {out = f_num(x, digits = digits, p = paste0("\\", "$"), ...)}
  
  gsub("^\\$\\.", "$0.", out)
  
}
