#' @import rstan
#' @importFrom stats "contr.sum" "contr.helmert" "contrasts" "contrasts<-" "cor" "median" "model.matrix" "pnorm" "sd" "terms"
#' @importFrom utils installed.packages
#' @importFrom lubridate "seconds_to_period"
NULL

.onAttach <- function(libname, pkgname) {
	packageStartupMessage("[2017-10-31] Note from the ezStan developer:\nI've updated this package to have more consistent naming of functions and function arguments. Specifically, I've converted everything to snake_case and any functions with the format \"...BigStan()\" have changed to \"..._stan()\". See function help pages for updated function arguments.")
}

str_rep = function(x, i) {
	paste(rep.int(x, i), collapse = "")
}
time_as_string = function(x) {
	tolower(lubridate::seconds_to_period(round(x)))
}
append_string = function(s,i,spacing,one_line_per_chain){
	if(one_line_per_chain){
		w = getOption("width")
		s = paste0(s,i,str_rep(' ',w-nchar(i)+spacing))
	}else{
		s = paste0(s,i,str_rep(' ',spacing))
	}
	return(s)
}

