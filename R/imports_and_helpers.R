#' @import rstan
#' @importFrom stats "contr.sum" "contr.helmert" "contrasts" "contrasts<-" "cor" "median" "model.matrix" "pnorm" "sd" "terms"
#' @importFrom utils installed.packages
#' @importFrom lubridate "seconds_to_period"
NULL

.onAttach <- function(libname, pkgname) {
	packageStartupMessage('
[2018-02-16] ezStan changes:\n
- start_stan: "args" argument eliminated; see ?start_stan for new usage.
- start_stan: now permits running multiple sequential chains on each core (useful if you want more chains than available cores).
- watch_stan: now shows warnings and errors as they occur (requires loggr package to be installed via devtools::install_github("mike-lawrence/loggr")).
- watch_stan: now shows post-warmup divergences and iterations exceeding the max_treedepth.
- watch_stan: now plays a beep if the beepr package is installed.
- watch_stan: now has an argument "kill_on_divergence" that kills the sampling if a post-warmup divergence is encountered.
- collect_stan: now shows any warnings and errors encountered during sampling.
- build_stan: new function that attempts to build stan models in a separate process, thereby avoiding interface hangs in RStudio. Still a work in progess as scenarios where you encounter the "hash mismatch" warning will still cause a hang.
'
	)
}

str_rep = function(x, i) {
	paste(rep.int(x, i), collapse = "")
}
time_as_string = function(x) {
	tolower(lubridate::seconds_to_period(round(x)))
}
append_string = function(s,i,spacing,one_line_per_chain){
	if(one_line_per_chain){
		w = getOption("width")+1
		s = paste0(s,i,str_rep(' ',w-(nchar(i)%%w)+spacing))
	}else{
		s = paste0(s,i,str_rep(' ',spacing))
	}
	return(s)
}

