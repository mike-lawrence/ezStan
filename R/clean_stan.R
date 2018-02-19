#' Delete temporary files created by a Stan session
#' @param report_all_clean A logical value indicating whether this function should print an "All Clean!" message when complete.
#' @return No value is returned
#' @export
#'
#' @examples
clean_stan = function(report_all_clean=T){
	if(dir.exists('stan_temp')){
		file.remove(list.files(path='stan_temp',all.files=T,full.names=T,recursive=T),recursive=T)
		file.remove('stan_temp')
	}
	if(report_all_clean){
		cat("\r","All clean!")
		utils::flush.console()
	}
	return(invisible(NULL))
}
