#' Delete temporary files created by a Big Stan session
#' @param reportAllClean A logical value indicating whether this function should print an "All Clean!" message when complete.
#' @return No value is returned
#' @export
#'
#' @examples
cleanBigStan = function(reportAllClean=T){
	if(dir.exists('bigStanTemp')){
		file.remove(list.files(path='bigStanTemp',all.files=T,full.names=T,recursive=T),recursive=T)
	}
	if(dir.exists('bigStanTemp/r')){
		file.remove('bigStanTemp/r')
	}
	if(dir.exists('bigStanTemp/samples')){
		file.remove('bigStanTemp/samples')
	}
	if(dir.exists('bigStanTemp/samples')){
		file.remove('bigStanTemp/samples')
	}
	if(dir.exists('bigStanTemp/stdout')){
		file.remove('bigStanTemp/stdout')
	}
	if(dir.exists('bigStanTemp/stderr')){
		file.remove('bigStanTemp/stderr')
	}
	if(dir.exists('bigStanTemp/rdas')){
		file.remove('bigStanTemp/rdas')
	}
	if(dir.exists('bigStanTemp')){
		file.remove('bigStanTemp')
	}
	if(dir.exists('bigStanTemp')){
		file.remove(list.files(path='bigStanTemp',all.files=T,full.names=T,recursive=T),recursive=T)
		file.remove('bigStanTemp')
	}
	if(reportAllClean){
		cat("\r","All clean!")
		utils::flush.console()
	}
	return(invisible(NULL))
}
