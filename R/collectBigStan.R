#' Collect the results from a Big Stan session
#'
#' @return An object of S4 class stanfit.
#' @export
#'
#' @examples
collectBigStan = function(){
	bigStanStuff = NULL
	load('bigStanTemp/bigStanStuff.rda')
	rdaList = list()
	for(i in bigStanStuff$doneList){
		load(bigStanStuff$rdaFileList[[i]])
		rdaList[[i]] = get(bigStanStuff$chainNameList[[i]])
	}
	return(sflist2stanfit(rdaList))
}
