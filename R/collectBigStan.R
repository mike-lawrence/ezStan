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
	j = 1
	for(i in 1:bigStanStuff$cores){
		if(file.exists(bigStanStuff$rdaFileList[[i]])){
			load(bigStanStuff$rdaFileList[[i]])
			rdaList[[j]] = get(bigStanStuff$chainNameList[[i]])
			j = j + 1
		}
	}
	return(sflist2stanfit(rdaList))
}
