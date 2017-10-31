#' Collect the results from a Stan session
#'
#' @return An object of S4 class stanfit.
#' @export
#'
#' @examples
collect_stan = function(){
	stan_stuff = NULL
	load('stan_temp/stan_stuff.rda')
	rda_list = list()
	j = 1
	for(i in 1:stan_stuff$cores){
		if(file.exists(stan_stuff$rda_fileList[[i]])){
			load(stan_stuff$rda_fileList[[i]])
			rda_list[[j]] = get(stan_stuff$chain_name_list[[i]])
			j = j + 1
		}
	}
	return(sflist2stanfit(rda_list))
}
