#' Collect the results from a Stan session
#'
#' @return An object of S4 class stanfit.
#' @export
#'
#' @examples
collect_stan = function(){
	#pre-defining objects we'll get from load() to avoid package build warnings
	cores = NULL
	chains_per_core = NULL
	seed_start = NULL
	iter = NULL
	stan_args = NULL
	load('stan_temp/start_stan_args.rda')
	#loads the following objects:
	#	cores
	#	chains_per_core
	#	seed_start
	#	iter
	#	stan_args
	num_chains = cores*chains_per_core
	post_list = list()
	j = 1
	for(this_chain in 1:num_chains){
		chain_name = sprintf(paste0('chain%0',ceiling(log10(cores)),'d'),this_chain)
		log_file = paste0('stan_temp/logs_',chain_name,'.log')
		if(file.exists(log_file)){ #check if the log file exists
			all_lines = readLines(log_file)
			if(length(all_lines)>0){ #log file has contents
				cat(paste0('[', chain_name, ':] '))
				cat('\n')
				for(line in all_lines){
					cat(line)
					cat('\n')
				}
				cat('\n')
			}
		}
		rda_file = paste0('stan_temp/rdas_',chain_name,'.rda')
		if(file.exists(rda_file)){
			load(rda_file)
			post_list[[j]] = get('post')
			names(post_list)[j] = chain_name
			j = j + 1
		}
	}
	return(sflist2stanfit(post_list))
}
