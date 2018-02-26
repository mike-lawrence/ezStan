#' Start a Stan session
#'
#' @param data A list of objects expected by the model as data.
#' @param mod A stan model created by rstan::stan_model().
#' @param cores An integer value specifying the number of cores to run in parallel.
#' @param chains_per_core An integer value specifying the number of chain to run sequentially on each core
#' @param seed_start An integer specifying the seed to use for the first core. All subsequent cores will use seed values that increment seed_start.
#' @param ... Additional named arguments passed to rstan::sampling().
#' @return No value is returned.
#' @export
#'
#' @examples
start_stan = function(
	data
	, mod
	, cores = parallel::detectCores()/2
	, chains_per_core = 1
	, seed_start = 1
	, ...
){
	if(!('loggr' %in% installed.packages())){
		warning(
			'IMPORTANT: ezStan now relies on the loggr package to show erros an warnings from Stan. If you\'re seeing this message, you need to install loggr by running:
			devtools::install_github("mike-lawrence/loggr")'
		)
	}
	if(dir.exists('stan_temp')){
		clean_stan(report_all_clean=F)
	}
	dir.create('stan_temp')
	stan_args = list(...)
	if('iter' %in% names(stan_args)){
		iter = stan_args[['iter']]
	}else{
		iter = 2e3
	}
	save(
		cores
		, chains_per_core
		, seed_start
		, iter
		, stan_args
		, file = 'stan_temp/start_stan_args.rda'
	)
	save(
		data
		, mod
		, file = 'stan_temp/data.rda'
	)
	cat(
		"chain_num = as.numeric(commandArgs(trailingOnly=TRUE)[1])
		load(file='stan_temp/start_stan_args.rda')
		#loads the following objects:
		#	cores
		#	chains_per_core
		#	seed_start
		#	stan_args

		chain_name = sprintf(paste0('chain%0',ceiling(log10(cores)),'d'),chain_num)
		sample_file_name = paste0('stan_temp/samples_',chain_name,'.txt')
		rda_file_name = paste0('stan_temp/rdas_',chain_name,'.rda')
		log_file_name = paste0('stan_temp/logs_',chain_name,'.log')

		if('loggr' %in% installed.packages()){
			suppressMessages(library(loggr,quietly=T))
			my_formatter <- function(event) event$message
			log_file(log_file_name,.formatter = my_formatter,subscriptions=c('message', 'warning','stop'))
		}

		load('stan_temp/data.rda')
		suppressMessages(library(rstan,quietly=T))

		stan_args$refresh = 0
		stan_args$chains = 1
		stan_args$cores = 1
		stan_args$iter = iter
		stan_args$data = data
		stan_args$object = mod
		stan_args$seed = seed_start + chain_num - 1
		stan_args$sample_file = sample_file_name

		post = NULL
		while(is.null(post)){
		try(post <- do.call(rstan::sampling,stan_args))
		}
		save(post,file=rda_file_name)
		#file.remove(sample_file_name)

		next_chain_num = chain_num + cores
		if( next_chain_num<=(chains_per_core*cores) ){
		next_chain_name = sprintf(paste0('chain%0',ceiling(log10(cores)),'d'),next_chain_num)
		stdout_file = paste0('stan_temp/stdout_',next_chain_name,'.txt')
		stderr_file = paste0('stan_temp/stderr_',next_chain_name,'.txt')
		system2(
		command = 'Rscript'
		, args = c('--vanilla','stan_temp/do_chain.R',next_chain_num)
		, stdout = stdout_file
		, stderr = stderr_file
		, wait = FALSE
		)

		}
		"
		, file = 'stan_temp/do_chain.R'
	)
	cat("\nStarting chains...")
	for(i in 1:cores){
		chain_name = sprintf(paste0("chain%0",ceiling(log10(cores)),"d"),i)
		stdout_file = paste0('stan_temp/stdout_',chain_name,'.txt')
		stderr_file = paste0('stan_temp/stderr_',chain_name,'.txt')
		system2(
			command = "Rscript"
			, args = c('--vanilla','stan_temp/do_chain.R',i)
			, stdout = stdout_file
			, stderr = stderr_file
			, wait = FALSE
		)
	}
	start_time = Sys.time()
	save(start_time,file='stan_temp/start_time.rda')
	cat("\nChains started. Run watch_stan() to watch progress")
	return(invisible(NULL))
}
