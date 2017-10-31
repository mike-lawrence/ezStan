#usage:
# #compile the model using rstan::stan_model:
# mod = rstan::stan_model('my_model.stan')
# #start the chains:
# start_stan(
# 	mod = mod
# 	, data = data_for_stan
# )
# #watch their progress:
# watch_stan()
# #when done, get the samples
# fromStan = collect_stan()
# #delete temporary files
# clean_stan()
# #if necessary, kill all cores in broken _stan run:
# kill_stan()

#' Start a Stan session
#'
#' @param data A list of objects expected by the model as data.
#' @param mod A stan model created by rstan::stan_model().
#' @param iter An integer value specifying the number of iterations to run per core.
#' @param cores An integer value specifying the number of cores to run in parallel.
#' @param args A string containing code to append inside the call to rstan::sampling() to specify non-default values for its arguments. For example, the string "pars='mu',control=list(adapt_delta=.9)" would only keep samples for the "mu" parameter and set the adapt_delta value to .9.
#' @param seed_start An integer specifying the seed to use for the first core. All subsequent cores will use seed values that increment seed_start.
#' @param warmup An integer value, smaller than `iter` that specifies the number of iterations to run as warmup.
#' @return No value is returned.
#' @export
#'
#' @examples
start_stan = function(
	data
	, mod
	, iter = 2e3
	, cores = parallel::detectCores()
	, args = NULL
	, seed_start = 1
	, warmup = NULL
){
	if(dir.exists('stan_temp')){
		clean_stan(report_all_clean=F)
	}
	dir.create('stan_temp')
	if(is.null(warmup)){
		warmup = iter/2
	}
	cat("\nStarting chains...")
	save(data,mod,file='stan_temp/data.rda')
	if(!dir.exists('stan_temp/r')){
		dir.create('stan_temp/r')
	}
	if(!dir.exists('stan_temp/samples')){
		dir.create('stan_temp/samples')
	}
	if(!dir.exists('stan_temp/samples')){
		dir.create('stan_temp/samples')
	}
	if(!dir.exists('stan_temp/stdout')){
		dir.create('stan_temp/stdout')
	}
	if(!dir.exists('stan_temp/stderr')){
		dir.create('stan_temp/stderr')
	}
	if(!dir.exists('stan_temp/rdas')){
		dir.create('stan_temp/rdas')
	}
	stan_stuff = list(cores=cores,iter=iter,warmup=warmup)
	stan_stuff$rFileList = list()
	stan_stuff$chain_name_list = list()
	stan_stuff$sample_file_list = list()
	stan_stuff$sample_file_size_list = list()
	stan_stuff$rda_fileList = list()
	stan_stuff$stdoutFileList = list()
	stan_stuff$stderr_file_list = list()
	stan_stuff$progress_list = list()
	stan_stuff$samplesList = list()
	for(i in 1:cores){
		stan_stuff$sample_file_size_list[[i]] = 0
		stan_stuff$progress_list[[i]] = 0
		stan_stuff$samplesList[[i]] = NULL
		stan_stuff$rFileList[[i]] = paste0('stan_temp/r/',i,'.r')
		stan_stuff$chain_name_list[[i]] = sprintf(paste0("chain%0",ceiling(log10(cores)),"d"),i)
		stan_stuff$sample_file_list[[i]] = paste0('stan_temp/samples/',stan_stuff$chain_name_list[[i]],'.txt')
		stan_stuff$rda_fileList[[i]] = paste0('stan_temp/rdas/',stan_stuff$chain_name_list[[i]],'.rda')
		stan_stuff$stdoutFileList[[i]] = paste0('stan_temp/stdout/',stan_stuff$chain_name_list[[i]],'.txt')
		stan_stuff$stderr_file_list[[i]] = paste0('stan_temp/stderr/',stan_stuff$chain_name_list[[i]],'.txt')
		cat(
			paste0('seed = ',seed_start-1+i)
			, "\n"
			, paste0('iter = ',iter)
			, "\n"
			, paste0('warmup = ',warmup)
			, "\n"
			, "suppressMessages(library(rstan,quietly=T))"
			, "\n"
			, 'load("stan_temp/data.rda")'
			, "\n"
			, stan_stuff$chain_name_list[[i]]
			, " = NULL"
			, "\n"
			, "while(is.null("
			, stan_stuff$chain_name_list[[i]]
			, ")){"
			, "\n"
			, "try("
			, stan_stuff$chain_name_list[[i]]
			, "<-rstan::sampling(
			object = mod
			, data = data
			, seed = seed
			, iter = iter
			, warmup = warmup
			, refresh = 0
			, chains = 1
			, cores = 1
			, "
			, args
			, ", sample_file = '"
			, stan_stuff$sample_file_list[[i]]
			, "'\n))}"
			, "\n"
			, "save(",stan_stuff$chain_name_list[[i]],",file='",stan_stuff$rda_fileList[[i]],"')"
			, "\n"
			, "file.remove('",stan_stuff$sample_file_list[[i]],"')"
			, "\n"
			, sep = ''
			, file = stan_stuff$rFileList[[i]]
			, append = FALSE
		)
		system2(
			command = "Rscript"
			, args = c('--vanilla',stan_stuff$rFileList[[i]])
			, stdout = stan_stuff$stdoutFileList[[i]]
			, stderr = stan_stuff$stderr_file_list[[i]]
			, wait = FALSE
		)
		Sys.sleep(.1)
	}
	stan_stuff$start_time = Sys.time()
	save(stan_stuff,file='stan_temp/stan_stuff.rda')
	cat("\nChains started. Run watch_stan() to watch progress")
	return(invisible(NULL))
}
