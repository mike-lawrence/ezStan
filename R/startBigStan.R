#usage:
# #compile the model using rstan::stan_model:
# mod = rstan::stan_model('my_model.stan')
# #start the chains:
# startBigStan(
# 	stanMod = mod
# 	, stanData = data_for_stan
# )
# #watch their progress:
# watchBigStan()
# #when done, get the samples
# fromStan = collectBigStan()
# #delete temporary files
# cleanBigStan()
# #if necessary, kill all cores in broken bigStan run:
# killBigStan()

#' Start a Big Stan session
#'
#' @param stanMod A stan model created by rstan::stan_model().
#' @param stanData A list of objects expected by the model as data.
#' @param iter An integer value specifying the number of iterations to run per core.
#' @param cores An integer value specifying the number of cores to run in parallel.
#' @param stanArgs A string containing code to append inside the call to rstan::sampling() to specify non-default values for its arguments. For example, the string "pars='mu',control=list(adapt_delta=.9)" would only keep samples for the "mu" parameter and set the adapt_delta value to .9.
#' @param seedStart An integer specifying the seed to use for the first core. All subsequent cores will use seed values that increment seedStart.
#' @param warmup An integer value, smaller than `iter` that specifies the number of iterations to run as warmup.
#' @return No value is returned.
#' @export
#'
#' @examples
startBigStan = function(
	stanMod
	, stanData
	, iter = 2e3
	, cores = parallel::detectCores()
	, stanArgs = NULL
	, seedStart = 1
	, warmup = NULL
){
	if(dir.exists('bigStanTemp')){
		cleanBigStan(reportAllClean=F)
	}
	dir.create('bigStanTemp')
	if(is.null(warmup)){
		warmup = iter/2
	}
	cat("\nStarting chains...")
	save(stanData,stanMod,file='bigStanTemp/stanData.rda')
	if(!dir.exists('bigStanTemp/r')){
		dir.create('bigStanTemp/r')
	}
	if(!dir.exists('bigStanTemp/samples')){
		dir.create('bigStanTemp/samples')
	}
	if(!dir.exists('bigStanTemp/samples')){
		dir.create('bigStanTemp/samples')
	}
	if(!dir.exists('bigStanTemp/stdout')){
		dir.create('bigStanTemp/stdout')
	}
	if(!dir.exists('bigStanTemp/stderr')){
		dir.create('bigStanTemp/stderr')
	}
	if(!dir.exists('bigStanTemp/rdas')){
		dir.create('bigStanTemp/rdas')
	}
	bigStanStuff = list(cores=cores,iter=iter,warmup=warmup)
	bigStanStuff$rFileList = list()
	bigStanStuff$chainNameList = list()
	bigStanStuff$sampleFileList = list()
	bigStanStuff$sampleFileSizeList = list()
	bigStanStuff$rdaFileList = list()
	bigStanStuff$stdoutFileList = list()
	bigStanStuff$stderrFileList = list()
	bigStanStuff$progressList = list()
	bigStanStuff$samplesList = list()
	for(i in 1:cores){
		bigStanStuff$sampleFileSizeList[[i]] = 0
		bigStanStuff$progressList[[i]] = 0
		bigStanStuff$samplesList[[i]] = NULL
		bigStanStuff$rFileList[[i]] = paste0('bigStanTemp/r/',i,'.r')
		bigStanStuff$chainNameList[[i]] = sprintf(paste0("chain%0",ceiling(log10(cores)),"d"),i)
		bigStanStuff$sampleFileList[[i]] = paste0('bigStanTemp/samples/',bigStanStuff$chainNameList[[i]],'.txt')
		bigStanStuff$rdaFileList[[i]] = paste0('bigStanTemp/rdas/',bigStanStuff$chainNameList[[i]],'.rda')
		bigStanStuff$stdoutFileList[[i]] = paste0('bigStanTemp/stdout/',bigStanStuff$chainNameList[[i]],'.txt')
		bigStanStuff$stderrFileList[[i]] = paste0('bigStanTemp/stderr/',bigStanStuff$chainNameList[[i]],'.txt')
		cat(
			paste0('seed = ',seedStart-1+i)
			, "\n"
			, paste0('iter = ',iter)
			, "\n"
			, paste0('warmup = ',warmup)
			, "\n"
			, "suppressMessages(library(rstan,quietly=T))"
			, "\n"
			, 'load("bigStanTemp/stanData.rda")'
			, "\n"
			, bigStanStuff$chainNameList[[i]]
			, " = NULL"
			, "\n"
			, "while(is.null("
			, bigStanStuff$chainNameList[[i]]
			, ")){"
			, "\n"
			, "try("
			, bigStanStuff$chainNameList[[i]]
			, "<-rstan::sampling(
			object = stanMod
			, data = stanData
			, seed = seed
			, iter = iter
			, warmup = warmup
			, refresh = 0
			, chains = 1
			, cores = 1
			, "
			, stanArgs
			, ", sample_file = '"
			, bigStanStuff$sampleFileList[[i]]
			, "'\n))}"
			, "\n"
			, "save(",bigStanStuff$chainNameList[[i]],",file='",bigStanStuff$rdaFileList[[i]],"')"
			, "\n"
			, "file.remove('",bigStanStuff$sampleFileList[[i]],"')"
			, "\n"
			, sep = ''
			, file = bigStanStuff$rFileList[[i]]
			, append = FALSE
		)
		system2(
			command = "Rscript"
			, args = c('--vanilla',bigStanStuff$rFileList[[i]])
			, stdout = bigStanStuff$stdoutFileList[[i]]
			, stderr = bigStanStuff$stderrFileList[[i]]
			, wait = FALSE
		)
		Sys.sleep(.1)
	}
	bigStanStuff$startTime = Sys.time()
	save(bigStanStuff,file='bigStanTemp/bigStanStuff.rda')
	cat("\nChains started. Run watchBigStan() to watch progress")
	return(invisible(NULL))
}
