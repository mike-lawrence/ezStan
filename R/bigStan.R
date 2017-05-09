#' @import rstan
#' @importFrom lubridate "seconds_to_period"
NULL

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
	bigStanStuff$rdaFileList = list()
	bigStanStuff$stdoutFileList = list()
	bigStanStuff$stderrFileList = list()
	bigStanStuff$progressList = list()
	bigStanStuff$samplesList = list()
	for(i in 1:cores){
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

#' Watch the progress of a Big Stan session
#'
#' @param updateInterval Number of seconds to wait between updates.
#' @param one_line_per_chain A logical value specifying whether the progress for each chain should be printed on seperate lines.
#' @param spacing An integer value specifying the number of extra spaces to add to the end of each chain's progress string. Can be used to fix misalignment when one_line_per_chain is TRUE.
#' @return No value is returned.
#' @export
#'
#' @examples
watchBigStan = function(updateInterval=1,one_line_per_chain=TRUE,spacing=3){
	bigStanStuff = NULL
	load('bigStanTemp/bigStanStuff.rda')
	bigStanStuff$numDone = length(list.files(path="bigStanTemp/rdas"))
	while(bigStanStuff$numDone<bigStanStuff$cores){
		chains_with_stderr = c()
		bigStanStuff$numDone = length(list.files(path="bigStanTemp/rdas"))
		Sys.sleep(updateInterval)
		for(i in 1:bigStanStuff$cores){
			if(bigStanStuff$progressList[[i]]<bigStanStuff$iter){ #only check this chain if it isn't done
				if(file.exists(bigStanStuff$stderrFileList[[i]])){ #check if the stderr file exists
					temp = readLines(bigStanStuff$stderrFileList[[i]])
					if(length(temp)>0){ #stderr file has contents
						chains_with_stderr = c(chains_with_stderr,i)
					}
				}
				if(file.exists(bigStanStuff$sampleFile[[i]])){ #only try reading the sample file if it exists
					a = readLines(bigStanStuff$sampleFile[[i]])
					a = a[substr(a,1,1)!="#"]
					a = a[substr(a,1,4)!="lp__"]
					a = a[a!='']
					old_progress = bigStanStuff$progressList[[i]]
					bigStanStuff$progressList[[i]] = length(a)
					# if(length(a)>warmup){
					# 	old_warn = options("warn")
					# 	options(warn=-1)
					# 	samplesList[[i]] = rstan::read_stan_csv(sampleFile)
					# 	options(warn=old_warn$warn)
					# }
					if(bigStanStuff$progressList[[i]]!=old_progress){
						save(bigStanStuff,file='bigStanTemp/bigStanStuff.rda')
					}
				}
			}
			minDone = min(unlist(bigStanStuff$progressList))
			timeElapsed = difftime(Sys.time(), bigStanStuff$startTime,unit='secs')
			timeLeft = "?"
			if(is.finite(minDone)){
				if(minDone>0){
					timeLeft = timeAsString(timeElapsed/minDone*(bigStanStuff$iter-minDone))
				}
			}
			temp = paste0(unlist(bigStanStuff$chainNameList),': ',unlist(bigStanStuff$progressList),'/',bigStanStuff$iter)
			updateTextToPrint = '\r'
			for(i in temp){
				updateTextToPrint = appendString(updateTextToPrint,i,spacing,one_line_per_chain)
			}
			temp = paste0('Chains complete: ',bigStanStuff$numDone,'/',bigStanStuff$cores)
			updateTextToPrint = appendString(updateTextToPrint,temp,spacing,one_line_per_chain)
			temp = paste0('Estimated time remaining: ',timeLeft)
			updateTextToPrint = appendString(updateTextToPrint,temp,spacing,one_line_per_chain)
			if(length(chains_with_stderr)>0){
				temp = paste0('chains with errors: ',paste(chains_with_stderr,collapse=', '))
				updateTextToPrint = appendString(updateTextToPrint,temp,spacing,one_line_per_chain)
			}
			cat(updateTextToPrint)
			utils::flush.console()
		}
	}
	chains_with_stderr = c()
	for(i in 1:bigStanStuff$cores){
		if(file.exists(bigStanStuff$stderrFileList[[i]])){ #check if the stderr file exists
			temp = readLines(bigStanStuff$stderrFileList[[i]])
			if(length(temp)>0){ #stderr file has contents
				chains_with_stderr = c(chains_with_stderr,i)
			}
		}
	}
	updateTextToPrint = '\n'
	timeElapsed = difftime(Sys.time(), bigStanStuff$startTime,unit='secs')
	temp = paste0('All done! Elapsed time: ',timeAsString(timeElapsed))
	updateTextToPrint = appendString(updateTextToPrint,temp,spacing,one_line_per_chain)
	if(length(chains_with_stderr)>0){
		temp = paste0('chains with messages from Stan: ',paste(chains_with_stderr,collapse=', '))
		updateTextToPrint = appendString(updateTextToPrint,temp,spacing,one_line_per_chain)
	}
	cat(updateTextToPrint)
	utils::flush.console()
	return(invisible(NULL))
}

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
	for(i in 1:bigStanStuff$cores){
		load(bigStanStuff$rdaFileList[[i]])
		rdaList[[i]] = get(bigStanStuff$chainNameList[[i]])
	}
	return(sflist2stanfit(rdaList))
}

#' Terminate an errant Big Stan session
#'
#' @return No value is returned
#' @export
#'
#' @examples
killBigStan = function(){
	system2(
		command = "killall"
		, args = "R"
	)
}


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

str_rep = function(x, i) {
	paste(rep.int(x, i), collapse = "")
}
timeAsString = function(x) {
	tolower(lubridate::seconds_to_period(round(x)))
}
appendString = function(s,i,spacing,one_line_per_chain){
	if(one_line_per_chain){
		w = getOption("width")
		s = paste0(s,i,str_rep(' ',w-nchar(i)+spacing))
	}else{
		s = paste0(s,i,str_rep(' ',spacing))
	}
	return(s)
}

