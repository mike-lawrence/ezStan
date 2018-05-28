
#' Build a Stan model without locking R/RStudio
#'
#' @param file A character string specifying the path to a Stan file to build.
#' @return an object of the class rstan::stan_model.
#' @export
#'
#' @examples
build_stan = function(
	file
){
	if(!('loggr' %in% installed.packages())){
		warning(
			'IMPORTANT: ezStan now relies on the loggr package to show erros an warnings from Stan. If you\'re seeing this message, you need to install loggr by running:
			devtools::install_github("smbache/loggr")'
		)
	}
	if(!dir.exists('stan_temp')){
		dir.create('stan_temp')
	}
	unlink('stan_temp/build*')
	script_file = 'stan_temp/build.R'#tempfile()
	stderr_file = 'stan_temp/build.stderr'#tempfile()
	stdout_file = 'stan_temp/build.stdout'#tempfile()
	log_file = 'stan_temp/build.log'#tempfile()
	flag_file = 'stan_temp/build.flag'#tempfile()
	cat(
		"
		args = commandArgs(trailingOnly=TRUE)
		mod_file = args[1]
		log_file = args[2]
		flag_file = args[3]
		if('loggr' %in% installed.packages()){
			suppressMessages(library(loggr,quietly=T))
			my_formatter <- function(event) event$message
			log_file(log_file,.formatter = my_formatter)#,subscriptions=c('message', 'warning','stop'))
		}
		suppressMessages(library(rstan,quietly=T))
		rstan_options(auto_write = TRUE)
		suppressMessages(mod <- rstan::stan_model(mod_file))
		save(flag_file,file=flag_file)
		"
		, file = script_file
	)
	cat('\rBuilding ...')
	system2(
		command = "Rscript"
		, args = c('--vanilla',script_file,file,log_file,flag_file)
		, stdout = NULL#stdout_file
		, stderr = NULL#stderr_file
		, wait = FALSE
	)
	done = FALSE
	loops = 0
	while(!done){
		loops = loops+1
		Sys.sleep(1)
		cat('\rBuilding ...',rep('.',times=loops),sep='')
		if(file.exists(flag_file)){
			done = T
		}
		if(file.exists(log_file)){
			if(length(scan(log_file,what='character',sep='\n',quiet=T))>0){
				done = T
			}
		}
	}
	cat('\r',rep(' ',times=loops+12),sep='')
	#cat('\nBuild complete.')
	# cat('\r')
	# if(file.exists(log_file)){
	# 	temp = scan(log_file,what='character',quiet=T,sep='\n')
	# 	if(length(temp)>0){
	# 		cat('Messages from stan_model:',temp,'\n',sep='\n')
	# 	}
	# }
	cat('\r')
	# if(file.exists(stderr_file)){
	# 	temp = scan(stderr_file,what='character',quiet=T)
	# 	if(length(temp)>0){
	# 		cat('\nMessages from stan_model:',temp,sep='\n')
	# 	}
	# }
	return(rstan::stan_model(file))
	# mod = NULL
	# try(mod <- rstan::stan_model(file))
	# if(!is.null(mod)){
	# 	cat('\nBuild successful.')
	# 	return(mod)
	# }else{
	# 	stop('Build failed.')
	# }
}
