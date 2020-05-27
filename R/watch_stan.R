#' Watch the progress of a Stan session
#'
#' @param update_interval Number of seconds to wait between updates.
#' @param one_line_per_chain A logical value specifying whether the progress for each chain should be printed on seperate lines.
#' @param spacing An integer value specifying the number of extra spaces to add to the end of each chain's progress string. Can be used to fix misalignment when one_line_per_chain is TRUE.
#' @param beep_when_done A logical value specifying whether a sound should be played on completion (requires the beepr package to be installed).
#' @param kill_on_divergence A logical value specifying whether to kill chains if a post-warmup divergence is encountered.
#' @return No value is returned.
#' @export
#'
#' @examples
watch_stan = function(update_interval=1,one_line_per_chain=TRUE,spacing=2,beep_when_done=TRUE,kill_on_divergence=FALSE,timeout=NA){
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
	#	warmup
	#	max_treedepth
	#	stan_args
	#pre-defining objects we'll get from load() to avoid package build warnings
	start_time = NULL
	load('stan_temp/start_time.rda')
	#loads the following objects:
	#  start_time
	num_chains = cores*chains_per_core
	if(!file.exists('stan_temp/watching.rda')){
		watching = list()
		watching$chain_names = list()
		watching$sample_files = list()
		watching$rda_files = list()
		watching$log_files = list()
		watching$sample_file_sizes = list()
		watching$samples_done = list()
		watching$time_left = list()
		watching$sum_exceed_max = list()
		watching$sum_divergences = list()
		for(this_chain in 1:num_chains){
			chain_name = sprintf(paste0('chain%0',ceiling(log10(cores)),'d'),this_chain)
			watching$chain_names[[this_chain]] = chain_name
			watching$sample_files[[this_chain]] = paste0('stan_temp/samples_',chain_name,'.txt')
			watching$rda_files[[this_chain]] = paste0('stan_temp/rdas_',chain_name,'.rda')
			watching$log_files[[this_chain]] = paste0('stan_temp/logs_',chain_name,'.log')
			watching$sample_file_sizes[[this_chain]] = 0
			watching$samples_done[[this_chain]] = 0
			watching$time_left[[this_chain]] = NA
			watching$sum_exceed_max[[this_chain]] = 0
			watching$sum_divergences[[this_chain]] = 0
			watching$warmup_end_time[[this_chain]] = NA
		}
		watching$num_done = length(list.files(path="stan_temp",pattern='rdas_'))
		watching$dones = c()
		watching$chains_with_messages = c()
		watching$messages_to_print = c()
	}else{
		#pre-defining objects we'll get from load() to avoid package build warnings
		watching = NULL
		load('stan_temp/watching.rda')
		#loads the following objects:
		#	watching
	}
	while((watching$num_done<num_chains) & (length(watching$chains_with_messages)<num_chains) ){ #quit if done or all errors
		watching$num_done = length(list.files(path="stan_temp",pattern='rdas_'))
		Sys.sleep(update_interval)
		time_now = as.numeric(Sys.time())
		time_elapsed = ( time_now - start_time )
		if(!is.na(timeout)){
			if(time_elapsed>timeout){
				kill_stan()
				if('beepr'%in%installed.packages()){
					eval(parse(text='beepr::beep()')) #hiding beepr dependency from package check
				}
				stop('Timeout reached.')
			}
		}
		for(this_chain in 1:num_chains){
			if(!(this_chain %in% watching$dones)){ #if this chain isn't already done
				if(file.exists(watching$sample_files[[this_chain]])){ #only try reading the sample file if it exists
					size = file.size(watching$sample_files[[this_chain]])
					if(size>watching$sample_file_sizes[[this_chain]]){ #only try reading if the sample file size has changed
						# f = file(description=watching$sample_files[[this_chain]],open='rb')
						# seek(con = f, origin = 'start', where = watching$sample_file_sizes[[this_chain]])
						# a = readLines(f)
						# close(f)
						if(is.na(watching$warmup_end_time[[this_chain]])){
							a = NULL
							try(a <- data.table::fread(
								file = watching$sample_files[[this_chain]]
								, sep = ','
								, select = paste('V',1:7,sep='')
								, header = FALSE
								, skip = 26 + watching$samples_done[[this_chain]]
								, blank.lines.skip = TRUE
								, fill = TRUE
								, nrows = warmup - watching$samples_done[[this_chain]]
							),silent=T)
						}else{
							a = NULL
							try(a <- data.table::fread(
								file = watching$sample_files[[this_chain]]
								, sep = ','
								, select = paste('V',1:7,sep='')
								, header = FALSE
								, skip = 26 + watching$samples_done[[this_chain]] + 4
								, nrows = iter - watching$samples_done[[this_chain]]
								, fill = TRUE
								, blank.lines.skip = TRUE
							),silent=T)
						}
						if(!is.null(a)){
							names(a)[1:7] = c('lp__','accept_stat__','stepsize__','treedepth__','n_leapfrog__','divergent__','energy__')
							a = a[!is.na(a$divergent__),]
							if(nrow(a)>0){
								watching$sample_file_sizes[[this_chain]] = size
								watching$samples_done[[this_chain]] = watching$samples_done[[this_chain]] + nrow(a)
								watching$sum_exceed_max[[this_chain]] = watching$sum_exceed_max[[this_chain]] + sum(a$treedepth__>max_treedepth)
								if(watching$samples_done[[this_chain]]>warmup){
									if(any(a$divergent__==1)){
										watching$sum_divergences[[this_chain]] = watching$sum_divergences[[this_chain]] + sum(a$divergent__)
										if(kill_on_divergence){
											kill_stan()
											if('beepr'%in%installed.packages()){
												eval(parse(text='beepr::beep()')) #hiding beepr dependency from package check
											}
											stop('Post-warmup divergence encountered')
										}
									}
								}
								if(watching$samples_done[[this_chain]]>0){
									if(watching$samples_done[[this_chain]]>=warmup){
										if(is.na(watching$warmup_end_time[[this_chain]])){
											watching$warmup_end_time[[this_chain]] = time_now
										}else{
											time_per_sample = ( time_now -  watching$warmup_end_time[[this_chain]] ) / (watching$samples_done[[this_chain]]-warmup)
											samples_left = iter - watching$samples_done[[this_chain]]
											watching$time_left[[this_chain]] = samples_left*time_per_sample
										}
									}else{
										time_per_sample = time_elapsed / watching$samples_done[[this_chain]]
										samples_left = iter - watching$samples_done[[this_chain]]
										watching$time_left[[this_chain]] = samples_left*time_per_sample
									}
								}
								if(watching$samples_done[[this_chain]]==iter){
									watching$dones = c(watching$dones,this_chain)
								}
								save(watching,file='stan_temp/watching.rda')
							}
						}
					}
				}else{
					if(file.exists(watching$rda_files[[this_chain]])){ #might not have caught this finished chain before its sample file was deleted
						watching$samples_done[[this_chain]] = iter
						watching$dones = c(watching$dones,this_chain)
						save(watching,file='stan_temp/watching.rda')
					}
				}
			}
			# if(!(this_chain %in% watching$chains_with_messages)){ #if this chain isn't already in the error list
			# 	if(file.exists(watching$log_files[[this_chain]])){ #check if the log file exists
			# 		temp = readLines(watching$log_files[[this_chain]])
			# 		if(length(temp)>0){ #log file has contents
			# 			watching$chains_with_messages = c(watching$chains_with_messages,this_chain)
			# 			watching$messages_to_print = c(
			# 				watching$messages_to_print
			# 				, paste0(
			# 					'['
			# 					, watching$chain_names[[this_chain]]
			# 					, ' messages:] '
			# 				)
			# 			)
			# 			for(j in 1:length(temp)){
			# 				watching$messages_to_print = c(
			# 					watching$messages_to_print
			# 					, temp[j]
			# 				)
			# 			}
			# 		}
			# 	}
			# }
			time_left = "?"
			if(any(!is.na(unlist(watching$time_left)))){
				time_left = time_as_string(max(unlist(watching$time_left),na.rm=T))
			}
			update_text_to_print = '\r'
			for(this_chain in 1:num_chains){
				this_chain_message = paste0(
					watching$chain_names[[this_chain]]
					, ': '
					, watching$samples_done[[this_chain]]
					,'/',iter
				)
				if(watching$sum_divergences[[this_chain]]>0){
					this_chain_message = paste0(
						this_chain_message
						, ' , divergences: '
						, watching$sum_divergences[[this_chain]]
					)
				}
				if(watching$sum_exceed_max[[this_chain]]>0){
					this_chain_message = paste0(
						this_chain_message
						, ' , max_treedepth exceeded: '
						, watching$sum_exceed_max[[this_chain]]
					)
				}
				update_text_to_print = append_string(update_text_to_print,this_chain_message,spacing,one_line_per_chain)
			}
			temp = paste0('Chains complete: ',watching$num_done,'/',num_chains)
			update_text_to_print = append_string(update_text_to_print,temp,spacing,one_line_per_chain)
			temp = paste0('Estimated time remaining: ',time_left)
			update_text_to_print = append_string(update_text_to_print,temp,spacing,one_line_per_chain)
			temp = paste0('Elapsed time: ',time_as_string(time_elapsed))
			update_text_to_print = append_string(update_text_to_print,temp,spacing,one_line_per_chain)
			if(!is.na(timeout)){
				temp = paste0('Time to timeout: ',time_as_string(timeout-time_elapsed))
				update_text_to_print = append_string(update_text_to_print,temp,spacing,one_line_per_chain)
			}
			if(length(watching$messages_to_print)>0){
				for(temp in watching$messages_to_print){
					update_text_to_print = append_string(update_text_to_print,temp,spacing,one_line_per_chain)
				}
				update_text_to_print = append_string(update_text_to_print,' ',spacing,one_line_per_chain)
			}
			cat(update_text_to_print)
			utils::flush.console()
		}
	}
	#one last check for messages
	for(this_chain in 1:num_chains){
		if(!(this_chain %in% watching$chains_with_messages)){ #if this chain isn't already in the error list
			if(file.exists(watching$log_files[[this_chain]])){ #check if the log file exists
				temp = readLines(watching$log_files[[this_chain]])
				if(length(temp)>0){ #log file has contents
					watching$chains_with_messages = c(watching$chains_with_messages,this_chain)
					for(j in 1:length(temp)){
						watching$messages_to_print = c(
							watching$messages_to_print
							, paste0(
								'['
								, watching$chain_names[[this_chain]]
								, ':] '
								, temp[j]
							)
						)
					}
				}
			}
		}
	}
	update_text_to_print = '\r'
	for(this_chain in 1:(num_chains+1)){
		update_text_to_print = append_string(update_text_to_print,' ',spacing,one_line_per_chain)
	}
	time_elapsed = ( as.numeric(Sys.time()) - start_time )
	temp = paste0('All done! Elapsed time: ',time_as_string(time_elapsed))
	update_text_to_print = append_string(update_text_to_print,temp,spacing,one_line_per_chain)
	if(length(watching$messages_to_print)>0){
		for(temp in watching$messages_to_print){
			update_text_to_print = append_string(update_text_to_print,temp,spacing,one_line_per_chain)
		}
		update_text_to_print = append_string(update_text_to_print,' ',spacing,one_line_per_chain)
	}
	update_text_to_print = paste0(update_text_to_print,'\n')
	cat(update_text_to_print)
	utils::flush.console()
	if(beep_when_done){
		if('beepr'%in%installed.packages()){
			eval(parse(text='beepr::beep()')) #hiding beepr dependency from package check
		}
	}
	return(invisible(NULL))
}
