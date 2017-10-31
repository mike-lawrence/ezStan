#' Watch the progress of a Stan session
#'
#' @param update_interval Number of seconds to wait between updates.
#' @param one_line_per_chain A logical value specifying whether the progress for each chain should be printed on seperate lines.
#' @param spacing An integer value specifying the number of extra spaces to add to the end of each chain's progress string. Can be used to fix misalignment when one_line_per_chain is TRUE.
#' @return No value is returned.
#' @export
#'
#' @examples
watch_stan = function(update_interval=1,one_line_per_chain=TRUE,spacing=3){
	stan_stuff = NULL
	load('stan_temp/stan_stuff.rda')
	if(!("num_done" %in% names(stan_stuff))){
		stan_stuff$num_done = length(list.files(path="stan_temp/rdas"))
		stan_stuff$done_list = c()
		stan_stuff$error_list = c()
		stan_stuff$message_list = c()
	}
	while((stan_stuff$num_done<stan_stuff$cores) & (length(stan_stuff$error_list)<stan_stuff$cores) ){ #quit if done or all errors
		stan_stuff$num_done = length(list.files(path="stan_temp/rdas"))
		Sys.sleep(update_interval)
		for(i in 1:stan_stuff$cores){
			if(!(i %in% stan_stuff$done_list)){ #if this chain isn't already done
				if(file.exists(stan_stuff$sample_file_list[[i]])){ #only try reading the sample file if it exists
					size = file.size(stan_stuff$sample_file_list[[i]])
					if(size>stan_stuff$sample_file_size_list[[i]]){ #only try reading if the sample file size has changed
						f = file(description=stan_stuff$sample_file_list[[i]],open='rb')
						seek(con = f, origin = 'start', where = stan_stuff$sample_file_size_list[[i]])
						a = readLines(f)
						close(f)
						stan_stuff$sample_file_size_list[[i]] = size
						a = a[substr(a,1,1)!="#"]
						a = a[substr(a,1,4)!="lp__"]
						a = a[a!='']
						stan_stuff$progress_list[[i]] = stan_stuff$progress_list[[i]] + length(a)
						if(stan_stuff$progress_list[[i]]==stan_stuff$iter){
							stan_stuff$done_list = c(stan_stuff$done_list,i)
							#check for post-sampling messages from Stan
							if(file.exists(stan_stuff$stderr_file_list[[i]])){ #check if the stderr file exists
								temp = readLines(stan_stuff$stderr_file_list[[i]])
								if(length(temp)>0){ #stderr file has contents
									stan_stuff$message_list = c(stan_stuff$message_list,i)
								}
							}
						}
						save(stan_stuff,file='stan_temp/stan_stuff.rda')
					}
				}else{
					if(file.exists(stan_stuff$rda_file[[i]])){ #might not have caught this finished chain before its sample file was deleted
						stan_stuff$progress_list[[i]] = stan_stuff$iter
						stan_stuff$done_list = c(stan_stuff$done_list,i)
						#check for post-sampling messages from Stan
						if(file.exists(stan_stuff$stderr_file_list[[i]])){ #check if the stderr file exists
							temp = readLines(stan_stuff$stderr_file_list[[i]])
							if(length(temp)>0){ #stderr file has contents
								stan_stuff$message_list = c(stan_stuff$message_list,i)
							}
						}
						save(stan_stuff,file='stan_temp/stan_stuff.rda')
					}
				}
			}
			if(!(i %in% stan_stuff$done_list)){
				if(!(i %in% stan_stuff$error_list)){ #if this chain isn't already in the error list
					if(file.exists(stan_stuff$stderr_file_list[[i]])){ #check if the stderr file exists
						temp = readLines(stan_stuff$stderr_file_list[[i]])
						if(length(temp)>0){ #stderr file has contents
							stan_stuff$error_list = c(stan_stuff$error_list,i)
						}
					}
				}
			}
			min_done = min(unlist(stan_stuff$progress_list))
			time_elapsed = difftime(Sys.time(), stan_stuff$start_time,units='secs')
			time_left = "?"
			if(is.finite(min_done)){
				if(min_done>0){
					time_left = time_as_string(time_elapsed/min_done*(stan_stuff$iter-min_done))
				}
			}
			temp = paste0(unlist(stan_stuff$chain_name_list),': ',unlist(stan_stuff$progress_list),'/',stan_stuff$iter)
			update_text_to_print = '\r'
			for(i in temp){
				update_text_to_print = append_string(update_text_to_print,i,spacing,one_line_per_chain)
			}
			temp = paste0('Chains complete: ',stan_stuff$num_done,'/',stan_stuff$cores)
			update_text_to_print = append_string(update_text_to_print,temp,spacing,one_line_per_chain)
			temp = paste0('Estimated time remaining: ',time_left)
			update_text_to_print = append_string(update_text_to_print,temp,spacing,one_line_per_chain)
			if(length(stan_stuff$error_list)>0){
				temp = paste0('chains with errors: ',paste(stan_stuff$error_list,collapse=', '))
				update_text_to_print = append_string(update_text_to_print,temp,spacing,one_line_per_chain)
			}
			if(length(stan_stuff$message_list)>0){
				temp = paste0('chains with messages: ',paste(stan_stuff$message_list,collapse=', '))
				update_text_to_print = append_string(update_text_to_print,temp,spacing,one_line_per_chain)
			}
			cat(update_text_to_print)
			utils::flush.console()
		}
	}
	if((length(stan_stuff$error_list)==stan_stuff$cores)){ #we're done because all errors
		cat('\nErrors on all chains!')
	}else{
		#one last check for messages
		for(i in 1:stan_stuff$cores){
			if(!(i %in% stan_stuff$error_list)){ #if it's not already in the error list
				if(!(i %in% stan_stuff$message_list)){ #if it's not already in the message list
					if(file.exists(stan_stuff$stderr_file_list[[i]])){ #if the stderr file exists
						temp = readLines(stan_stuff$stderr_file_list[[i]])
						if(length(temp)>0){ #stderr file has contents
							stan_stuff$message_list = c(stan_stuff$message_list,i)
						}
					}
				}
			}
		}
		update_text_to_print = '\n'
		time_elapsed = difftime(Sys.time(), stan_stuff$start_time,units='secs')
		temp = paste0('All done! Elapsed time: ',time_as_string(time_elapsed))
		update_text_to_print = append_string(update_text_to_print,temp,spacing,one_line_per_chain)
		if(length(stan_stuff$error_list)>0){
			temp = paste0('chains with errors: ',paste(stan_stuff$error_list,collapse=', '))
			update_text_to_print = append_string(update_text_to_print,temp,spacing,one_line_per_chain)
		}
		if(length(stan_stuff$message_list)>0){
			temp = paste0('chains with messages: ',paste(stan_stuff$message_list,collapse=', '))
			update_text_to_print = append_string(update_text_to_print,temp,spacing,one_line_per_chain)
		}
		cat(update_text_to_print)
	}
	utils::flush.console()
	return(invisible(NULL))
}
