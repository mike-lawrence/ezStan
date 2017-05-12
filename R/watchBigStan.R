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
	if(!("numDone" %in% names(bigStanStuff))){
		bigStanStuff$numDone = length(list.files(path="bigStanTemp/rdas"))
		bigStanStuff$doneList = c()
		bigStanStuff$errorList = c()
		bigStanStuff$messageList = c()
	}
	while((bigStanStuff$numDone<bigStanStuff$cores) & (length(bigStanStuff$errorList)<bigStanStuff$cores) ){ #quit if done or all errors
		bigStanStuff$numDone = length(list.files(path="bigStanTemp/rdas"))
		Sys.sleep(updateInterval)
		for(i in 1:bigStanStuff$cores){
			if(!(i %in% bigStanStuff$doneList)){ #if this chain isn't already done
				if(file.exists(bigStanStuff$sampleFileList[[i]])){ #only try reading the sample file if it exists
					size = file.size(bigStanStuff$sampleFileList[[i]])
					if(size>bigStanStuff$sampleFileSizeList[[i]]){ #only try reading if the sample file size has changed
						f = file(description=bigStanStuff$sampleFileList[[i]],open='rb')
						seek(con = f, origin = 'start', where = bigStanStuff$sampleFileSizeList[[i]])
						a = readLines(f)
						close(f)
						bigStanStuff$sampleFileSizeList[[i]] = size
						a = a[substr(a,1,1)!="#"]
						a = a[substr(a,1,4)!="lp__"]
						a = a[a!='']
						bigStanStuff$progressList[[i]] = bigStanStuff$progressList[[i]] + length(a)
						if(bigStanStuff$progressList[[i]]==bigStanStuff$iter){
							bigStanStuff$doneList = c(bigStanStuff$doneList,i)
							#check for post-sampling messages from Stan
							if(file.exists(bigStanStuff$stderrFileList[[i]])){ #check if the stderr file exists
								temp = readLines(bigStanStuff$stderrFileList[[i]])
								if(length(temp)>0){ #stderr file has contents
									bigStanStuff$messageList = c(bigStanStuff$messageList,i)
								}
							}
						}
						save(bigStanStuff,file='bigStanTemp/bigStanStuff.rda')
					}
				}else{
					if(file.exists(bigStanStuff$rdaFile[[i]])){ #might not have caught this finished chain before its sample file was deleted
						bigStanStuff$progressList[[i]] = bigStanStuff$iter
						bigStanStuff$doneList = c(bigStanStuff$doneList,i)
						#check for post-sampling messages from Stan
						if(file.exists(bigStanStuff$stderrFileList[[i]])){ #check if the stderr file exists
							temp = readLines(bigStanStuff$stderrFileList[[i]])
							if(length(temp)>0){ #stderr file has contents
								bigStanStuff$messageList = c(bigStanStuff$messageList,i)
							}
						}
						save(bigStanStuff,file='bigStanTemp/bigStanStuff.rda')
					}
				}
			}
			if(!(i %in% bigStanStuff$doneList)){
				if(!(i %in% bigStanStuff$errorList)){ #if this chain isn't already in the error list
					if(file.exists(bigStanStuff$stderrFileList[[i]])){ #check if the stderr file exists
						temp = readLines(bigStanStuff$stderrFileList[[i]])
						if(length(temp)>0){ #stderr file has contents
							bigStanStuff$errorList = c(bigStanStuff$errorList,i)
						}
					}
				}
			}
			minDone = min(unlist(bigStanStuff$progressList))
			timeElapsed = difftime(Sys.time(), bigStanStuff$startTime,units='secs')
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
			if(length(bigStanStuff$errorList)>0){
				temp = paste0('chains with errors: ',paste(bigStanStuff$errorList,collapse=', '))
				updateTextToPrint = appendString(updateTextToPrint,temp,spacing,one_line_per_chain)
			}
			if(length(bigStanStuff$messageList)>0){
				temp = paste0('chains with messages: ',paste(bigStanStuff$messageList,collapse=', '))
				updateTextToPrint = appendString(updateTextToPrint,temp,spacing,one_line_per_chain)
			}
			cat(updateTextToPrint)
			utils::flush.console()
		}
	}
	if((length(bigStanStuff$errorList)==bigStanStuff$cores)){ #we're done because all errors
		cat('\nErrors on all chains!')
	}else{
		#one last check for messages
		for(i in 1:bigStanStuff$cores){
			if(!(i %in% bigStanStuff$errorList)){ #if it's not already in the error list
				if(!(i %in% bigStanStuff$messageList)){ #if it's not already in the message list
					if(file.exists(bigStanStuff$stderrFileList[[i]])){ #if the stderr file exists
						temp = readLines(bigStanStuff$stderrFileList[[i]])
						if(length(temp)>0){ #stderr file has contents
							bigStanStuff$messageList = c(bigStanStuff$messageList,i)
						}
					}
				}
			}
		}
		updateTextToPrint = '\n'
		timeElapsed = difftime(Sys.time(), bigStanStuff$startTime,units='secs')
		temp = paste0('All done! Elapsed time: ',timeAsString(timeElapsed))
		updateTextToPrint = appendString(updateTextToPrint,temp,spacing,one_line_per_chain)
		if(length(bigStanStuff$errorList)>0){
			temp = paste0('chains with errors: ',paste(bigStanStuff$errorList,collapse=', '))
			updateTextToPrint = appendString(updateTextToPrint,temp,spacing,one_line_per_chain)
		}
		if(length(bigStanStuff$messageList)>0){
			temp = paste0('chains with messages: ',paste(bigStanStuff$messageList,collapse=', '))
			updateTextToPrint = appendString(updateTextToPrint,temp,spacing,one_line_per_chain)
		}
		cat(updateTextToPrint)
	}
	utils::flush.console()
	return(invisible(NULL))
}
