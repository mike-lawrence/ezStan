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

