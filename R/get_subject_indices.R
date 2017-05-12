#' Get a table of start/end indices from subject label vector
#'
#' Given input of an ordered subject label vector, returns the indices into this
#' vector corresponding to the beginning and end of each subject.
#'
#' @param x A vector of subject labels, sorted by said labels.
#'
#' @return A matrix with two columns (start & end) and as many rows as there are unique labels in the input
#' @export
#'
#' @examples
get_subject_indices = function(x){
	x = as.numeric(factor(x)) #converts labels/numbers to 1:length(x) sequence
	temp = 1:length(x)
	indices = matrix(NA,nrow=max(x),ncol=2)
	for(i in 1:max(x)){
		indices[i,1] = min(temp[x==i])
		indices[i,2] = max(temp[x==i])
	}
	return(indices)
}
