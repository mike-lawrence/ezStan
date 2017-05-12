#' Obtain a contrast matrix for a data set given a specified formula
#'
#' @param data A data frame (or tibble).
#' @param formula A formula starting with `~` expressing the model
#' @param do_halfsum_contrasts A logical indicating whether to use half-sum contrasts.
#'
#' @return A contrast matrix with the formula & data saved as attributes.
#' @export
#'
#' @examples
get_contrast_matrix = function(
	data
	, formula
	, do_halfsum_contrasts = T
){
	if (inherits(data, "tbl_df")) {
		data = as.data.frame(data)
	}
	vars = attr(terms(formula),'term.labels')
	vars = vars[!grepl(':',vars)]
	if(length(vars)==1){
		data = data.frame(data[,vars])
		names(data) = vars
	}else{
		data = data[,vars]
	}
	vars_to_rename = NULL
	for(i in vars){
		if(is.character(data[,i])){
			data[,i] = factor(data[,i])
		}
		if( is.factor(data[,i])){
			if(length(levels(data[,i]))==2){
				vars_to_rename = c(vars_to_rename,i)
			}
			if(do_halfsum_contrasts ){
				contrasts(data[,i]) = halfsum_contrasts
			}
		}
	}
	mm = model.matrix(data=data,object=formula)
	dimnames(mm)[[2]][dimnames(mm)[[2]]=='(Intercept)'] = '(I)'
	for(i in vars_to_rename){
		dimnames(mm)[[2]] = gsub(paste0(i,1),i,dimnames(mm)[[2]])
	}
	attr(mm,'formula') = formula
	attr(mm,'data') = data
	return(mm)
}
