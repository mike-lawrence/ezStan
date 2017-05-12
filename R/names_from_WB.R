#' Figure out variable names given W & B contrast matrices
#'
#' @param W Optional contrast matrix as yielded from get_contrast_matrix() indicating the within-subjects contrasts passed as data when creating the above stanfit object. If present, X is ignored and B must be present.
#' @param B Optional contrast matrix as yielded from get_contrast_matrix() indicating the between-subjects contrasts passed as data when creating the above stanfit object. W must be present, else B is ignored.
#' @param reverse Logical indicating whether to reverse the order of the names.
#'
#' @return A vector of character strings indicating names of variables.
#' @export
#'
#' @examples
names_from_WB = function(W,B,reverse = F){
	B_names = dimnames(B)[[2]]
	W_names = dimnames(W)[[2]]
	new_names = rep(NA,length(W_names)*length(B_names))
	k = 1
	if(!reverse){
		for(i in B_names){
			for(j in W_names){
				new_names[k] = paste(i,j,sep=':')
				k = k + 1
			}
		}
	}else{
		for(i in W_names){
			for(j in B_names){
				new_names[k] = paste(j,i,sep=':')
				k = k + 1
			}
		}
	}
	new_names = gsub('(I):','',new_names,fixed=T)
	new_names = gsub(':(I)','',new_names,fixed=T)
	new_names = gsub('(Intercept):','',new_names,fixed=T)
	new_names = gsub(':(Intercept)','',new_names,fixed=T)
	return(new_names)
}
