#' Compute a summary table for the posterior samples for a parameter from a Stan model
#'
#' @param from_stan A stanfit object from a call to rstan::stan() or rstan::sampling() containing posterior samples from the model.
#' @param par Character string naming the parameter on which to compute the summary table.
#' @param probs Vector of probabilities to show as columns in the summary table.
#' @param digits Integer value indicating number of significant digits to show when printing the summary table.
#' @param X Optional contrast matrix as yielded from get_contrast_matrix() indicating the contrasts passed as data when creating the above stanfit object.
#' @param W Optional contrast matrix as yielded from get_contrast_matrix() indicating the within-subjects contrasts passed as data when creating the above stanfit object. If present, X is ignored and B must be present.
#' @param B Optional contrast matrix as yielded from get_contrast_matrix() indicating the between-subjects contrasts passed as data when creating the above stanfit object. W must be present, else B is ignored.
#' @param is_cor A logical indicating whether the parameter is a correlation matrix. If TRUE, returns only the upper-diagonal entries.
#' @param return_array A logical indicating whether to return the summary table as an array. Default is to print but not return anything.
#'
#' @return Either nothing (merely prints to screen) if return_array is FALSE, or the array corresponding to the summary table.
#' @export
#'
#' @examples
stan_summary = function(
	from_stan
	, par
	, probs = c(.5,.025,.975)
	, digits = 2
	, X = NULL
	, W = NULL
	, B = NULL
	, is_cor = F
	, return_array = F
){

	s = summary(object=from_stan,pars=par,probs=probs,use_cache=F)$summary
	s = array(
		s[,4:ncol(s)]
		, dim = c(dim(s)[1],ncol(s)-3)
		, dimnames = list(
			dimnames(s)[[1]]
			, dimnames(s)[[2]][4:ncol(s)]
		)
	)
	if(!is_cor){
		if(!is.null(X)){
			dimnames(s)[[1]] = dimnames(X)[[2]]
		}
		if(!is.null(W)){
			dimnames(s)[[1]] = names_from_WB(W,B)
		}
	}else{
		temp = dimnames(s)[[1]]
		temp = gsub(']','',temp)
		temp = unlist(strsplit(temp,'[',fixed=T))
		temp = temp[(1:length(temp))%%2==0]
		temp = unlist(strsplit(temp,',',fixed=T))
		v1 = temp[(1:length(temp))%%2==1]
		v2 = temp[(1:length(temp))%%2==0]
		keep = v2>v1
		v1 = v1[keep]
		v2 = v2[keep]
		if(!is.null(X)){
			v1 = dimnames(X)[[2]][as.numeric(v1)]
			v2 = dimnames(X)[[2]][as.numeric(v2)]
		}
		if(!is.null(W)){
			temp = names_from_WB(W,B)
			v1 = temp[as.numeric(v1)]
			v2 = temp[as.numeric(v2)]
		}
		s = array(
			s[keep,]
			, dim = c(sum(keep),ncol(s))
			, dimnames = list(
				paste(v1,v2,sep='~')
				, dimnames(s)[[2]]
			)
		)
	}
	if(!return_array){
		print(s,digits=digits)
	}else{
		return(s)
	}
}
