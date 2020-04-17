#' Compute a summary table for the posterior samples for a parameter from a Stan model
#'
#' @param from_stan A stanfit object from a call to rstan::stan() or rstan::sampling() containing posterior samples from the model.
#' @param par Character string naming the parameter on which to compute the summary table.
#' @param probs Vector of probabilities to show as columns in the summary table.
#' @param X Optional contrast matrix as yielded from get_contrast_matrix() indicating the contrasts passed as data when creating the above stanfit object.
#' @param W Optional contrast matrix as yielded from get_contrast_matrix() indicating the within-subjects contrasts passed as data when creating the above stanfit object. If present, X is ignored and B must be present.
#' @param B Optional contrast matrix as yielded from get_contrast_matrix() indicating the between-subjects contrasts passed as data when creating the above stanfit object. W must be present, else B is ignored.
#' @param is_cor A logical indicating whether the parameter is a correlation matrix. If TRUE, returns only the upper-diagonal entries.
#'
#' @return A tibble
#' @export
#'
#' @examples
stan_summary = function(
	from_stan
	, par
	, probs = c(.5,.025,.975)
	, X = NULL
	, W = NULL
	, B = NULL
	, is_cor = F
){
	m = monitor(post,probs=probs,print=F)
	all_pars = dimnames(m)[[1]]
	all_pars_no_squares = str_replace(dimnames(m)[[1]],'\\[.*\\]','')
	select_pars = all_pars[all_pars_no_squares%in%par]
	requested_pars = par
	m %>% 
		tibble::as_tibble(m) %>% 
		dplyr::mutate(
			par = str_replace(dimnames(m)[[1]],'\\[.*\\]','')
		) %>% 
		dplyr::filter(
			par%in%requested_pars
		) %>% 
		dplyr::select(
			par
			, contains('%')
			, Rhat
			, Bulk_ESS
			, Tail_ESS
		) ->
		m
	
	if(!is_cor){
		if(!is.null(X)){
			m$par = dimnames(X)[[2]]
		}
		if(!is.null(W)){
			m$par = names_from_WB(W,B)
		}
	}else{
		temp = select_pars
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
		m = m[keep,]
		m$par = paste(v1,v2,sep='~')
	}
	return(m)
}
