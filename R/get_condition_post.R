#' Combine coefficient samples from a Stan model with the design matrix to compute posterior samples on the conditions of the design.
#'
#' @param from_stan A stanfit object from a call to rstan::stan() or rstan::sampling() containing posterior samples from the model.
#' @param par Character string naming the parameter on which to compute the summary table.
#' @param X Optional contrast matrix as yielded from get_contrast_matrix() indicating the contrasts passed as data when creating the above stanfit object.
#' @param W Optional contrast matrix as yielded from get_contrast_matrix() indicating the within-subjects contrasts passed as data when creating the above stanfit object. If present, X is ignored and B must be present.
#' @param B Optional contrast matrix as yielded from get_contrast_matrix() indicating the between-subjects contrasts passed as data when creating the above stanfit object. W must be present, else B is ignored.
#' @param numeric_res Integer value conveying the number of points within the range of a continuous variable to sample. If 0, the values from the data are used.
#' @param collapse_intercept_to_median Logical indicating whether to use the median of the samples for the intercept rather than its full set of samples. Assumes that the intercept is the first parameter.
#'
#' @return If the `tibble` package is installed, a tibble; else, a data frame.
#' @export
#'
#' @examples
#' @importFrom tidyr gather
#' @importFrom dplyr one_of
get_condition_post <-
	function(
		from_stan
		, par
		, X = NULL
		, W = NULL
		, B = NULL
		, numeric_res = 0
		, collapse_intercept_to_median = FALSE
	){
		if(!is.null(W)){
			W_data = attr(W,'data')
			if (inherits(W_data, "tbl_df")) {
				W_data = as.data.frame(data)
			}
			B_data = attr(B,'data')
			if (inherits(B_data, "tbl_df")) {
				B_data = as.data.frame(B_data)
			}
			W_vars = attr(terms(attr(W,'formula')),'term.labels')
			W_vars = W_vars[!grepl(':',W_vars)]
			B_vars = attr(terms(attr(B,'formula')),'term.labels')
			B_vars = B_vars[!grepl(':',B_vars)]
			data_vars = c(B_vars,W_vars)
			new_names = names_from_WB(W,B,reverse=T)
		}else{
			data = attr(X,'data')
			if (inherits(data, "tbl_df")) {
				data = as.data.frame(data)
			}
			data_vars = attr(terms(attr(X,'formula')),'term.labels')
			data_vars = data_vars[!grepl(':',data_vars)]
			new_names = dimnames(X)[[2]]
		}
		temp = list()
		for(i in 1:length(data_vars)){
			this_var = data_vars[i]
			if(is.null(W)){
				this_fixed_data = data[,names(data)==this_var]
			}else{
				if(this_var%in%W_vars){
					this_fixed_data = W_data[,names(W_data)==this_var]
				}else{
					this_fixed_data = B_data[,names(B_data)==this_var]
				}
			}
			if(is.numeric(this_fixed_data)*(numeric_res>0)){
				temp[[i]] = seq(
					min(this_fixed_data)
					, max(this_fixed_data)
					, length.out=numeric_res
				)
			}else{
				temp[[i]] = sort(unique(this_fixed_data))
				if(is.factor(temp[[i]])){
					contrasts(temp[[i]]) = contrasts(this_fixed_data)
				}
			}
		}
		to_return = data.frame(expand.grid(temp))
		names(to_return) = data_vars
		both_formula = eval(parse(text=paste('~', paste(data_vars, collapse = '*'))))
		unique_mm = get_contrast_matrix(
			data = to_return
			, formula = both_formula
		)
		unique_mm = unique_mm[,new_names]
		samples = rstan::extract(from_stan,par)[[1]]
		samples = matrix(samples,nrow=dim(samples)[1])
		if(collapse_intercept_to_median){
			samples[,1] = median(samples[,1])
		}
		# samples = samples[,match(new_names,dimnames(unique_mm)[[2]])]
		mat = matrix(NA,nrow=nrow(to_return),ncol=dim(samples)[1])
		for(i in 1:dim(samples)[1]){
			mat[,i] <- unique_mm%*%samples[i,]
		}
		to_return = cbind(to_return,as.data.frame(mat))
		to_return = tidyr::gather(to_return,key='sample',value='value',-dplyr::one_of(data_vars))
		to_return$sample = as.numeric(gsub('V','',as.character(to_return$sample),fixed=T))
		if('tibble' %in% installed.packages()){
			to_return = tibble::as_tibble(to_return)
		}
		return(to_return)
	}

