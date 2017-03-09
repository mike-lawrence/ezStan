#' @importFrom stats "contr.sum" "contrasts" "contrasts<-" "cor" "model.matrix" "pnorm" "sd" "terms"
#' @importFrom utils installed.packages
NULL

#' Half-sum Contrasts
#'
#' Returns contrasts whereby coeffients will be equal to the difference between
#' levels (in the case of 2-level factors). Simply contr.sum()/2.
#' @param ... Arguments passed to contr.sum
#'
#' @return See Value in contr.sum().
#' @export
#'
#' @examples
halfsum_contrasts = function(
	...
){
	contr.sum(...)*.5
}

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
	attr(mm,'data') = as.data.frame(data)
	return(mm)
}


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
				new_names[k] = paste(j,i,sep=':')
				k = k + 1
			}
		}
	}else{
		for(i in W_names){
			for(j in B_names){
				new_names[k] = paste(i,j,sep=':')
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

	s = summary(object=from_stan,pars=par,probs=probs,use_cache=F)
	s = s$summary
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

#' Combine coefficient samples from a Stan model with the design matrix to compute posterior samples on the conditions of the design.
#'
#' @param from_stan A stanfit object from a call to rstan::stan() or rstan::sampling() containing posterior samples from the model.
#' @param par Character string naming the parameter on which to compute the summary table.
#' @param X Optional contrast matrix as yielded from get_contrast_matrix() indicating the contrasts passed as data when creating the above stanfit object.
#' @param W Optional contrast matrix as yielded from get_contrast_matrix() indicating the within-subjects contrasts passed as data when creating the above stanfit object. If present, X is ignored and B must be present.
#' @param B Optional contrast matrix as yielded from get_contrast_matrix() indicating the between-subjects contrasts passed as data when creating the above stanfit object. W must be present, else B is ignored.
#' @param numeric_res Integer value conveying the number of points within the range of a continuous variable to sample. If 0, the values from the data are used.
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
			data_vars = c(W_vars,B_vars)
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
				this_fixed_data = sort(unique(data[,names(data)==this_var]))
			}else{
				if(this_var%in%W_vars){
					this_fixed_data = sort(unique(W_data[,names(W_data)==this_var]))
				}else{
					this_fixed_data = sort(unique(B_data[,names(B_data)==this_var]))
				}
			}
			if(is.numeric(this_fixed_data)*(numeric_res>0)){
				temp[[i]] = seq(
					min(this_fixed_data)
					, max(this_fixed_data)
					, length.out=numeric_res
				)
			}else{
				temp[[i]] = this_fixed_data
				if(is.factor(temp[[i]])){
					contrasts(temp[[i]]) = contrasts(this_fixed_data)
				}
			}
		}
		to_return = data.frame(expand.grid(temp))
		names(to_return) = data_vars
		both_formula = eval(parse(text=paste('~', paste(data_vars, collapse = '*'))))
		unique_mm = model.matrix(both_formula,to_return)
		unique_mm = unique_mm[,new_names]
		samples = rstan::extract(from_stan,par)[[1]]
		samples = matrix(samples,nrow=dim(samples)[1])
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


#' A metric of normality
#'
#' Provides a measure of normality for the input by computing the correlation
#' between the data and the expected data given each data point's quantile and a
#' normal distribution with a mean equal to the data's mean and a standard
#' deviation equal to the data's standard deviation.
#' @param x A numeric vector.
#'
#' @return A single value on the scale -1 to 1.
#' @export
#'
#' @examples
normality = function(x){
	x = sort(x)
	m = mean(x)
	s = sd(x)
	q = pnorm(seq(0,1,length.out=length(x))[2:(length(x)-1)],m,s)
	x = x[2:(length(x)-1)]
	r = cor(x,q)
	return(r)
}

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
