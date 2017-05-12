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
