## Transform a list of vector of the same length to a data.frame
list2DF <- function(list){
	l <- as.vector(unlist(lapply(list, length)))
	if(length(unique(l))>1) stop("list elements not some length. Cannot transform to data.frame")
	df <- as.data.frame(matrix(as.vector(unlist(list)), ncol=l[1]))
	return(df)
}