dyn.load("count_both.so")

count<-function(orig_p, shuffled_p, row = nrow(orig_p), col = ncol(orig_p)) {
	ta <- .C("count_both", as.double(orig_p), as.double(shuffled_p), as.integer(row), as.integer(col), 
               count.less=as.integer(rep(0, length(orig_p))), count.greater=as.integer(rep(0, length(orig_p))))
	list(less = matrix(ta$count.less, row, col), greater = matrix(ta$count.greater, row, col));
}







