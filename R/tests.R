
setwd("/Users/eriq/Documents/git-repos/solidmon/junk")
dyn.load("../src/tests.so")



# little functions to run the C-test functions
PairMacros_run <- function(p) {
	KK<-length(p)
	if(KK==0) stop("Just passed test_PairMacros some allele freqs with zero length")
	outs <- .C("test_PairMacros", as.integer(KK), as.double(p))
	list(A=read.table("test_PairMacros.txt", header=T), B=read.table("test_PairMacros2.txt", header=T))
	
}




# down here we actually do some little tests
# first take a simple 4 allele locus and inspect the output and verify things
# sum to 1
x<-PairMacros_run(c(.1, .15, .30, .45))

# check that these sum to one. Might be some rounding issues
lapply(x$A[8:10], sum) 

# now check that the quantities are the same regardless of allele order
step<-c(1,7,13,19)
boing<-lapply(0:5, function(z) {
	y<-x$B[z+step];
	cbind(y[,1]==y[,2], y[,1]==y[,3], y[,1]==y[,4], y[,2]==y[,3], y[,2]==y[,4], y[,3]==y[,4])
})
names(boing)<-names(x$B)[1:6]
boing

