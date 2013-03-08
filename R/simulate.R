
#setwd("/Users/eriq/Documents/git-repos/solidmon/src")
dyn.load("/Users/eriq/Documents/git-repos/solidmon/src/solidmon.so")


# wrapper to call geno_pair_probs with some allele frequencies
GenoPairProbs <- function(p) {
	KK<-length(p)
	if(KK==0) stop("Just passed GenoPairProb some allele freqs with zero length")
	
	# calculate how many states there will be
	N <- KK*KK + choose(KK,2) * (2*KK - 1)
	
	# make the call to the C function
	outs <- .C("geno_pair_probs", as.integer(KK), as.double(p), res=double(length=N*3))
	
	# make the result an N by 3 matrix
	matrix(outs$res, ncol=3, byrow=T)	
}	


if(FALSE) {
# here are some examples of how to use this:
tt<-GenoPairProbs((1/1:100)/sum(1/1:100))  #  locus with 100 alleles
tt<-GenoPairProbs( c(.5,.5))
unclass(object.size(tt)) / 10^6  # memory used by tt, in Mb
rr<-.25*tt[,1] + .5*tt[,2] + .25*tt[,3]  # probabities of the genotypes for full sibling pair
rr<-rr/sum(rr)  # make them conditional probs (conditional on sharing at least one allele)
unrel<-tt[,1]/sum(tt[,1])  # conditional probs for unrelated pair 


# take a sample of genotype probs (given they are unrelated) from unrelated pairs given that
# they share at least one allele
boing <- sample(tt[,1], 10^5, replace=T, prob=unrel) 
}                 