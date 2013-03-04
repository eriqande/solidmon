# read genotype data
kids <- read.table("kids4solo.txt", colClasses="character", na.strings=c("NA", "0"), header=T, row.names=1)
dads <- read.table("dads4solo.txt", colClasses="character", na.strings=c("NA", "0"), header=T, row.names=1)

# check to make sure the locus names are the same in both data sets
if(any(names(kids)!=names(dads))) stop("Locus names of kids and dads mismatch!!")

# put them into geno-data lists
listize.genos <- function(x) {
	res<-lapply(seq(1,ncol(x),by=2), function(y) list(a=x[, y], b=x[, y+1]))
	names(res)<-names(x)[seq(1,ncol(x),by=2)]
	res
}

kl<-listize.genos(kids)
dl<-listize.genos(dads)


# now, get the allele freqs for each locus by counting alleles in the parent
# population, but also add a single allele for each type observed in the kids. Note that the names
# of these alleles will be later used as the levels of a factor
af<-lapply(lapply(names(dl), function(x) table(c(dl[[x]]$a, dl[[x]]$b, unique(c(c(kl[[x]]$a, kl[[x]]$b)))), useNA="no")), function(x) x/sum(x))
names(af)<-names(dl)


# and here we compute the probability that an unrelated pair in this population does not share
# any alleles at each locus
# given allele freqs in vector q, this computes the exclusion prob
exc.prob.func <- function(q) {
	tt<-0.0
	for(i in 1:length(q)) {
		tt <- tt + q[i]^2 * (1-q[i])^2
		for(j in 1:length(q)) {
			if(i<j) tt <- tt + 2*q[i]*q[j]*(1-q[i]-q[j])^2
		}
	}
	tt	
}

excP <- as.double(sapply(af, exc.prob.func))

### HERE WE DO WHAT WE NEED TO IN ORDER TO PASS THESE THINGS INTO C
# now, make a vector p of allele freqs to pass into C, and a vector ps of the indexes of where each locus starts in p
p <- as.double(unlist(af))
ps <- as.integer(c(0,cumsum(sapply(af,length))-1))

# get the number of loci, kids and dads. Note these are all class integer, so I don't coerce them...
L <- length(dl)
nS <- nrow(dads)
nK <- nrow(kids)

# get S (for sires) and K (for kids) which are vectors of integer specifications of the genotypes
# this function turns each component of a geno-data list into an integer vector with alleles 
# having indexes 0, 1, 2, ... and missing data are given as -1
vectize.geno.list <- function(ll, af) {
	lapply(names(ll),  function(x) { res<-as.integer(factor(as.vector(rbind(ll[[x]]$a, ll[[x]]$b)), levels=names(af[[x]])))-1L; res[is.na(res)]<- -1L; res}  )
}
# now get S and K
S <- as.integer(unlist(vectize.geno.list(dl, af)))
K <- as.integer(unlist(vectize.geno.list(kl, af)))


# this list has all the variables that we need for the C function call
C.func.input <- list(S=S, K=K, p=p, E=excP, L=L, nS=nS, nK=nK, ps=ps)

# If needed, we can confirm that they have the right types, just fun.  Two should be doubles and the rest integer
# lapply(C.func.input, typeof)
