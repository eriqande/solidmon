setwd("/Users/eriq/Documents/git-repos/solidmon/smoking_gun/")
# load shared libraries:
dyn.load("../src/solidmon.so")
dyn.load("../src/mend_pair.so")



### DEFINE USEFUL FUNCTIONS ################################
# this function will read data from a kids file and a dads file and 
# prepare things for analysis
ReadAndPrep <- function(kidFile=NULL, dadFile=NULL) {

#	dadFile<-"/tmp/smoking_gun_dads1.txt"
#	kidFile<-"/tmp/smoking_gun_kids1.txt"
	
	if(is.null(kidFile) || is.null(dadFile)) stop("function ReadAndPrep needs kid and dad file paths")

	# read genotype data
	kids <- read.table(kidFile, colClasses="character", na.strings=c("NA", "0"), header=T, row.names=1)
	dads <- read.table(dadFile, colClasses="character", na.strings=c("NA", "0"), header=T, row.names=1)
	
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
	ps <- as.integer(c(0,cumsum(sapply(af,length)))) 
	
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
	
	# and we put that list inside this one and return it
	list(af=af, dads=dads, kids=kids, kl=kl, dl=dl, C.input=C.func.input)
	
}



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


# now something that would be fun here would be to compute another
# quantity for each pair, the posterior predictive p-value.  That is, given 
# the estimate of pi, what fraction of pairs with LoglRatio greater than
# or equal to the observed value will be parental pairs.
PPV <- function(af, REPS=5e4, pi, Pairs) {
 GPP <- lapply(af, GenoPairProbs)  # get the conditional probs
 
 # sample genotype indices of unrelated pairs conditional on no Mendelian incompatibilities
 idxs.U <- lapply(GPP, function(x) sample.int(nrow(x), REPS, T, x[,1]/sum(x[,1])))
 
 # sample genotypes of parent-offspring pairs
 idxs.PO <- lapply(GPP, function(x) sample.int(nrow(x), REPS, T, x[,2]))
 
 # now get the log-likelihood ratio for all of those and add them up across loci
 # first for the unrelated non excluded pairs
 logl.U <- rowSums(sapply(names(idxs.U), function(x) log(GPP[[x]][ idxs.U[[x]], 2]) - log(GPP[[x]][ idxs.U[[x]], 1])))
 # and then for the true parental pairs
 logl.PO <- rowSums(sapply(names(idxs.PO), function(x) log(GPP[[x]][ idxs.PO[[x]], 2]) - log(GPP[[x]][ idxs.PO[[x]], 1])))
 
 # and finally compute the PPV using pi.  Do this for each LogLratio of the observed pairs:
 ppv <- sapply(Pairs$LOG_LIKELIHOOD, function(x) (1-pi)*sum(logl.PO>=x) / ((1-pi)*sum(logl.PO>=x) + pi*sum(logl.U>=x)))
 ppv
 
}


# a simple function to take the kid's name and get the name of its father out of it:
GetDadFromName <- function(x) {
	gsub("Indiv", "NONE", gsub("KID.*", "", gsub("MOTHER.*FATHER_", "", x)))
}

# simple function to get the name of a Male like it would be reported as a dad
GetMaleFromName <- function(x) {
	gsub("_0$", "",  gsub("Indiv_", "", x))
}

# here is a function that does a complete analysis:
WholeSchmear <- function(kidfile, dadfile, REPS=5e4) {
 # now, go a ahead and do the pairwise comparisons
 D <- ReadAndPrep(kidfile, dadfile)
 
 MendMax <- 1
 CatchIt <- do.call(.C, args=c("Pair_filter", MendMax, D$C.input))
 
 Pairs <- read.table("Mend_pair_sel.tbl", header=T)
 Pairs<-Pairs[Pairs$NUM_MATCH==D$C.input$L,] # for this just focus on the no-mendelian incompatible loci case
 Pairs<-Pairs[order(Pairs$LOG_LIKELIHOOD, decreasing=T),]  # put in order of the likelihood ratio
 NP<-nrow(Pairs)  # count the number of pairs
 pi <- min(1, prod(1-D$C.input$E) * D$C.input$nS * D$C.input$nK / NP) 

 Pairs$PPV <- PPV(D$af, REPS, pi, Pairs)
 Pairs$SireName <- rownames(D$dads)[Pairs$SIR_INDX]
 Pairs$KidName <- rownames(D$kids)[Pairs$KIDS_INDX]
 
 Pairs$KidsDad <- GetDadFromName(Pairs$KidName)
 Pairs$DadNamesToCompare <- GetMaleFromName(Pairs$SireName)
 
 Pairs$Correct <- Pairs$DadNamesToCompare == Pairs$KidsDad
 
 NoDropPairs <- Pairs
 Pairs<-Pairs[!duplicated(Pairs$KIDS_INDX),] # if any kid is in more than one pair, just retain the one with highest likelihood
 

 list(D=D, Pairs=Pairs, NoDrop=NoDropPairs)
}	


####################################### DONE WITH USEFUL FUNCTIONS ############



kidfiles<-paste("./data/smoking_gun_kids",1:50,".txt", sep="")
dadfiles<-paste("./data/smoking_gun_dads",1:50,".txt", sep="")

# just doing the first 25 here...
solidmon.ten.locus.output <- lapply(1:20, function(x) WholeSchmear(kidfiles[x], dadfiles[x]))

save(solidmon.ten.locus.output, file="SolidmonMicSatOutput.Rda")

## From here we go to the other scripts to process that file and make a figure for the paper.





# now, compute the ROC curves for each of those:
roc <- lapply(boing, function(P) list(x=cumsum(!(P$Pairs$Correct)), y=cumsum((P$Pairs$Correct))))

plot(1:1,1:1, xlim=c(0,42), ylim=c(0,50), type="n", xlab="False Postives", ylab="True Positives")
ll <- length(roc)
lapply(1:ll, function(x) lines(roc[[x]]$x+(x-1)/ll, roc[[x]]$y+(x-1)/ll, lwd=.2, col="blue" ))

dev.copy2pdf(file="50_roc_curves_solidmon.pdf")



# compare to SOLOMON
# here are the results that Thomas got from SOLOMON:
SOLOM.OUTPUT <- lapply(1:25, function(x) read.table(paste("SOLOMON_output/ALL_out_SOLOMON/",x,"_Output_SOLOMON.txt", sep=""), header=T) )
SOLOM.DROP <- lapply(SOLOM.OUTPUT, function(x) x[!duplicated(x$Offspring) & x$NL_mismatch==0,]) # drop duplicated kids and only take the ones with No Mendelian incompatibilities


# now we want to get the indices for the individuals 


SOLOM.RES <- lapply(strsplit(readLines("data/50runs_output_labels.txt"), "\\s+", perl=TRUE), function(x) as.logical(as.numeric(x)))
SOLOM.ROC <- lapply(SOLOM.RES, function(x) list(x=cumsum(!x), y=cumsum(x)))


par(mfrow=c(5,5))
par(mar=c(2,2,2,2))
lapply(1:25, function(x) {plot(roc[[x]], col="blue", type="l", ylim=c(0,50), xlim=c(0, max(length(roc[[x]]$x)-50, length(SOLOM.ROC[[x]]$x)-50))); lines(SOLOM.ROC[[x]], col="red")})


# read in the raw solomon output
