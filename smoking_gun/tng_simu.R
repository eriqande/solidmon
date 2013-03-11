#zipf returns a table of allele frequencies of alleles i with Locus L
zipf.func <- function(i) {
  seq(1,i)/sum(seq(1,i))
}

num_sires <- 100
num_Loci <- 10
num_kids <- 50
num_unrel <- 150
num_alleles <- 10

fert_sim <- function(sires, num_alleles, num_Loci, num_sires) {
	#this function returns a matrix of genotype of offspring	 	
	mam <- matrix(sample(c(1:num_alleles), 2*num_sires*num_Loci, replace = T, prob=zipf.func(num_alleles)), ncol=2*num_Loci)
	sel <- matrix(replicate(num_sires*num_Loci, sample(c(0,1))), ncol=2*num_Loci, byrow=T)
	sires*sel + mam*(1-sel)
}

#the genotype setup presumes that all LOCI have the same allele frequencies 
sires <- matrix(sample(1:num_alleles, 2*num_sires*num_Loci, replace = T, prob=zipf.func(num_alleles)), ncol=num_Loci*2)
offspring <- fert_sim(sires, num_alleles, num_Loci, num_sires)
unrelated <- matrix(sample(1:num_alleles, 2*num_unrel*num_Loci, replace = T, prob=zipf.func(num_alleles)), ncol=num_Loci*2)

sires <- cbind( paste0("Father_",1:num_sires), sires)
colnames(sires) = c("Individuals",rep(paste0("Locus",1:num_Loci),each=2))
write.table(sires, "Dad_data.txt", sep="\t", row.names=F,quote=F)

offspring <- cbind( paste0("Father_",1:num_sires,"_Kid_",1:num_sires), offspring)
unrelated <- cbind( paste0("Individ_",1:num_unrel), unrelated)

kids <- rbind(offspring[1:num_kids,], unrelated)
colnames(kids) = c("Individuals", rep(paste0("Locus",1:num_Loci),each=2))
write.table(kids, "kids_data.txt", sep="\t", row.names=F,quote=F)
