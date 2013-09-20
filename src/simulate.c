#include <R.h>
#include "solidmon.h"

/* given the frequencies of KK alleles in the array p, this function
returns a vector that includes the probability of the genotypes of a pair 
of individuals that share at least one allele. It computes this probability
for three cases: 1) the pair share no genes IBD, 2) the pair shares 1 gene IBD,
and 3) the pair shares two genes IBD. It does not scale these
so they sum to one. */
void geno_pair_probs(int *KK, double *p, double *out) {
	int K=*KK;
	int a,b,c,d, i=0;
	
	for(a=0;a<K;a++) {
		for(b=a;b<K;b++) {
			for(c=0;c<K;c++) {
				for(d=c;d<K;d++) {
					if(SHARES_1_OR_2) { 
						/* geno probs if no genes are shared IBD (like unrelated) */
						out[i] = UN_PROB;
						
						/* geno probs if 1 gene is shared IBD  (like parent-offspring) */
						out[i+1] = PO_PROB;
						
						/* geno probs if 2 genes are shared IBD (like monozygous twin). Note it has to be zero if only one allele is shared */
						out[i+2] = SHARES_2 * MZ_PROB;
						
						i+=3;
					}
				}	
			}
		}
	}
}