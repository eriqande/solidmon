#include <R.h>
#include <stdio.h>
#include "solidmon.h"

#define G(i,j,k,L) i*2*L + j*2 + k

/*generate all possible pairs between S vec and K vec but skip any pairs that exceed a pre-defined level of mendalian incompatibility */

void Pair_filter(int *max_incompat, int *S, int *K, double *P,  double *excP, int *L, int *nS, int *nK, int *ps)
{
	int num_S = *nS;
	int num_K = *nK;
	int num_L = *L;
	int max_incom = *max_incompat;
	int s, k, j;
	int a, b, c, d;
	
	FILE *out;
	out=fopen("Mend_pair_sel.tbl", "w");
	 
	//iterate for each dad 
	for (s = 0; s < num_S; s++) { //iterate for each dad 
		for (k = 0; k < num_K; k++) { //iterate thru each kids
			double prob_pair = 1;
			double unrel_prob = 1;
			int match = 0;
			
			//iterate thru each locus
			fprintf(out, "%i\t%i\t" ,s+1, k+1);
			for (j = 0, match=0; j < num_L, match < max_incom ; j++) { //iterate thru each locus
				a = S[G(s, j, 0, num_L)]; 
				b = S[G(s, j, 1, num_L)];
				c = K[G(s, j, 0, num_L)]; 
				d = K[G(s, j, 1, num_L)];
				double *p =  P + ps[j];
				
				if (MISSING) {
					 prob_pair *= 1;
					 unrel_prob *= 1;
					 fprintf(out, "-1\t");
					 continue;
				}
				
				if(SHARES_1_OR_2) {
					match++;
					prob_pair *= PO_PROB;
					unrel_prob *= UN_PROB;
					fprintf(out, "1\t");
				}
				else{
					fprintf(out, "0\t");
				}				
				
			}
			fprintf(out, "%i\t%lf\t$lf\n" ,match, prob_pair, unrel_prob);
		}
	}
	fclose(out);
}
