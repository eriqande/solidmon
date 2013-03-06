#include <R.h>
#include <stdio.h>
#include <math.h>
#include "solidmon.h"

#define G(L,s,H,nS) (2*nS)*L + s*2 + H 

/*generate all possible pairs between S vec and K vec but skip any pairs that exceed a pre-defined level of mendalian incompatibility */

int Pair_filter(int *max_incompat, int *S, int *K, double *P,  double *excP, int *L, int *nS, int *nK, int *ps)
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
			double prob_pair = 0;
			double unrel_prob = 0;
			int match = 0;
			
			//iterate thru each locus
			fprintf(out, "%i\t%i\t" ,s+1, k+1);
			for (j = 0, match=0; j < num_L ; j++) { //iterate thru each locus
				a = S[G(j, s, 0, num_S)]; 
				b = S[G(j, s, 1, num_S)];
				c = K[G(j, k, 0, num_K)]; 
				d = K[G(j, k, 1, num_K)];
				double *p =  P + ps[j];
				
				fprintf(out, "%i\t", (MISSING)*-1 + (!MISSING)*(SHARES_1_OR_2));
				//fprintf(out, "%i %i %i %i , %i, %i, %i, \n", a, b, c, d, G(s, k, 0, j), G(s, k, 1,j), j);
				if (MISSING) 
				    continue;
				
				if(SHARES_1_OR_2) {
					match++;
					prob_pair += log(PO_PROB);
					unrel_prob += log(UN_PROB);
				}				
				
			}
			fprintf(out, "%i\t%lf\t%lf\n" ,match, prob_pair, unrel_prob);
		}
	}
	fclose(out);
	return 1;
}
