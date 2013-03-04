#include <R.h>
#include <stdio.h>
#include <solidmon.h>

/* get the k-th gene copy (0 or 1) at the j-th locus, in the i-th individual */
//#define G(i,j,k,L) i*2*L + j*2 + k
//#define freq(j,p,ps) p[ps[j]]  

//generate all possible pairs between S vec and K vec but skip any pairs that exceed a pre-defined level of mendalian incompatibility

void Pair_filter(int *max_incompat, int *S, int *K, double *P,  double *excP, int *L, int *nS, int *nK, int *ps)
{
	int num_S = *nS;
	int num_K = *nK;
	int num_L = *L;
	int max_incom = *max_incompat;
	int s, k, j;
	int a, b, c, d;
	double freq_c, freq_d; 
	
	FILE *out;
	out=fopen("Mend_pair_sel.tbl", "w");
	 
	//iterate for each dad 
	for (s = 0; s < num_S; s++) { //iterate for each dad 
		for (k = 0; k < num_K; k++) { //iterate thru each kids
			double prob_pair = 1;
			double unrel_prob = 1;
			int match = 0;
			
			fprintf(out, "--> %i %i %i\n", max_incom, num_S, num_L);
			//iterate thru each locus
			for (j = 0, match=0; j < num_L, match < max_incom ; j++) { //iterate thru each locus
				//examine whether there are any shared alleles
				a = S[G(s, j, 0, num_L)]; 
				b = S[G(s, j, 1, num_L)];
				c = K[G(s, j, 0, num_L)]; 
				d = K[G(s, j, 1, num_L)];
				double *p =  P + ps[j];
				
				
				//fprintf(out, "inside\t->%i\t%i\t%i\t%i\tFreq_c:%lf\tFreq_d:%lf\n" ,a, b, c, d, freq_c, freq_d);

				if (MISSING) {
					//should either store or output the info of what locus or the magnitude of the missing data
					if (SHARE_1_)
					
					 prob_pair *= 0;
					 unrel_prob *= 0;
					 continue;
				}
				
				if(SHARE_1_OR_2) {
					match++;
					prob_pair *= PO_PROB;
					unrel_prob *= UN_PROB;
				}				
				
			}
			fprintf(out, "%i\t%i\t%i\t%lf\n" ,s+1, k+1, match, prob_pair);
		}
	}
	fclose(out);
	
}

//calculate marginal distribution of all parental pairs
