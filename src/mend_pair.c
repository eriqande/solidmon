#include <R.h>
#include <stdio.h>

/* get the k-th gene copy (0 or 1) at the j-th locus, in the i-th individual */
#define G(i,j,k,L) i*2*L + j*2 + k
#define freq(j,k,p,ps) p[ps[j]+k]  

//generate all possible pairs between S vec and K vec but skip any pairs that exceed a pre-defined level of mendalian incompatibility

void Pair_filter(int *max_incompat, int *S, int *K, double *p,  double *excP, int *L, int *nS, int *nK, int *ps)
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
	for (s = 0; s < num_S; s++)
	{
		//iterate thru each kids
		for (k = 0; k < num_K; k++)
		{
			double prob_pair = 1;
			int match = 0;
			fprintf(out, "--> %i %i %i\n", max_incom, num_S, num_L);
			//iterate thru each locus
			for (j = 0, match=0; j < num_L, match < max_incom ; j++)
			{
				//examine whether there are any shared alleles
				a = S[G(s, j, 0, num_L)]; 
				b = S[G(s, j, 1, num_L)];
				c = K[G(s, j, 0, num_L)]; 
				d = K[G(s, j, 1, num_L)];
				freq_c = freq(j,c,p,ps);
				freq_d = freq(j,d,p,ps);
				double prob = 0;
				
				fprintf(out, "inside\t->%i\t%i\t%i\t%i\tFreq_c:%lf\tFreq_d:%lf\n" ,a, b, c, d, freq_c, freq_d);
				//for now, we treat missing data as mendalian incompatible type
				if (a==-1 || b==-1 || c==-1 || d==-1)
				{
					 prob_pair *= prob;
					 continue;
				}
				
				if(a == c || a == d || b == c || b == d)
				{
					match++;
					// calculate the likelihood prob. for the putative pairs to have any mendalian compatibility
					prob += a==b? ( b==c ? freq_d: freq_c ) : 0.5 * ((a==c)*freq_d + (a==d)*freq_c + (c!=d)*((b==c)*freq_d + (b==d)*freq_c) );
				}
				fprintf(out, "prob pair: %lf \n", prob);
				//calculate the prior distribution of the parental genotype
				prob *= freq_c * freq_d ;
				prob_pair = a==b? prob*2 : prob;				
				
			}
			fprintf(out, "%i\t%i\t%i\t%lf\n" ,s+1, k+1, match, prob_pair);
		}
	}
	fclose(out);
	
}

//calculate marginal distribution of all parental pairs
