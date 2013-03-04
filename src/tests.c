/* these are just a few functions that we can call from R in order to test 
   little parts of our programs
*/
#include <R.h>
#include "solidmon.h"


/* test the macros we have for computing things about pairs of genotypes for a locus with K alleles at frequencies of P */
void test_PairMacros(int *K, double *P) {
	int a,b,c,d, q,r,s,t;
	int k = *K;
	double *p=P;
	FILE *out=fopen("test_PairMacros.txt", "w");
	
	/* first, we can verify things are working right by enumerating things and we can sum to make sure they equal one*/
	fprintf(out,"a b c d SHARES_1_OR_2 SHARES_2 MISSING UN_PROB PO_PROB MZ_PROB\n");
	for(a=0;a<k;a++) for(b=a;b<k;b++) for(c=0;c<k;c++) for(d=c;d<k;d++) {
		fprintf(out, "%d %d %d %d %d %d %d %f %f %f\n", a,b,c,d, SHARES_1_OR_2, SHARES_2, MISSING, UN_PROB, SHARES_1_OR_2*PO_PROB, SHARES_2*MZ_PROB);
	}
	fclose(out);
	
	/* and also, we can check to make sure that allele ordering doesn't screw up the probability calculations */
	out=fopen("test_PairMacros2.txt", "w");
	for(a=0;a<4;a++) fprintf(out, "SHARES_1_OR_2 SHARES_2 MISSING UN_PROB PO_PROB MZ_PROB ");
	fprintf(out,"\n");
	for(q=0;q<k;q++) for(r=q;r<k;r++) for(s=0;s<k;s++) for(t=s;t<k;t++) {
		a=q; b=r; c=s; d=t;
		fprintf(out,"%d %d %d %f %f %f ",SHARES_1_OR_2, SHARES_2, MISSING, UN_PROB, SHARES_1_OR_2*PO_PROB, SHARES_2*MZ_PROB);
		c=t; d=s;
		fprintf(out,"%d %d %d %f %f %f ",SHARES_1_OR_2, SHARES_2, MISSING, UN_PROB, SHARES_1_OR_2*PO_PROB, SHARES_2*MZ_PROB);
		a=r; b=q;
		fprintf(out,"%d %d %d %f %f %f ",SHARES_1_OR_2, SHARES_2, MISSING, UN_PROB, SHARES_1_OR_2*PO_PROB, SHARES_2*MZ_PROB);
		c=s; d=t;
		fprintf(out,"%d %d %d %f %f %f \n",SHARES_1_OR_2, SHARES_2, MISSING, UN_PROB, SHARES_1_OR_2*PO_PROB, SHARES_2*MZ_PROB);
	}	
	fclose(out);
}