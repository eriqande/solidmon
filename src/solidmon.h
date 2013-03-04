/* here are a number of useful macros to inline some of the genotype pair
   calculations.  Unless otherwise stated, these all assume that the candidate
   parent has alleles a and b and the candidate offspring has alleles c and d and
   that the frequencies of the alleles are in the array p.  Note that a,b,c,d are
   indexes of alleles.
*/

/* returns 1 if the pair shares at least one allele */
#define SHARES_1_OR_2     (a==c || a==d || b==c || b==d)
/* returns 1 if the pair shares exactly 2 alleles */
#define SHARES_2     ((a==c && b==d) || (a==d && b==c))
/*return 1 if one of the alleles is missing*/
#define MISSING     (a==-1 || b==-1 || c==-1 || d==-1)
/*return a four-digit value of which indexes of alleles are missing*/
#define indx_MISS (a==-1)*1000 + (b==-1)*100 + (c==-1)*10 + (d==-1) 

/* prob of two genotypes given they are unrelated */
#define UN_PROB     (p[a]*p[b]*(1.0 + (a!=b)) * p[c]*p[d]*(1.0 + (c!=d)))
/* prob of genotype pair given they are parent offspring (so long as they share at least one allele!) */
#define PO_PROB     (p[a]*p[b]*(1.0 + (a!=b)) * (a==b  ?  (b==c ? p[d] : p[c]  ) : (0.5 * ( (c==a || c==b)*p[d] + (c!=d)*(d==a || d==b)*p[c]) )))
/* prob of genotype pair given then are monozygous twins  (so long as they share two alleles!) */
#define MZ_PROB     (p[a]*p[b]*(1.0 + (a!=b)))
