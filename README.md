README

These  materials  are supporting data for a comment that Eric An‐
derson and Thomas Ng sent to Bioinformatics regarding  the  soft‐
ware SOLOMON which was authored by Mark Christie and others.

We  have  implemented  software akin to SOLOMON, but differing in
that it is based upon the likelihood‐ratio  and  consequently  is
more accurate than SOLOMON.  We have named our software SOLIDMON.

We have also replaced a lot of the simulation that  SOLOMON  uses
to  calculate  probabilities with analytical calculations, and we
do our simulation conditional on sharing  at  least  one  allele,
which seems to make SOLIDMON considerably faster than SOLOMON.

Additionally,  some  segments  of the code are implemented in the
compiled C language which reduces computational time as well.

We are not releasing SOLIDMON here as a  piece  of  software  for
general  use  by molecular ecologists, etc.  Though they may cer‐
tainly adapt it to their own situation.  We hope that it will  be
useful in upating the program SOLOMON.

SOLIDMON  software  was written in part while ECA was working for
the government of the United States of America.   Accordingly  it
is  not subject to copyright in the U.S.  It is in the public do‐
main.

We include here a portion of the SOLOMON code in  SOLOMON.R  that
we  modified to allow easy batch use without the Tcl/Tk GUI. This
code is not part of the SOLIDMON package.

THERE IS NO WARRANTY FOR THIS SOFTWARE. WE  PROVIDE  THE  PROGRAM
âAS  ISâ  WITHOUT  WARRANTY  OF ANY KIND, EITHER EXPRESSED OR IM‐
PLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED  WARRANTIES  OF
MERCHANTABILITY  AND FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE
RISK AS TO THE QUALITY AND PERFORMANCE OF  THE  PROGRAM  IS  WITH
YOU.  SHOULD  THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF
ALL NECESSARY SERVICING, REPAIR OR CORRECTION.


Questions? contact  eric.anderson@noaa.gov



Tutorial on running SOLIDMON and SOLOMON as we did to get the re‐
sults in our paper:


1)  Compile  all of the C source files into shared objects that R
can interface with

cd PATH_TO_SOLIDMON_pkg R CMD SHLIB src/simulate.c  
R  CMD  SHLIB src/mend_pair.c




2)  To  run  SOLOMON  in batch mode you provide paths for the kid
genotype file, the dad genotype file and the output file you want
SOLOMON  to write to. These can be specified when invoking R from
the command line like so:

R CMD BATCH  ‐‐vanilla  ‐slave  ’‐‐args  kidfile="demo/kids1.txt" dadfile="demo/dads1.txt"  out_file="out/Output_SOLOMON.txt"’  SOLOMON.R

SOLOMON.R contains mostly code written by Mark Christie.  We com‐
mented out the GUI stuff in it and selected only the bit for‐sin‐
gle parent parentage.  Since we were trying to understand how the
program  worked  we disabled the part of the program that cleaned
up all its intermediate files.  If you plan to run SOLOMON.R more
than once, it is important to clean up those files yourself.  So,
once you are done with a run, you  can  move  your  out_file  (as
specified  on  the  command line) out of the "out" directory, and
then remove all the other files in the "out" directory.


3) To run SOLIDMON you provide paths for the kid  genotype  file,
dad genotype file and the name of the output file. You should al‐
so provide the number of simulation replicates.   100000   (1e05)
is reasonable.

R  CMD  BATCH  ‐‐vanilla  ‐slave ’‐‐args kidfile="demo/kids1.txt" dadfile="demo/dads1.txt"  REPS=1e05   out_file="out/Output_SOLIDMON.txt"’ SOLIDMON.R





The directories snp_data and micro_data contain data sets that we
simulated under the two data scenarios  described  in  our  manu‐
script.   The kids in the "kids" files are named according to who
their parents are (in the corresponding "dads" file), unless they
have  no  parents  in  the "dads" file, which is apparent because
they are named "Indiv_Fem_X_Y", where X and Y are numbers.  Indi‐
viduals in the "dads" files that have names like "Indiv_Male_X_Y"
will have offspring in the corresponding "kids" file, while those
with names like "Indiv_Fem_X_Y" will not.
