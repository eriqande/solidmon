setwd("/Users/eriq/Documents/git-repos/solidmon/smoking_gun/")
source("../R/data_input_and_summary.R")



dyn.load("../src/mend_pair.so")


MendMax<-10L
do.call(.C, args=c("Pair_filter", MendMax, C.func.input))

MM <- read.table("Mend_pair_sel.tbl", header=T)