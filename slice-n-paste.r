#! /usr/bin/Rscript

##
# Read multiple *.csv files, split off the specified columns and
# write them to a new file.  Time-stamp: <2016-12-13 15:32:07 gepr>
# dev.off()

argv <- commandArgs(TRUE)

if (length(argv) < 2) {
    print("Usage: slice-n-paste.r <col_1:col_N | col_1,col_2,...,col_N> *_nectrig.csv")
    print("e.g. slice-n-paste.r 1,2,3 pap0+(0[89]|1[01])x_nectrig.csv")
    print("e.g. slice-n-paste.r 5:10 pap0+(0[89]|1[01])x_nectrig.csv")
    quit()
}

if (grepl(":",argv[1])) {
  cols <- strsplit(argv[1],':')
  col1 <- as.integer(cols[[1]][1])
  col2 <- as.integer(cols[[1]][2])
  colrange <- col1:col2
} else {
  cols <- strsplit(argv[1],',')
  colrange <- as.integer(cols[[1]])
}

for (f in argv[2:length(argv)]) {
  expname <- substr(f, 0, regexpr('_',f)[1]-1)
  print(paste("Processing ", expname))

  data.in <- read.csv(f)
  colnames(data.in) <- paste(expname, colnames(data.in), sep="-")

  ## pad the shorter one
  if (exists("data.out")) {
    if (nrow(data.in) > nrow(data.out)) {
       pad <- matrix(data=NA, nrow(data.in)-nrow(data.out), ncol(data.out))
       colnames(pad) <- colnames(data.out)
       data.out <- rbind(data.out, pad)
    } else if (nrow(data.in) < nrow(data.out)) {
       pad <- matrix(data=NA, nrow(data.out)-nrow(data.in), ncol(data.in))
       colnames(pad) <- colnames(data.in)
       data.in <- rbind(data.in,pad)
    }
  }

  for (column in colrange) {
    if (exists("data.out")) {
      data.out <- cbind(data.out, data.in[column])
    } else {
      data.out <- data.in[column]
    }
  }
}
write.csv(data.out,"pasted.csv")
