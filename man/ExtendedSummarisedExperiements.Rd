\name{hello}
\alias{hello}
\title{Hello, World!}
\usage{
ExtendedSummarisedExperiment(ExonMatrix=NULL, ExonDescription=NULL, VarientInfo=NULL, IsoformMatrix=NULL, IsoformDescription=NULL, counts=NULL, rowRanges=NULL, colData=NULL)
}
\description{
creates a 'ExtendedSummarisedExperiment' object. This stores the isoform, gene and exon count matrix's,
}
\examples{
nrows <- 200; ncols <- 6
counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
rowRanges <- GRanges(rep(c("chr1", "chr2"), c(50, 150)),
                     IRanges(floor(runif(200, 1e5, 1e6)), width=100),
                     strand=sample(c("+", "-"), 200, TRUE),
                     feature_id=sprintf("ID%03d", 1:200))
colData <- DataFrame(Treatment=rep(c("ChIP", "Input"), 3),
                     row.names=LETTERS[1:6])

example <- ExtendedSummarisedExperiment(
  ExonMatrix=matrix(0,0,0),
  ExonDescription=matrix(0,0,0),
  VarientInfo=matrix(0,0,0),
  IsoformMatrix=matrix(0,0,0),
  IsoformDescription=matrix(0,0,0),
  counts = counts,
  rowRanges = rowRanges,
  colData = colData)
}
