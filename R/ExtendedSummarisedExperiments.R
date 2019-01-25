# Hello, world!
#
# This is an example function named 'ExtendedSummarisedExperients'
# which creates a 'ExtendedSummarisedExperiment' object.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

library(SummarizedExperiment)

######################################################################

.ExtendedSummarisedExperiment <- setClass("ExtendedSummarisedExperiment",
                                          slots= representation(
                                            ExonMatrix="list",
                                            ExonDescription="matrix",
                                            VarientInfo="matrix",
                                            IsoformMatrix="list",
                                            IsoformDescription="matrix"
                                          ),
                                          contains="SummarizedExperiment"
)
ExtendedSummarisedExperiment <- function(
  ExonMatrix=matrix(0,0,0),
  ExonDescription=matrix(0,0,0),
  VarientInfo=matrix(0,0,0),
  IsoformMatrix=matrix(0,0,0),
  IsoformDescription=matrix(0,0,0),
  counts, rowRanges, colData)
{
  se <- SummarizedExperiment(assays=SimpleList(counts=counts),
                             rowRanges=rowRanges, colData=colData)
  .ExtendedSummarisedExperiment(se, ExonMatrix=list(ExonMatrix=ExonMatrix), ExonDescription=ExonDescription,
                                VarientInfo=VarientInfo, IsoformMatrix=list(IsoformMatrix=IsoformMatrix),
                                IsoformDescription=IsoformDescription)
}

setValidity2("ExtendedSummarisedExperiment", function(object){
  msg <- NULL
  if(assayNames(object)[1] != "counts"){
    msg <- c(msg, "'counts' must be first assay")
  }
  if(min(assay(object))<0){
    msg <- c(msg, "ngative values in 'counts'")
  }
  for(matrix in object@ExonMatrix){
    if(!isEmpty(matrix)){
      if(min(matrix)<0){
        msg <- c(msg, "ngative values in 'ExonMatrix'")
      }
      if(!identical(rownames(matrix), rownames(object@ExonDescription))){
        msg <- c(msg, "rownames in 'ExonMatrix' and 'ExonDescription' must be identical")
      }
      if(!identical(colnames(matrix), rownames(object@colData))){
        msg <- c(msg, "colnames in 'ExonMatrix' and rownames in 'colData' must be identical")
      }
    }
  }
  for(matrix in object@IsoformMatrix){
    if(!isEmpty(matrix)){
      if(min(matrix)<0){
        msg <- c(msg, "ngative values in 'IsoformMatrix'")
      }
      if(!identical(rownames(matrix), rownames(object@IsoformDescription))){
        msg <- c(msg, "rownames in 'IsoformMatrix' and 'IsoformDescription' must be identical")
      }
      if(!identical(colnames(matrix), rownames(colData(object)))){
        msg <- c(msg, "colnames in 'IsoformMatrix' and rownames in 'colData' must be identical")
      }
    }
  }
  if(is.null(msg)){
    TRUE
  } else msg
})
