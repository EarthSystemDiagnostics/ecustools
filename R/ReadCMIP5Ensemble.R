
##' Read the multimodel-ensemble of  CMIP5 climate model data
##' It expects a filelist (txt file with two columns, first column
##' model name e.g. CCSM4, second column number of ensemble members 8
##' and expects that the filenames are formatted in a simplified CMIP5 formatting except
##' PREFIX_modelname_MIDDLE_ensmemberNumber_SUFFIX
##' The perl scripts for processing the CMIP5 simulations to this format
##' are oder geonob1/ecus/pool/cmip5/scripts
##' 
##'
##' @title Read the multimodel-ensemble of  CMIP5 climate model data
##' @param path , [optional] path of the METAFILE named filelist.txt
##' @param bFullEnsemble FALSE: read only first member; TRUE:Read all members
##' @param METAFILE filename (and path) of the metafile (file list)
##' @param DATADIR Path of the netcdf files (default = path)
##' @param PREFIX prefix of the files e.g. "tas_Amon_"
##' @param MIDDLE   middle of the filename, e.g. "_historical_" or "_past1000_"
##' @param SUFFIX "suffix"
##' @param varname variable name to be read
##' @return list(cdat,meta)
##' cdat= List of pField objects
##' meta: List of vectors; name = unique name, modelName = modelName... ensMember = Ensemble member, here 1..100
##' @examples
##' #DO NOT RUN: past1000<-ReadCMIP5Ensemble(path="/model/CMIP5/past1000/tas.djfmam/",
##' #MIDDLE = "_past1000_",SUFFIX=".ncdjfmam")
##' @author Thomas Laepple
##' @export
ReadCMIP5Ensemble <- function(path = "", bFullEnsemble = TRUE, 
    METAFILE = paste(path, "filelist.txt", sep = ""), DATADIR = path, 
    PREFIX = "tas_Amon_", MIDDLE = "_historical_", SUFFIX = ".nc", 
                              varname = "tas")
{
    b.meta <- read.table(METAFILE, header = FALSE)
    meta <- list(names = vector(), modelName = vector(), ensMember = vector())
    cdat <- list()
    iRun <- 1
    for (i in 1:length(b.meta$V1)) {
        NRUN <- b.meta$V2[i]
        if (!bFullEnsemble) 
            NRUN = 1
        for (i.run in 1:NRUN) {
            if (i.run > 9) 
                s.i.run = i.run
            else s.i.run = paste("0", i.run, sep = "")
            filename <- paste(DATADIR, PREFIX, b.meta$V1[i], 
                MIDDLE, s.i.run, SUFFIX, sep = "")
            print(paste("Reading ", filename))
            cdat[[iRun]] <- read_data(filename, varname = varname)
            meta$names[iRun] <- paste(b.meta$V1[i], "_", i.run, 
                sep = "")
            meta$modelName[iRun] <- paste(b.meta$V1[i])
            meta$ensMember[iRun] <- i.run
            iRun <- iRun + 1
        }
    }
    return(list(cdat = cdat, meta = meta))
}
