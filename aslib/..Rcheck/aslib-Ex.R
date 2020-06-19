pkgname <- "aslib"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('aslib')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("getCosealASScenario")
### * getCosealASScenario

flush(stderr()); flush(stdout())

### Name: getCosealASScenario
### Title: Retrieves a scenario from the Coseal Github repository and
###   parses into an S3 object.
### Aliases: getCosealASScenario

### ** Examples

## Not run: 
##D   sc = getCosealASScenario("CSP-2010")
## End(Not run)



cleanEx()
nameEx("parseASScenario")
### * parseASScenario

flush(stderr()); flush(stdout())

### Name: parseASScenario
### Title: Parses the data files of an algorithm selection scenario into an
###   S3 object.
### Aliases: ASScenario parseASScenario

### ** Examples

## Not run: 
##D   sc = parseASScenario("/path/to/scenario")
## End(Not run)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
