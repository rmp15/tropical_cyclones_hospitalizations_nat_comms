# look at http://hmdc.github.io/rce-interactive-tools/cmd/using_the_rce_cmd_client.html
# for how to install using command line in personal directory

# first type in below to open interactive R command line sessions
# rce_submit.py -r -a R

# packages to install
# more information about NSAPHutils via https://rdrr.io/github/NSAPH/NSAPHutils/
install.packages("remotes")
remotes::install_github("NSAPH/NSAPHutils") # this is loaded confusingly via library('NSAPHutils')
install.packages(fst)
install.packages(icd)

# for coastal storm stuff
install.packages("drat") # Only run if you don't have `drat`
library("drat")
addRepo("geanders")
install.packages("hurricaneexposuredata")