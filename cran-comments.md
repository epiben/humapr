## Test environments
* local OS X install (Catalina 10.15), R 3.6.3
* ubuntu 16.04.6 LTS (on travis-ci), R 3.6.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was one NOTE (only in win-builder):
* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Benjamin Skov Kaas-Hansen <benskov@gmail.com>'
  
  New submission
  
  Possibly mis-spelled words in DESCRIPTION:
    ggplot (9:30)
    
  COMMENTS:
   - This is my first submission.
   - DESCRIPTION mentions ggplot2 twice but never just ggplot. Not sure why this NOTE appears.

## DOWNSTREAM DEPENDENCIES
There are currently no downstream dependencies for this package.