#!/bin/sh
echo "Start P4"
Rscript P4DeJong1975.R > P4log.txt
echo "Start UPC4"
Rscript UPC4DeJong1975.R > UPC4log.txt
echo "Start IV4"
Rscript IV4DeJong1975.R > IV4log.txt
echo "Done!"
