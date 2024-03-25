# Population Synthesis Data

This directory contains all the data files required for generating the MATSim demand, i.e., population with activities and trips, for Melbourne based on [ABS 2016](https://www.abs.gov.au/websitedbs/censushome.nsf/home/2016) and [VISTA 2012-2018](https://transport.vic.gov.au/about/data-and-research/vista).

Due to its size this data is stored outside this Git repository.

1. To download the data go to [this OSF location](https://osf.io/j56ux/),  and download it [like this](./download.PNG).

2. Once downloaded, extract the archive into this directory and unzip the `<file>.zip` files into subdirectories named `<files.zip>.dir`. 

When you are all done, the current directory should contain the following new files and sub-directories.
```
absRegionsReprojected.sqlite
addresses.csv.gz
distanceMatrix.rds
distanceMatrixIndex.csv
melbourne-2016-population.zip
melbourne-2016-population.zip.dir/
sa1_2016_aust.csv.gz
SA1attributed.csv.gz
SA1centroids.csv.gz
VISTA_12_18_CSV.zip
VISTA_12_18_CSV.zip.dir/
```
