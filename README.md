# MATSim population for Melbourne
`master`![passing?](https://github.com/matsim-melbourne/demand/workflows/build/badge.svg?branch=master) `dev`![passing?](https://github.com/matsim-melbourne/demand/workflows/build/badge.svg?branch=dev)

This script generates a sample population for Melbourne based on the [ABS 2016 census](https://www.abs.gov.au/websitedbs/censushome.nsf/home/2016) and using [VISTA-like](https://transport.vic.gov.au/about/data-and-research/vista) activities and trips.

## Setup R

The population generation code (in the `./R` directory) is written in [R](https://www.r-project.org) and a working knowledge of R is assumed here.

We use [`renv`](https://rstudio.github.io/renv/) to manage the R package dependencies for this project. To install the required R packages locally inside this repository, do:
```
install.packages("renv")
renv::restore()
```

Ensure that your setup is working by running some quick tests:
```
testthat::test_dir("tests/testthat")
```

If all tests pass, you are all set to produce a sample Melbourne population using the steps below.

## How to get the Melbourne Data

Download the required data files for generating the population and place them into `./data`. For download instructions see [`./data/README.md`](./data/README.md).

## How to build a sample Melbourne population

Here is an example of how to build a small sample population (0.1%) for Melbourne with census-like persons and VISTA-like activities and trips, for weekdays:
```
Rscript -e 'setwd("R"); source("makeExamplePopulation.R"); runexample()'
```

The script is quite verbose and takes a few minutes to run. If all went well you should get the MATSim population in `./output/8.xml/plan.xml`.

Population sampling is performed on complete synthetic households within each SA2. The requested percentage therefore controls the number of households, while the resulting percentage of persons may differ slightly. The sampled person file retains `HouseholdId` and `HouseholdSize` for downstream household coordination.

VISTA setup also writes `vista_2012_18_extracted_car_roles_weekday_*.csv` sidecar files. These retain the source trip, person and household identifiers plus timing, purpose and the leg-level `Vehicle Driver` or `Vehicle Passenger` role. They are source-trip inputs for household joint-travel coordination; they do not classify a VISTA person as a permanent driver or passenger and do not alter generated modes.

`R/householdJointTravel.R` can assign those observed roles to generated car legs using compatible source VISTA trips from the same demographic plan group. Matching prefers the same origin/destination purpose pair and departure-time window, then progressively relaxes purpose and time while remaining within the group. The output retains the source trip and match level for auditing. This step only labels existing car legs; it does not change their mode, timing, distance, origin or destination.

For the temporary household workflow, run the role assignment after the time step and use the resulting CSV as the input to the XML step:

```r
source("R/householdJointTravel.R")
assignVistaCarRolesToPlanFile(
  "output01/7.time/plan.csv",
  "output01/4.plan/plan2agent2group.csv",
  "output01/1.setup",
  "output01/7.time/plan.roles.csv",
  rseed=12345
)
```

Household identifiers are retained through the intermediate plan files and written as MATSim person attributes. MATSim person IDs use the stable synthetic `AgentId`. From the locate stage onward, `LegId` identifies the leg arriving at each non-initial activity, for example `213021342P1_leg_2`.

## How to build a sample population for inner Melbourne:

Here is an example of how to build a small sample population (0.1%) for inner Melbourne with census-like persons and VISTA-like activities and trips, for weekdays:
```
Rscript -e 'setwd("R"); source("makeExamplePopulation.R"); runexample(samplePercent=0.1,outputDir="example_inner_melbourne",sa1Subset="../data/smallRegion.csv,allDestinations=FALSE,do.steps=c(T,T,T,T,T,T,T,T))'
```

## Troubleshooting Windows installations

[RTools](https://cran.csiro.au/bin/windows/Rtools/) is required in order to compile some libraries.

Udunits2 may be required to get the sf library to work:
```
install.packages("udunits2")
```

If there are still issues with the sf package, try installing the development version:
```
install.packages("remotes")
library(remotes)
install_github("r-spatial/sf")
```
