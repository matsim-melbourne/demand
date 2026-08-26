# MATSim population for Melbourne
`master` [![master build](https://github.com/matsim-melbourne/demand/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/matsim-melbourne/demand/actions/workflows/ci.yml?query=branch%3Amaster) `dev` [![dev build](https://github.com/matsim-melbourne/demand/actions/workflows/ci.yml/badge.svg?branch=dev)](https://github.com/matsim-melbourne/demand/actions/workflows/ci.yml?query=branch%3Adev)

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

`R/householdJointTravel.R` can assign those observed roles to generated car legs using compatible source VISTA trips from the same demographic plan group. Matching prefers the same origin/destination purpose pair and departure-time window, then progressively relaxes purpose and time while remaining within the group. A VISTA passenger trip also records whether a different member of the surveyed household reported a driver trip on the same survey day. When that household context is transferred to a generated passenger leg, the temporary coordination step ensures that a different person in the generated household has a driver leg. If this cannot be done, the passenger leg is resampled from VISTA passenger trips that do not require another household driver. The initial role, final role, source household context and any adjustment are retained for auditing.

This temporary constraint can change a generated car leg between driver and passenger, but it does not change the existing car mode, trip frequency, purpose, timing, distance, origin or destination. It does not claim that the paired driver leg is feasible for that particular passenger leg; feasible timing and route combinations are provided separately as candidates.

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

The role-labelled plan can then be used to write the possible household joint-travel matches:

```r
writeHouseholdJointTravelCandidates(
  "output01/7.time/plan.roles.csv",
  "output01/7.time/household-joint-travel-candidates.csv"
)
```

Each candidate links one passenger leg to one compatible driver leg in the same household. The output includes both participants, their time windows, the estimated shared route section and the required passenger seat and vehicle capacity. Several rows may use the same driver leg, allowing that driver to carry multiple passengers, and a passenger leg may appear with several possible drivers. No final driver or vehicle is selected and the population plan is not modified.

This temporary implementation estimates route compatibility from straight lines between the generated activity coordinates. The default tolerances are 30 minutes and 1,000 metres; both can be changed when writing the candidates. Network-route overlap, household scheduling and final vehicle allocation are intentionally left for the later coordination model.

For an end-to-end validation with summary tables and plots, source `R/validateHouseholdJointTravel.R` and run `runHouseholdJointTravelValidation()`. The outputs compare the weighted VISTA household-driver target with generated passenger coverage before and after the household constraint, count each role adjustment, and report the stricter timing and route candidate coverage separately.

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
