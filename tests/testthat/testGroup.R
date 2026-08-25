library(tools) # for md5sum

source("../../R/group.R")

test_that("VISTA 2012-18 groupings work", {
  set.seed(12345)
  outdir<-'../actual/1.setup'
  dir.create(outdir, showWarnings = FALSE, recursive=TRUE)
  capture_output(
    make_groups(
      '../data/P_VISTA1218_V1.sample.csv',
      '../data/T_VISTA1218_V1.sample.csv',
      '../data/vistaCohorts.csv.gz',
      '../actual/1.setup',
      '../actual/1.setup/vista_2012_18_extracted_persons_weekday.csv.gz',
      'vista_2012_18_extracted_group_weekday_',
      'vista_2012_18_extracted_trips_weekday_',
      NULL, NULL, NULL # ignoring weekends
    )
  )
  files<-c(
    'vista_2012_18_extracted_persons_weekday.csv.gz',
    'vista_2012_18_extracted_group_weekday_1.csv',
    'vista_2012_18_extracted_group_weekday_2.csv',
    'vista_2012_18_extracted_group_weekday_3.csv',
    'vista_2012_18_extracted_group_weekday_4.csv',
    'vista_2012_18_extracted_group_weekday_5.csv',
    'vista_2012_18_extracted_trips_weekday_1.csv',
    'vista_2012_18_extracted_trips_weekday_2.csv',
    'vista_2012_18_extracted_trips_weekday_3.csv',
    'vista_2012_18_extracted_trips_weekday_4.csv',
    'vista_2012_18_extracted_trips_weekday_5.csv'
  )
  for (file in files) {
    expect_true(file.exists(paste0('../actual/1.setup/', file)))
    expect_true(md5sum(paste0('../actual/1.setup/', file)) == md5sum(paste0('../expected/1.setup/', file)))
  }

  roleFiles<-paste0(
    '../actual/1.setup/vista_2012_18_extracted_car_roles_weekday_',
    1:5,
    '.csv'
  )
  expect_true(all(file.exists(roleFiles)))
  roles<-do.call(rbind,lapply(roleFiles,read.csv,stringsAsFactors=FALSE))
  vistaTrips<-read.csv('../data/T_VISTA1218_V1.sample.csv',stringsAsFactors=FALSE)
  groupedPersonIds<-unique(unlist(lapply(
    paste0('../actual/1.setup/vista_2012_18_extracted_trips_weekday_',1:5,'.csv'),
    function(file) read.csv(file,stringsAsFactors=FALSE)$PERSID
  )))
  expectedTrips<-vistaTrips[
    vistaTrips$PERSID%in%groupedPersonIds &
      vistaTrips$LINKMODE%in%c('Vehicle Driver','Vehicle Passenger'),
  ]

  expect_equal(sort(roles$VistaTripId),sort(expectedTrips$TRIPID))
  expect_equal(
    roles$VistaCarRole[match(expectedTrips$TRIPID,roles$VistaTripId)],
    ifelse(expectedTrips$LINKMODE=='Vehicle Driver','driver','passenger')
  )
  expect_true(all(roles$VistaLinkMode%in%c('Vehicle Driver','Vehicle Passenger')))
})

test_that("VISTA driver and passenger roles remain leg-level", {
  vistaTrips<-data.frame(
    TRIPID=c('trip_1','trip_2'),
    PERSID=c('person_1','person_1'),
    HHID=c('household_1','household_1'),
    TRIPNO=1:2,
    STARTIME=c(480,1020),
    ARRTIME=c(500,1040),
    ORIGPURP1=c('At Home','Work Related'),
    DESTPURP1=c('Work Related','Go Home'),
    LINKMODE=c('Vehicle Driver','Vehicle Passenger'),
    WDTRIPWGT=c(1,1)
  )

  roles<-getVistaCarRoles(vistaTrips)

  expect_equal(roles$VistaPersonId,c('person_1','person_1'))
  expect_equal(roles$VistaTripId,c('trip_1','trip_2'))
  expect_equal(roles$VistaCarRole,c('driver','passenger'))
})

test_that("VISTA car roles retain weekday household context", {
  vistaTrips<-data.frame(
    TRIPID=paste0('trip_',1:5),
    PERSID=c('driver_1','passenger_1','passenger_2',
             'weekend_driver','weekend_passenger'),
    HHID=c('household_1','household_1','household_2',
           'household_3','household_3'),
    TRIPNO=1,
    STARTIME=c(480,485,500,600,605),
    ARRTIME=c(500,505,520,620,625),
    ORIGPURP1='At Home',
    DESTPURP1='Work Related',
    LINKMODE=c('Vehicle Driver','Vehicle Passenger','Vehicle Passenger',
               'Vehicle Driver','Vehicle Passenger'),
    WDTRIPWGT=1,
    stringsAsFactors=FALSE
  )
  vistaHouseholds<-data.frame(
    HHID=paste0('household_',1:3),
    TRAVDOW=c('Monday','Tuesday','Saturday'),
    DayType=c('Weekday','Weekday','Weekend Day'),
    CARS=c(1,0,1),
    FOURWDS=c(1,0,0),
    stringsAsFactors=FALSE
  )

  roles<-getVistaCarRoles(vistaTrips,vistaHouseholds,dayType='Weekday')

  expect_equal(roles$VistaTripId,paste0('trip_',1:3))
  expect_true(all(roles$VistaDayType=='Weekday'))
  expect_equal(roles$VistaTravelDay,c('Monday','Monday','Tuesday'))
  expect_equal(roles$VistaHouseholdCars,c(1L,1L,0L))
  expect_equal(roles$VistaHouseholdFourWheelDrives,c(1L,1L,0L))
  expect_equal(roles$VistaHouseholdHasDriverTrip,c(TRUE,TRUE,FALSE))
  expect_equal(roles$VistaHouseholdHasOtherDriverTrip,c(FALSE,TRUE,FALSE))
})
