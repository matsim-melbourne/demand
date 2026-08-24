source("../../R/xml.R")

test_that("Converting to xml works", {
  set.seed(12345)
  plans<-read.csv('../expected/7.time/plan.csv')
  agentIds<-unique(plans$AgentId)
  households<-data.frame(
    AgentId=agentIds,
    HouseholdId=paste0('household_',ceiling(seq_along(agentIds)/2)),
    stringsAsFactors=FALSE
  )
  households$HouseholdSize<-ave(
    rep(1,nrow(households)),
    households$HouseholdId,
    FUN=sum
  )
  plans<-merge(plans,households,by='AgentId',sort=FALSE)
  plans<-plans[order(plans$PlanId,plans$StartBin,plans$EndBin),]
  plancsv<-'../actual/8.xml/plan.csv'
  outxml<-'../actual/8.xml/plan.xml'
  outdir<-'../actual/8.xml'
  dir.create(outdir, showWarnings = FALSE, recursive=TRUE)
  write.csv(plans,plancsv,row.names=FALSE)
  writeInterval <- 2 # write to file every so many plans
  capture_output(
    writePlanAsMATSimXML(plancsv, outxml, writeInterval)
  )
  expect_true(file.exists('../actual/8.xml/plan.xml'))
  xml<-xml2::read_xml('../actual/8.xml/plan.xml')
  people<-xml2::xml_find_all(xml,'//person')
  expect_equal(xml2::xml_attr(people,'id'),unique(plans$AgentId))
  expect_equal(length(xml2::xml_find_all(xml,'//leg')),nrow(plans)-length(people))
  expect_equal(
    xml2::xml_text(xml2::xml_find_all(people,'./attributes/attribute[@name="householdId"]')),
    households$HouseholdId[match(unique(plans$AgentId),households$AgentId)]
  )
})
