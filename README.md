Install with R commands

install.packages("devtools")
devtools::install_github("martingraham/refimpact")


dependency openNLPmodels.en not available on CRAN, install similarly with R command

install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")

See http://datacube.wu.ac.at


Settings needed to change default locations of reference files etc so it works (which it won't unless you're on my computer)

here's an example:
defaultSettings <- list (
    dirName = "C:/Martin/Data/Impact/REF submission documents/Case Studies",
    boilerPlateTerms = c(
      "Page",
      "Impact case study (REF3b)",
      "Title of case study:",
      "(indicative maximum 100 words)",
      "(indicative maximum 500 words)",
      "(indicative maximum of six references)",
      "(indicative maximum 750 words)",
      "(indicative maximum of 10 references)"
    ),
    sectionHeadings = c(
      "Summary of the impact",
      "Underpinning research",
      "References to the research",
      "Details of the impact",
      "Sources to corroborate the impact|Sources to corroborate impact|References to corroborate the impact" 
    ),
    impactSettings = list(cname="C:/Martin/Data/Impact/REF2014-Results-xlsx.csv", uoa=19, profile="Impact"),
    helperFiles = list(buzzFile1="C:/Martin/Data/Impact/buzz.txt", buzzFile2="C:/Martin/Data/Impact/busibuzz.txt"),
    topicCount = 10,
    topicIter = 60,
    keepSpaces = FALSE,
    places = TRUE,
    orgs = TRUE
  )

then run

fromTheTop(mySettings=defaultSettings)

