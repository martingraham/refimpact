Install the package with these R commands:
```
install.packages("devtools")
devtools::install_github("martingraham/refimpact")
```


The dependency openNLPmodels.en not available on CRAN, install this with:
```
install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
```
See http://datacube.wu.ac.at

If you are trying to import pdfs myou will need to install the separate xpdf software from here:
http://www.foolabs.com/xpdf/home.html
and then add the xpdf.exe to your computer's PATH variable so R can call it. If you're using text docs you're fine.


The location of the case study files needs to be stated. The example below shows all the values that can be changed and passed into the R package, but the dirName='WhereYourCaseStudiesAre' is the only one that needs to be set for certain.

here's an example:
```
mySettings <- list (
    dirName = "C:/*YourDir*/Case Studies",
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
    impactSettings = list(uoa=19, profile="Impact"),
    helperFiles = list(buzzFile1="C:/*YourDir*/buzz.txt", buzzFile2="C:/*YourDir*/busibuzz.txt"),
    doProperCorpus = TRUE,
    topicCount = 10,
    topicIter = 60,
    keepSpaces = FALSE,
    places = TRUE,
    orgs = TRUE,
    people = FALSE
  )
```
then run

`fromTheTop (mySettings)`

