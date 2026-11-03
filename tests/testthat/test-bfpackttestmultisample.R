context("BFpack Multivariate One Sample t-Test")

options <-
    list(
      bfType = "fractional",
      ciLevel = 0.95,
      estimatesTable = FALSE,
      complement = TRUE,
      interactionTerms = list(),
      iterationsEstimation = 5000,
      iterationsBayesFactor = 10000,
      logScale = FALSE,
      manualHypotheses = list(
        list(
          hypothesisText = "contNormal>contGamma",
          priorProbManual = "1/2",
          includeHypothesis = TRUE,
          value = "#"
        )
      ),
      muValue = 0,
      manualPlots = FALSE,
      priorProbComplement = "1/2",
      seed = 100,
      tablesManualHypothesesComputationBfs = FALSE,
      priorProbStandard = "1",
      priorProbStandard2 = "1",
      testValues = list(
        list(testValue = 1, value = "contNormal"),
        list(testValue = 0, value = "contGamma")
      ),
      variables = c("contNormal", "contGamma"),
      tablesStandardHypothesesViewBfs = FALSE,
      standardize = FALSE
)



set.seed(1)
results <- jaspTools::runAnalysis("bfpackTTestMultiSample", "debug.csv", options)


test_that("Posterior probabilities when testing individual parameters table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2.58215607191321e-27, 1))
})

test_that("Evidence matrix (BFs) table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 9.23271288425205e-20, "H1", 10831052720221249536, 1, "H2"
                                 ))
})

test_that("Posterior model probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H1", 9.23271288425205e-20, "H2", 1))
})

# "View BFs" table: H± vs. H0 is the reciprocal of H0 vs. H±, i.e. the negative on the log scale
for (logScale in c(FALSE, TRUE)) {
  test_that(paste0("Standard hypotheses View BFs table: second BF is the reciprocal (logScale = ", logScale, ")"), {
    optionsViewBfs <- options
    optionsViewBfs$tablesStandardHypothesesViewBfs <- TRUE
    optionsViewBfs$logScale <- logScale
    set.seed(1)
    resultsViewBfs <- jaspTools::runAnalysis("bfpackTTestMultiSample", "debug.csv", optionsViewBfs)
    table <- resultsViewBfs[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_stdBfTable"]][["data"]]
    expect_length(table, 1)
    expected <- if (logScale) -table[[1]][["bf1"]] else 1 / table[[1]][["bf1"]]
    expect_equal(table[[1]][["bf2"]], expected)
  })
}
