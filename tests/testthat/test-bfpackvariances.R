context("BFpack Variances")


options <- list(
  bfType = "fractional",
  ciLevel = 0.95,
  complement = TRUE,
  estimatesTable = TRUE,
  groupingVariable = "facGender",
  groupingVariable.types = "nominal",
  interactionTerms = list(),
  iterationsBayesFactor = 10000,
  iterationsEstimation = 5000,
  logScale = FALSE,
  manualHypotheses = list(
    list(
      hypothesisText = "",
      includeHypothesis = FALSE,
      priorProbManual = "1",
      value = "#"
    )
  ),
  manualHypothesisBfTable = FALSE,
  manualPlots = FALSE,
  muValue = 0,
  plotHeight = 320,
  plotWidth = 480,
  priorPosteriorPlot = FALSE,
  priorPosteriorPlotAdditionalEstimationInfo = TRUE,
  priorPosteriorPlotAdditionalTestingInfo = TRUE,
  priorProbComplement = "1",
  priorProbInteractionNonZero = "1",
  priorProbInteractionZero = "1",
  priorProbMainNonZero = "1",
  priorProbMainZero = "1",
  priorProbStandard = "1",
  priorProbStandard2 = "1",
  priorProbStandard3 = "1",
  seed = 1,
  setSeed = FALSE,
  standardHypothesisBfTable = FALSE,
  traceplot = FALSE,
  variables = "contNormal",
  variables.types = "scale",
  variances = "equal",
  standardize = FALSE
)

set.seed(1)
results <- runAnalysis("bfpackVariances", "debug.csv", options, makeTests = F)

test_that("Posterior Probabilities Testing Standard Hypotheses table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.767286393957479, 0.232713606042521))
})

test_that("Estimates Table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("f", 0.601384535211166, 0.898524969022479, 0.873708488830913,
                                      1.33832309757562, "m", 0.900914395996814, 1.34605070858815,
                                      1.3088739556896, 2.00489782245158))
})
