context("BFpack Independent T-Test")

options <-
  list(
    bfType = "fractional",
    ciLevel = 0.95,
    estimatesTable = FALSE,
    complement = FALSE,
    groupingVariable = "facExperim",
    groupingVariable.types = "nominal",
    interactionTerms = list(),
    variances = "unequal",
    iterationsBayesFactor = 1000000,
    iterationsEstimation = 5000,
    muValue = 0,
    logScale = TRUE,
    manualHypotheses = list(
      list(hypothesisText = "difference > 3", priorProbManual = "1/2", includeHypothesis = TRUE, value = "#"),
      list(hypothesisText = "difference < 0", priorProbManual = "1/2", includeHypothesis = TRUE, value = "#2"),
      list(hypothesisText = "difference < 3", priorProbManual = "1/2", includeHypothesis = TRUE, value = "#3")
    ),

    manualPlots = TRUE,
    priorProbComplement = "1/2",
    seed = 100,
    tablesManualHypothesesComputationBfs = TRUE,
    priorProbStandard = "1",
    priorProbStandard2 = "1",
    priorProbStandard3 = "1",
    variables = "contNormal",
    variables.types = "scale",
    standardize = FALSE,
    tablesStandardHypothesesViewBfs = FALSE
  )

debug <- read.csv("https://raw.githubusercontent.com/jasp-stats/jasp-desktop/development/Resources/Data%20Sets/debug.csv")
dt <- debug[, c("contNormal", "facExperim")]
dt$facExperim <- as.factor(dt$facExperim)
set.seed(1)
results <- jaspTools::runAnalysis("bfpackTTestIndependentSamples", dt, options, makeTests = F)

test_that("Manual hypotheses legend table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("difference&gt;3", "H1", "difference&lt;0", "H2", "difference&lt;3",
                                      "H3"))
})

test_that("Posterior probabilities when testing individual parameters table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("difference", 0.850906132369181, 0.0562612136566447, 0.0928326539741745
                                 ))
})

test_that("Posterior probabilities plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_probabilitiesPlotContainer"]][["collection"]][["bfpackContainer_probabilitiesPlotContainer_postPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "posterior-probabilities")
})

test_that("Prior probabilities plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_probabilitiesPlotContainer"]][["collection"]][["bfpackContainer_probabilitiesPlotContainer_priorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-probabilities")
})

test_that("Evidence matrix (BFs) table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, "-<unicode>", "-<unicode>", "H1", "<unicode>", 0, 0.00526177151092475,
                                      "H2", "<unicode>", -0.00526177151092475, 0, "H3"))
})

test_that("Posterior model probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H1", 0, "H2", 0.501315439842767, "H3", 0.498684560157233))
})

test_that("Log BFs: Manual Hypotheses table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_specTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("-<unicode>", 0, "-<unicode>", "-<unicode>", 0, -1.66989053808205,
                                      0, "-<unicode>", "H1", 0.21384641079905, 0, 0.21384641079905,
                                      -0.69051975558042, 0, -0.670819403640925, 0, -0.456972992841875,
                                      "H2", 0.208584639288125, 0, 0.208584639288125, -0.695781527091345,
                                      0, -0.208584639288125, 0, 0, "H3"))
})
