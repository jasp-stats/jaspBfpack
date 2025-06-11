context("BFpack Paired Samples T-Test")

options <-
    list(
      bfType = "fractional",
      ciLevel = 0.95,
      estimatesTable = TRUE,
      complement = TRUE,
      interactionTerms = list(),
      iterationsEstimation = 5000,
      logScale = FALSE,
      muValue = 0,
      manualHypotheses = list(
        list(hypothesisText = ".1<difference<2", priorProbManual = "1/2", includeHypothesis = TRUE, value = "#"),
        list(hypothesisText = "difference=2", priorProbManual = "1/2", includeHypothesis = TRUE, value = "#2")
      ),
      pairs = list(c("contNormal", "contGamma")),
      manualPlots = TRUE,
      priorProbComplement = "1/2",
      seed = 100,
      manualHypothesisBfTable = FALSE,
      priorProbStandard = "1",
      priorProbStandard2 = ".5",
      priorProbStandard3 = "1",
      standardHypothesisBfTable = FALSE,
      standardize = FALSE
    )



set.seed(1)
results <- jaspTools::runAnalysis("bfpackTTestPairedSamples", "debug.csv", options, makeTests = F)

test_that("Estimates table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("difference", -2.60134149720009, -2.22170938375, -2.22170938375,
                                      -1.84207727029991))
})

test_that("Posterior probabilities when testing individual parameters table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("difference", 9.8559713484495e-18, 1.19550749899889e-19, 1))
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

test_that("Evidence Matrix (BFs) table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 0, 0, "H1", "<unicode>", 1, 1.65949117110234e-37, "H2", "<unicode>",
                                      6.02594347842016e+36, 1, "H3"))
})

test_that("Posterior model probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H1", 1.01119137876107e-32, "H2", 1.57145592029229e-37, "H3",
                                      1))
})
