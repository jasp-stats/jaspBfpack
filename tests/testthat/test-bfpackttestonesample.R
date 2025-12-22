context("BFpack One Sample T-Test")

options <-
  list(
    bfType = "fractional",
    ciLevel = 0.1,
    estimatesTable = TRUE,
    complement = TRUE,
    interactionTerms = list(),
    iterationsEstimation = 5000,
    muValue = 0,
    logScale = TRUE,
    manualHypotheses = list(list(hypothesisText = "mu<.5", priorProbManual = "1/2", includeHypothesis = TRUE, value = "#")),
    manualPlots = FALSE,
    priorProbComplement = "1/2",
    seed = 100,
    tablesManualHypothesesComputationBfs = TRUE,
    priorProbStandard = "1",
    priorProbStandard2 = "1",
    priorProbStandard3 = "1",
    variables = "contNormal",
    standardize = FALSE,
    tablesStandardHypothesesViewBfs = FALSE
  )

set.seed(1)
results <- jaspTools::runAnalysis("bfpackTTestOneSample", "debug.csv", options, makeTests = F)


test_that("Coefficients table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("mu", -0.202082917611105, -0.18874858754, -0.18874858754, -0.175414257468895
                                 ))
})

test_that("Manual hypotheses legend table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("mu&lt;.5", "H1", "complement", "H2"))
})

test_that("Posterior probabilities when testing individual parameters table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("mu", 0.592794450958136, 0.019627260756729, 0.387578288285135
                                 ))
})

test_that("Evidence matrix (BFs) table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 19.4908635074636, "H1", -19.4908635074636, 0, "H2"))
})

test_that("Posterior model probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H1", 0.999999996570542, "H2", 3.42945832529895e-09))
})

test_that("Log BFs: Manual Hypotheses table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_specTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.379267220657277, 0, 0.379267220657277, -3.42945827879944e-09,
                                      0, -0.37926722223899, 0, -1.58171231792927e-09, "H1", -19.1115962868063,
                                      0, -19.1115962868063, -19.490863510893, 0, -1.15316154519548,
                                      0, -20.2647578320018, "H2"))
})
