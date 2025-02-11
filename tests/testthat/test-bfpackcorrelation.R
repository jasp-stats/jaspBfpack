context("BFpack Correlation")

options <- list(
  bfType = "fractional",
  ciLevel = 0.95,
  complement = TRUE,
  covariates = NULL,
  estimatesTable = TRUE,
  groupingVariable = "",
  interactionTerms = list(),
  iterationsEstimation = 2000,
  nugget = .995,
  standardize = FALSE,
  logScale = TRUE,
  manualHypotheses = list(
    list(
      hypothesisText = "sales_with_adverts = airplay_with_adverts = attract_with_adverts",
      includeHypothesis = TRUE,
      priorProbManual = "1",
      value = "#"
    ),
    list(
      hypothesisText = "sales_with_adverts > airplay_with_adverts > attract_with_adverts",
      includeHypothesis = TRUE,
      priorProbManual = "1",
      value = "#2"
    )
  ),
  manualHypothesisBfTable = TRUE,
  manualPlots = TRUE,
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
  priorProbStandard = "2",
  priorProbStandard2 = "1",
  priorProbStandard3 = "1",
  seed = 1,
  setSeed = FALSE,
  standardHypothesisBfTable = TRUE,
  traceplot = FALSE,
  variables = c("adverts", "sales", "airplay", "attract"),
  variables.types = c("scale", "scale", "scale", "ordinal"),
  variances = "equal"
)

set.seed(1)
results <- jaspTools::runAnalysis("bfpackCorrelation", testthat::test_path("sales.csv"), options, makeTests = F)

test_that("Posterior Probabilities of the Standard Hypotheses table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("sales_with_adverts", 3.63925815458126e-19, 1, 1.95450322482754e-21,
                                      "airplay_with_adverts", 0.754119020141021, 0.224255578578344,
                                      0.0216254012806351, "attract_with_adverts", 0.815121179506138,
                                      0.1582918525355, 0.0265869679583617, "airplay_with_sales", 2.81577651763816e-19,
                                      1, 1.56979601995476e-21, "attract_with_sales", 0.000103992786328404,
                                      0.999894918244598, 1.08896907338297e-06, "attract_with_airplay",
                                      0.274731578764012, 0.720044084944159, 0.00522433629182948))
})

test_that("Posterior Probabilities plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_probabilitiesPlotContainer"]][["collection"]][["bfpackContainer_probabilitiesPlotContainer_postPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "posterior-probabilities")
})

test_that("Prior Probabilities plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_probabilitiesPlotContainer"]][["collection"]][["bfpackContainer_probabilitiesPlotContainer_priorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-probabilities")
})

test_that("Estimates Table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("sales_with_adverts", 0.46850609728035, 0.567696755034827, 0.568484470153949,
                                      0.652328059899295, "airplay_with_adverts", -0.0489214020430954,
                                      0.0935448716917604, 0.0932004003059686, 0.225810810974835, "attract_with_adverts",
                                      -0.0539695056070947, 0.0726722206281893, 0.0729081662582253,
                                      0.20573165784248, "airplay_with_sales", 0.489278810174119, 0.586067084326948,
                                      0.588624456682559, 0.673096867967619, "attract_with_sales",
                                      0.189105103360261, 0.314730045767477, 0.314438140449582, 0.433992526051401,
                                      "attract_with_airplay", 0.0346531603465677, 0.170231597506475,
                                      0.169710786321016, 0.305871521447342))
})

test_that("Manual Hypotheses Legend table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("sales_with_adverts=airplay_with_adverts=attract_with_adverts",
                                      "H1", "sales_with_adverts&gt;airplay_with_adverts&gt;attract_with_adverts",
                                      "H2", "complement", "H3"))
})

test_that("Evidence Matrix (log BFs) table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_matrixTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, -43.9605581486528, -41.9812957745389, "H1", 43.9605581486528,
                                      0, 1.97926237411396, "H2", 41.9812957745389, -1.97926237411396,
                                      0, "H3"))
})

test_that("Posterior Model Probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H1", 7.11155566337474e-20, "H2", 0.878602508796586, "H3", 0.121397491203414
                                 ))
})

test_that("BFs: Manual Hypotheses table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_specTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2.8722147423499e-19, 2.8722147423499e-19, 1, 7.11155566337474e-20,
                                      0.334621459725668, 1, 9.61104689730706e-20, 1, "H1", 3.54849936903064,
                                      1, 3.54849936903064, 0.878602508796586, 1, 0.166666666666667,
                                      1, 0.591416561505107, "H2", 0.490300126193871, 1, 0.490300126193871,
                                      0.121397491203414, 1, 0.833333333333333, 1, 0.408583438494893,
                                      "H3"))
})

test_that("BFs: Standard Hypotheses table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_stdBfTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.0163257338074273, -0.0147506357758907, "", "sales_with_adverts",
                                      "", -0.644885543670207, 1.86446312743751, "airplay_with_adverts",
                                      "", -1.19059290781714, 2.75826697832401, "attract_with_adverts",
                                      -0.0162276805985185, -0.0146821506243353, "", "airplay_with_sales",
                                      -0.0755795077210352, -0.0531674930011395, "", "attract_with_sales",
                                      -0.706587401559197, -0.161770513780311, "", "attract_with_airplay"
                                 ))
})

# check posterior distribution plots
options <- list(
  bfType = "fractional",
  ciLevel = 0.95,
  standardize = FALSE,
  complement = TRUE,
  covariates = list(
    types = list(),
    value = list()
  ),
  estimatesTable = FALSE,
  groupingVariable = "facGender",
  interactionTerms = list(),
  iterationsEstimation = 2000,
  logScale = FALSE,
  manualHypotheses = list(
    list(
      hypothesisText = "",
      includeHypothesis = FALSE,
      priorProbManual = "1",
      value = "#"
    )
  ),
  manualPlots = FALSE,
  muValue = 0,
  nugget = .995,
  plotHeight = 320,
  plotWidth = 480,
  priorPosteriorPlot = TRUE,
  priorPosteriorPlotAdditionalEstimationInfo = TRUE,
  priorPosteriorPlotAdditionalTestingInfo = TRUE,
  priorProbComplement = "1",
  priorProbInteractionNonZero = "1",
  priorProbInteractionZero = "1",
  priorProbMainNonZero = "1",
  priorProbMainZero = "1",
  priorProbStandard = "2",
  priorProbStandard2 = "1",
  priorProbStandard3 = "1",
  seed = 100,
  manualHypothesisBfTable = FALSE,
  standardHypothesisBfTable = FALSE,
  traceplot = TRUE,
  variables =  c("contNormal", "contGamma", "contExpon")
  )

set.seed(1)
results <- jaspTools::runAnalysis("bfpackCorrelation", "debug.csv", options, makeTests = F)

test_that("contGamma_with_contNormal_in_g1 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer_cor1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contgamma_with_contnormal_in_g1")
})

test_that("contExpon_with_contNormal_in_g1 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer_cor2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contexpon_with_contnormal_in_g1")
})

test_that("contExpon_with_contGamma_in_g1 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer_cor3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contexpon_with_contgamma_in_g1")
})

test_that("contGamma_with_contNormal_in_g2 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer_cor4"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contgamma_with_contnormal_in_g2")
})

test_that("contExpon_with_contNormal_in_g2 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer_cor5"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contexpon_with_contnormal_in_g2")
})

test_that("contExpon_with_contGamma_in_g2 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer"]][["collection"]][["bfpackContainer_posteriorPlotContainer_cor6"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contexpon_with_contgamma_in_g2")
})

test_that("contGamma_with_contNormal_in_g1 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_traceplotContainer"]][["collection"]][["bfpackContainer_traceplotContainer_cor1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contgamma_with_contnormal_in_g1-trace")
})

test_that("contExpon_with_contNormal_in_g1 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_traceplotContainer"]][["collection"]][["bfpackContainer_traceplotContainer_cor2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contexpon_with_contnormal_in_g1-trace")
})

test_that("contExpon_with_contGamma_in_g1 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_traceplotContainer"]][["collection"]][["bfpackContainer_traceplotContainer_cor3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contexpon_with_contgamma_in_g1-trace")
})

test_that("contGamma_with_contNormal_in_g2 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_traceplotContainer"]][["collection"]][["bfpackContainer_traceplotContainer_cor4"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contgamma_with_contnormal_in_g2-trace")
})

test_that("contExpon_with_contNormal_in_g2 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_traceplotContainer"]][["collection"]][["bfpackContainer_traceplotContainer_cor5"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contexpon_with_contnormal_in_g2-trace")
})

test_that("contExpon_with_contGamma_in_g2 plot matches", {
  plotName <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_traceplotContainer"]][["collection"]][["bfpackContainer_traceplotContainer_cor6"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "contexpon_with_contgamma_in_g2-trace")
})
