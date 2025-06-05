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
                                 list("sales_with_adverts", 1.25965242354417e-20, 1, 6.30047090958772e-23,
                                      "airplay_with_adverts", 0.764067303703275, 0.214172189292728,
                                      0.0217605070039969, "attract_with_adverts", 0.81103151830978,
                                      0.162734632999874, 0.0262338486903459, "airplay_with_sales",
                                      6.0552627054929e-19, 1, 3.41252975927948e-21, "attract_with_sales",
                                      0.000105124939115191, 0.999893765316547, 1.10974433775939e-06,
                                      "attract_with_airplay", 0.292836501058348, 0.701426532422573,
                                      0.00573696651907904))
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
                                 list("sales_with_adverts", 0.477735792808419, 0.568548538908731, 0.568536812578564,
                                      0.65118339386032, "airplay_with_adverts", -0.0382877935474967,
                                      0.0900218751790515, 0.089600655323082, 0.220152171556741, "attract_with_adverts",
                                      -0.058672006706562, 0.0744079433170593, 0.0735699408188674,
                                      0.211475076254999, "airplay_with_sales", 0.487417447406021,
                                      0.583270983838333, 0.584832395647687, 0.671286339854072, "attract_with_sales",
                                      0.193386143277419, 0.316622922762359, 0.316114275737559, 0.438099554775709,
                                      "attract_with_airplay", 0.0318182003949569, 0.169842607852354,
                                      0.168460989305395, 0.306349129258222))
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
                                 list(0, -47.8301810840189, -45.9392756876506, "H1", 47.8301810840189,
                                      0, 1.89090539636835, "H2", 45.9392756876506, -1.89090539636835,
                                      0, "H3"))
})

test_that("Posterior Model Probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H1", 1.4674572401231e-21, "H2", 0.868858728812906, "H3", 0.131141271187094
                                 ))
})

test_that("Log BFs: Manual Hypotheses table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_specTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(5.77524930146998e-21, 5.77524930146998e-21, 1, 1.4674572401231e-21,
                                      0.334621459725668, 1, 1.93252235153753e-21, 1, "H1", 3.41943576238849,
                                      1, 3.41943576238849, 0.868858728812906, 1, 0.166666666666667,
                                      1, 0.569905960398082, "H2", 0.516112847522302, 1, 0.516112847522302,
                                      0.131141271187094, 1, 0.833333333333333, 1, 0.430094039601918,
                                      "H3"))
})

test_that("Log BFs: Standard Hypotheses table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_stdBfTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.0151273260546063, -0.0137459148563934, "", "sales_with_adverts",
                                      "", -0.695206436046034, 1.97040057613332, "airplay_with_adverts",
                                      "", -1.13683980208192, 2.67934406984223, "attract_with_adverts",
                                      -0.0165238880870198, -0.0149276794908004, "", "airplay_with_sales",
                                      -0.0756688545762342, -0.0532446784650983, "", "attract_with_sales",
                                      -0.776956186969178, -0.166214715930709, "", "attract_with_airplay"
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
