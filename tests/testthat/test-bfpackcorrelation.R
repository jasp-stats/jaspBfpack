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
                                 list("sales_with_adverts", 1.77942563859518e-20, 1, 8.92961235286475e-23,
                                      "airplay_with_adverts", 0.766948560255952, 0.211092073274922,
                                      0.0219593664691263, "attract_with_adverts", 0.813884824859506,
                                      0.159564264441074, 0.02655091069942, "airplay_with_sales", 6.41121035546808e-19,
                                      1, 3.61451462181043e-21, "attract_with_sales", 0.000120557087633689,
                                      0.999878163322489, 1.27958987778277e-06, "attract_with_airplay",
                                      0.308603140108504, 0.685289204304595, 0.00610765558690174))
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
                                 list(0, -47.581294238802, -45.683831705795, "H1", 47.581294238802,
                                      0, 1.89746253300694, "H2", 45.683831705795, -1.89746253300694,
                                      0, "H3"))
})

test_that("Posterior Model Probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H1", 1.88377067290424e-21, "H2", 0.869604064811951, "H3", 0.130395935188049
                                 ))
})

test_that("Log BFs: Manual Hypotheses table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_specTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(7.42819717029706e-21, 7.42819717029706e-21, 1, 1.88377067290424e-21,
                                      0.334621459725668, 1, 2.48563418025488e-21, 1, "H1", 3.42907475226594,
                                      1, 3.42907475226594, 0.869604064811951, 1, 0.166666666666667,
                                      1, 0.57151245871099, "H2", 0.514185049546813, 1, 0.514185049546813,
                                      0.130395935188049, 1, 0.833333333333333, 1, 0.42848754128901,
                                      "H3"))
})

test_that("Log BFs: Standard Hypotheses table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_stdBfTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.0152422410688517, -0.0138416449257139, "", "sales_with_adverts",
                                      "", -0.713732101660131, 2.00470522770377, "airplay_with_adverts",
                                      "", -1.17645249461639, 2.73623129725681, "attract_with_adverts",
                                      -0.0165464191347455, -0.0149461889223048, "", "airplay_with_sales",
                                      -0.0768176240163965, -0.0538336345865915, "", "attract_with_sales",
                                      -0.848282977106952, -0.169541651077943, "", "attract_with_airplay"
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
