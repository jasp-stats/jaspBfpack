context("BFpack Correlation")

options <- list(
  bfType = "fractional",
  ciLevel = 0.95,
  complement = TRUE,
  covariates = NULL,
  estimatesTable = TRUE,
  groupingVariable = "",
  interactionTerms = list(),
  iterations = 2000,
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

test_that("Posterior Probabilities Testing Standard Hypotheses table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_parameterTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("sales_with_adverts", 3.2523818943943e-17, 1, 1.970112450086e-19,
                                      "airplay_with_adverts", 0.732858674286794, 0.24521279472314,
                                      0.0219285309900667, "attract_with_adverts", 0.795994001804431,
                                      0.177390453724093, 0.0266155444714761, "airplay_with_sales",
                                      6.82455076924779e-20, 1, 3.78136116961891e-22, "attract_with_sales",
                                      0.000127637955847278, 0.999870947202821, 1.4148413319691e-06,
                                      "attract_with_airplay", 0.209195541455684, 0.786922306221254,
                                      0.00388215232306168))
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
                                 list("sales_with_adverts", 0.46704051616864, 0.573332065211305, 0.575438290345421,
                                      0.663573247187459, "airplay_with_adverts", -0.0438222812338494,
                                      0.10195303919801, 0.103024412567663, 0.242751465304876, "attract_with_adverts",
                                      -0.0629108708219813, 0.0811545055974596, 0.0811052387789591,
                                      0.223832790376795, "airplay_with_sales", 0.499042705946624,
                                      0.596887007982468, 0.598943367775295, 0.68190014744071, "attract_with_sales",
                                      0.19680268140778, 0.325042169991706, 0.324762124697871, 0.447264821056987,
                                      "attract_with_airplay", 0.0477288113948471, 0.182776793375218,
                                      0.183519671727313, 0.318195074794616))
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
                                 list(0, -43.7803579197175, -41.8042836114827, "H1", 43.7803579197175,
                                      0, 1.97607430823485, "H2", 41.8042836114827, -1.97607430823485,
                                      0, "H3"))
})

test_that("Posterior Model Probability table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H1", 8.51248306883392e-20, "H2", 0.878262058604186, "H3", 0.121737941395814
                                 ))
})

test_that("BFs: Manual Hypotheses table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_resultsContainer"]][["collection"]][["bfpackContainer_resultsContainer_specTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(3.43487257590829e-19, 3.43487257590829e-19, 1, 8.51248306883392e-20,
                                      0.334621459725668, 1, 1.1493820753221e-19, 1, "H1", 3.543875782385,
                                      1, 3.543875782385, 0.878262058604186, 1, 0.166666666666667,
                                      1, 0.590645963730834, "H2", 0.491224843522999, 1, 0.491224843522999,
                                      0.121737941395814, 1, 0.833333333333333, 1, 0.409354036269166,
                                      "H3"))
})

test_that("BFs: Standard Hypotheses table results match", {
  table <- results[["results"]][["bfpackContainer"]][["collection"]][["bfpackContainer_stdBfTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-37.9645589624074, -42.3778789639442, 0.693147180559945, -0.0263403560407537,
                                      -0.023597216860495, 1.44269504088896, "sales_with_adverts",
                                      1.00917505062488, -1.80684207372354, 0.607495735871319, 0.990908365581175,
                                      -0.553451801096928, 1.64610208920351, "airplay_with_adverts",
                                      1.36144225406301, -1.34350679203959, 0.553351040503281, 0.734515178308634,
                                      -0.74432076259316, 1.80717108454424, "attract_with_adverts",
                                      -44.1311753423318, -48.6336408227144, 0.693147180559945, -0.0226597182658938,
                                      -0.0205618987820659, 1.44269504088896, "airplay_with_sales",
                                      -8.96618512556549, -12.7752183393783, 0.693145765537002, -0.111530153124842,
                                      -0.0782765486612159, 1.44269798607984, "attract_with_sales",
                                      -1.32978130968784, -4.62351382646608, 0.688225973358955, -0.752003350261216,
                                      -0.216285716347546, 1.45301113109609, "attract_with_airplay"
                                 ))
})


# check posterior distribution plots
options <- list(
  bfType = "fractional",
  ciLevel = 0.95,
  complement = TRUE,
  covariates = list(
    types = list(),
    value = list()
  ),
  estimatesTable = FALSE,
  groupingVariable = "facGender",
  interactionTerms = list(),
  iterations = 2000,
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
