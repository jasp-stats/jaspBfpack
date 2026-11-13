#
# Copyright (C) 2013-2025 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# This is a generated file. Don't change it!

#' BFpack Multivariate t-Test
#'
#' @param bfType, Choose how the default fractional Bayes factor prior is centered for hypothesis testing.
#' \itemize{
#'   \item \code{"fractional"}: Uses the default fractional Bayes factor, with the fractional prior centered at the maximum likelihood estimate.
#'   \item \code{"adjusted"}: Uses the adjusted fractional Bayes factor, with the fractional prior centered at a null value.
#' }
#' @param ciLevel, Coverage level of the reported uncertainty interval, for example 95%.
#' @param complement, Include the complement hypothesis in the hypothesis set.
#'    Defaults to \code{TRUE}.
#' @param correlationSamplingMethod, Sampling algorithm used for posterior estimation of the correlation matrix.
#' \itemize{
#'   \item \code{"LKJ"}: Use the LKJ method to sample correlation matrices.
#'   \item \code{"LD"}: Use the Liu-Daniels method to sample correlation matrices.
#' }
#' @param estimatesTable, Show the parameter estimates table with uncertainty intervals.
#'    Defaults to \code{FALSE}.
#' @param excludeIntercept, Remove the intercept from the fitted model. This is often appropriate for ANOVA-style parameterizations.
#'    Defaults to \code{FALSE}.
#' @param interactionTerms, Available two-way interaction terms generated from the selected predictors, factors, or covariates.
#' @param iterationsBayesFactor, Number of iterations used when Bayes factors are computed by sampling.
#' @param iterationsEstimation, Number of posterior draws used to estimate the correlation model.
#' @param logScale, Report Bayes factors on the natural logarithm scale.
#'    Defaults to \code{FALSE}.
#' @param manualHypotheses, List of manually specified hypotheses. Use parameter names from the Parameters box, assign prior weights, and tick Include for the hypotheses that should be tested.
#' @param manualPlots, Plot the prior and posterior probabilities of the manual hypotheses.
#'    Defaults to \code{FALSE}.
#' @param muValue, Reference value used in the standard hypothesis test for the means of the selected variables.
#' @param nugget, Scaling factor used with the LD sampler to keep sampled correlations away from plus or minus one when needed.
#' @param priorPosteriorPlot, Plot the prior and posterior distributions of the sampled correlations.
#'    Defaults to \code{FALSE}.
#' @param priorPosteriorPlotAdditionalEstimationInfo, Add estimation summaries to the prior and posterior correlation plot.
#'    Defaults to \code{TRUE}.
#' @param priorPosteriorPlotAdditionalTestingInfo, Add testing summaries for the constrained correlation hypotheses to the plot.
#'    Defaults to \code{TRUE}.
#' @param priorProbComplement, Prior weight assigned to the complement hypothesis.
#' @param priorProbInteractionNonZero, Prior weight for the hypothesis that an interaction effect is present.
#' @param priorProbInteractionZero, Prior weight for the hypothesis that an interaction effect is absent.
#' @param priorProbMainNonZero, Prior weight for the hypothesis that a main effect is present.
#' @param priorProbMainZero, Prior weight for the hypothesis that a main effect is absent.
#' @param priorProbStandard, Prior weight for the first standard hypothesis. Larger values assign more prior probability to that hypothesis.
#' @param priorProbStandard2, Prior weight for the second standard hypothesis.
#' @param priorProbStandard3, Prior weight for the third standard hypothesis.
#' @param standardize, Standardize continuous variables before fitting the model.
#'    Defaults to \code{FALSE}.
#' @param tablesManualHypothesesComputationBfs, Show the table with fit, complexity, and Bayes factor components for the manual hypotheses.
#'    Defaults to \code{FALSE}.
#' @param tablesStandardHypothesesViewBfs, Show the Bayes factor table for the standard hypothesis test.
#'    Defaults to \code{FALSE}.
#' @param testValues, Set the reference mean for each selected variable in the standard hypothesis test.
#' @param traceplot, Plot the MCMC sampling traces for the correlations.
#'    Defaults to \code{FALSE}.
#' @param variables, Continuous variables whose means are tested simultaneously.
#' @param variances, Assume equal or unequal group variances in the independent samples t-test.
#' \itemize{
#'   \item \code{"equal"}: Assume the two groups have equal variances.
#'   \item \code{"unequal"}: Allow the two groups to have different variances. Bayes factors are then approximated by iterative sampling.
#' }
bfpackTTestMultiSample <- function(
          data = NULL,
          version = "1",
          bfType = "fractional",
          ciLevel = 0.95,
          complement = TRUE,
          correlationSamplingMethod = "LKJ",
          estimatesTable = FALSE,
          excludeIntercept = FALSE,
          hiddenNames = list(),
          interactionTerms = list(),
          iterationsBayesFactor = 10000,
          iterationsEstimation = 10000,
          logScale = FALSE,
          manualHypotheses = list(list(hypothesisText = "", includeHypothesis = FALSE, priorProbManual = 1, value = "#")),
          manualPlots = FALSE,
          muValue = 0,
          nugget = 0.999,
          plotHeight = 320,
          plotWidth = 480,
          priorPosteriorPlot = FALSE,
          priorPosteriorPlotAdditionalEstimationInfo = TRUE,
          priorPosteriorPlotAdditionalTestingInfo = TRUE,
          priorProbComplement = 1,
          priorProbInteractionNonZero = 1,
          priorProbInteractionZero = 1,
          priorProbMainNonZero = 1,
          priorProbMainZero = 1,
          priorProbStandard = 1,
          priorProbStandard2 = 1,
          priorProbStandard3 = 1,
          seed = 1,
          setSeed = FALSE,
          standardize = FALSE,
          tablesManualHypothesesComputationBfs = FALSE,
          tablesStandardHypothesesViewBfs = FALSE,
          testValues = list(),
          traceplot = FALSE,
          variables = list(types = list(), value = list()),
          variances = "equal") {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("hiddenNames", "interactionTerms", "manualHypotheses", "testValues", "variables")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspBfpack", "bfpackTTestMultiSample", "bfpackTTestMultiSample.qml", options, version, TRUE))
}