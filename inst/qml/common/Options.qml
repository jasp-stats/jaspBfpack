//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

Section
{
	property bool bfTy: true
	property bool iterationsBf: false
	property var iterationsBfDefaultNumber: 10000
	property bool interactions: false
	property bool anova: false
	property bool variances: false
	property var interactionValues: []
	property bool correlation: false
	property bool intercept: false

	id: options
	title: 	qsTr("Options")
	info: qsTr("Configure Bayes factor reporting, tables, plots, uncertainty intervals, sampling settings, and model-specific options.")

	Group
	{
		title: qsTr("Bayes Factor")
		// Layout.rowSpan: 2

		CheckBox
		{
			name: "logScale"
			label: qsTr("Log scale")
			info: qsTr("Report Bayes factors on the natural logarithm scale.")
		}

		RadioButtonGroup
		{
			visible: bfTy
			name: "bfType"
			title: qsTr("Bayes factor type")
			info: qsTr("Choose how the default fractional Bayes factor prior is centered for hypothesis testing.")
			radioButtonsOnSameRow: false
			RadioButton { value: "fractional"; 	label: qsTr("Fractional Bayes factor"); checked: true; info: qsTr("Uses the default fractional Bayes factor, with the fractional prior centered at the maximum likelihood estimate.") }
			RadioButton { value: "adjusted"; 		label: qsTr("Adjusted fractional Bayes factor"); info: qsTr("Uses the adjusted fractional Bayes factor, with the fractional prior centered at a null value.") }

		}
	}

	Group
	{
		title: 	qsTr("Tables")
		info: qsTr("Select which result tables should be included in the output.")

		CheckBox 
		{
			id: tablesStandardHypothesesViewBfs
			name: "tablesStandardHypothesesViewBfs"
			text: qsTr("Standard hypotheses: View BFs")
			info: qsTr("Show the Bayes factor table for the standard hypothesis test.")
		}	

		CheckBox 
		{
			id: tablesManualHypothesesComputationBfs
			name: "tablesManualHypothesesComputationBfs"
			text: qsTr("Manual hypotheses: Computation BFs")
			info: qsTr("Show the table with fit, complexity, and Bayes factor components for the manual hypotheses.")
		}

		CheckBox
		{
			name: 						"estimatesTable"
			text: 						qsTr("Estimates")
			info: qsTr("Show the parameter estimates table with uncertainty intervals.")
		}
	}

	Group
	{
		
		title: qsTr("Plots")
		info: qsTr("Select which plots should be included in the output.")
		CheckBox
		{
			name: 						"manualPlots"
			text: 						qsTr("Manual hypotheses plots")
			info: qsTr("Plot the prior and posterior probabilities of the manual hypotheses.")
		}
		CheckBox
		{
			visible: 					correlation
			name: 						"priorPosteriorPlot"
			text: 						qsTr("Prior and posterior plot")
			info: qsTr("Plot the prior and posterior distributions of the sampled correlations.")
			CheckBox {	name: "priorPosteriorPlotAdditionalEstimationInfo";	label: qsTr("Estimation info");		checked: true; info: qsTr("Add estimation summaries to the prior and posterior correlation plot.") }
			CheckBox {	name: "priorPosteriorPlotAdditionalTestingInfo";		label: qsTr("Testing info");		checked: true; info: qsTr("Add testing summaries for the constrained correlation hypotheses to the plot.") }
		}
		CheckBox
		{
			visible: 					correlation
			name: 						"traceplot"
			text: 						qsTr("Traceplot")
			info: qsTr("Plot the MCMC sampling traces for the correlations.")
		}
	}

	Group
	{
		title: 							qsTr("Additional Options")
		info: qsTr("Set interval levels, standardization, sampling details, and other model-specific analysis settings.")
		CIField
		{
			label: qsTr("Uncertainty interval level")
			name: 					"ciLevel"
			info: qsTr("Coverage level of the reported uncertainty interval, for example 95%.")
		}
		CheckBox
		{
			visible: !correlation
			name: "standardize"
			text: qsTr("Standardize continuous variables")
			info: qsTr("Standardize continuous variables before fitting the model.")
		}
		RadioButtonGroup
		{
			visible: variances
			id: variancesId
			title: qsTr("Variances")
			name: "variances"
			info: qsTr("Assume equal or unequal group variances in the independent samples t-test.")
			radioButtonsOnSameRow: true
			RadioButton { value: "equal"; 	label: qsTr("Equal"); checked: true; info: qsTr("Assume the two groups have equal variances.") }
			RadioButton { value: "unequal"; 	label: qsTr("Unequal"); info: qsTr("Allow the two groups to have different variances. Bayes factors are then approximated by iterative sampling.") }
		}
		IntegerField
		{
			visible: iterationsBf
			name: "iterationsBayesFactor"
			text: qsTr("No. iterations for BF computation")
			defaultValue: iterationsBfDefaultNumber
			min: 2000
			fieldWidth: 60 * preferencesModel.uiScale
			info: qsTr("Number of iterations used when Bayes factors are computed by sampling.")
		}

		IntegerField
		{
			visible: correlation
			name: "iterationsEstimation"
			text: qsTr("No. iterations for MCMC")
			defaultValue: 10000
			min: 2000
			fieldWidth: 60 * preferencesModel.uiScale
			info: qsTr("Number of posterior draws used to estimate the correlation model.")
		}

		RadioButtonGroup
		{
			visible: correlation
			id: correlationSamplingMethod
			title: qsTr("Sampling Method")
			name: "correlationSamplingMethod"
			info: qsTr("Sampling algorithm used for posterior estimation of the correlation matrix.")
			radioButtonsOnSameRow: true
			RadioButton { value: "LKJ"; 	label: qsTr("LKJ"); checked: true; info: qsTr("Use the LKJ method to sample correlation matrices.") }
			RadioButton { value: "LD"; 	label: qsTr("LD"); info: qsTr("Use the Liu-Daniels method to sample correlation matrices.") }
		}

		DoubleField
		{
			visible: correlation && correlationSamplingMethod.value == "LD"
			name: "nugget"
			text: qsTr("Nugget")
			defaultValue: 0.999
			min: 0
			max: 1
			fieldWidth: 60 * preferencesModel.uiScale
			info: qsTr("Scaling factor used with the LD sampler to keep sampled correlations away from plus or minus one when needed.")
		}
		CheckBox
		{
			visible: intercept
			name: "excludeIntercept"
			text: qsTr("Exclude intercept from the model")
			checked: anova
			info: qsTr("Remove the intercept from the fitted model. This is often appropriate for ANOVA-style parameterizations.")
		}
		SetSeed{ info: qsTr("Fix the random number seed to make the analysis exactly reproducible.") }
	}


// there is still the issue how to get the covariates names and other variables names into this qml element
	function combinePairs(values) {
			var pairs = [];
			for (var i = 0; i < values.length; i++) {
					for (var j = i + 1; j < values.length; j++) {
							pairs.push(values[i] + ":" + values[j]);
					}
			}
			return pairs;
	}

	// Example usage
	property var interactionPairs: combinePairs(interactionValues);

	Group 
	{
		Layout.columnSpan: 2
		title: qsTr("Interaction terms")
		preferredWidth: 400 * jaspTheme.uiScale
		visible: interactions
		info: qsTr("Select which candidate two-way interaction terms should be added to the model.")
		ComponentsList 
		{
			height: 120 * preferencesModel.uiScale
			headerLabels: [qsTr("Include")]
			name: "interactionTerms"
			values: interactionPairs
			addItemManually: false
			info: qsTr("Available two-way interaction terms generated from the selected predictors, factors, or covariates.")
			rowComponent: RowLayout { 
				Text { Layout.preferredWidth: 210*jaspTheme.uiScale; text: rowValue }
				CheckBox { Layout.preferredWidth: 50*jaspTheme.uiScale; name: "includeInteractionEffect"; checked: false; info: qsTr("Include this interaction term in the model.") }
			}
		}
	}

	Group 
	{
		title: qsTr("Effects")
		visible: anova
		columns: 2
		info: qsTr("Prior weights for the omnibus hypotheses that main and interaction effects are absent or present in the ANOVA model.")
		Text { text: "" }
		Text { text: qsTr("Prior weight") }
		Text { text: qsTr("Main zero effect") }
		FormulaField { name: "priorProbMainZero"; defaultValue: "1"; fieldWidth: 50; info: qsTr("Prior weight for the hypothesis that a main effect is absent.") }
		Text { text: qsTr("Main non-zero effect") }
		FormulaField { name: "priorProbMainNonZero"; defaultValue: "1"; fieldWidth: 50; info: qsTr("Prior weight for the hypothesis that a main effect is present.") }
		Text { text: qsTr("Interaction zero effect") }
		FormulaField { name: "priorProbInteractionZero"; defaultValue: "1"; fieldWidth: 50; info: qsTr("Prior weight for the hypothesis that an interaction effect is absent.") }
		Text { text: qsTr("Interaction non-zero effect") }
		FormulaField { name: "priorProbInteractionNonZero"; defaultValue: "1"; fieldWidth: 50; info: qsTr("Prior weight for the hypothesis that an interaction effect is present.") }
	}

}
