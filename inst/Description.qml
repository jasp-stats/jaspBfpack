import QtQuick
import JASP.Module

Description
{
	title			: qsTr("BFpack")
	description		: qsTr("A module for computing Bayes factors for equality, inequality, and order constrained hypotheses.")
	icon			: "bain-module"
	requiresData		: true
	hasWrappers		: false
	preloadData: 	true

	GroupTitle
	{
		title: 		"t-Tests"
		icon: 		"analysis-bain-ttest.svg"
	}
	Analysis
	{
		menu:			"Independent Samples t-Test"
		title:		"BFpack Independent Samples t-Test"
		func:			"bfpackTTestIndependentSamples"
	}
	Analysis
	{
		menu: 		"Paired Samples t-Test"
		title:		"BFpack Paired Samples t-Test"
		func:			"bfpackTTestPairedSamples"
	}
	Analysis
	{
		menu:  		"One Sample t-Test"
		title:		"BFpack One Sample t-Test"
		func:			"bfpackTTestOneSample"
	}
	Analysis
	{
		menu:  		"Multivariate t-Test"
		title:		"BFpack Multivariate t-Test"
		func:			"bfpackTTestMultiSample"
	}

	GroupTitle
	{
		title: 		"ANOVA"
		icon: 		"analysis-bain-anova.svg"
	}
	Analysis
	{
		menu:   	"(M)AN(C)OVA"
		title:		"BFpack (M)AN(C)OVA"
		func:			"bfpackAnova"
	}

	GroupTitle
	{
		title: 		"Regression"
		icon: 		"analysis-bain-regression.svg"
	}
	Analysis
	{
		menu:   	"Univariate/Multivariate Linear Regression"
		title:		"BFpack Univariate/Multivariate Linear Regression"
		func:			"bfpackRegressionLinear"
	}
	Analysis
	{
		menu:   	"(Ordered) Logistic Regression"
		title:		"BFpack (Ordered) Logistic Regression"
		func:			"bfpackRegressionLogistic"
	}

	Analysis
	{
		menu: 		"Correlation"
		title:		"BFpack Correlation"
		func:			"bfpackCorrelation"
	}

	GroupTitle
	{
		title: 		"Variances"
		icon: 		"variances.svg"
	}
	Analysis
	{
		menu: 		"Variances"
		title:		"BFpack Variances"
		func:			"bfpackVariances"
	}
}
