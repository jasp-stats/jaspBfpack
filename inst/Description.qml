import QtQuick
import JASP.Module

Description
{
	name			: "jaspBfpack"
	title			: qsTr("BFpack (beta)")
	description		: qsTr("A module for computing Bayes factors for equality, inequality, and order constrained hypotheses.")
	icon			: "bain-module"
	requiresData	: true
	version			: "0.95.4"
	author			: "JASP Team"
	maintainer		: "Julius M. Pfadt <julius.pfadt@gmail.com>"
	website			: "https://bfpack.info"
	license			: "GPL (>= 3)"
	hasWrappers		: false
	preloadData: true

	GroupTitle
	{
		title: 		"T-Tests"
		icon: 		"analysis-bain-ttest.svg"
	}
	Analysis
	{
		menu:			"Independent Samples T-Test"
		title:		"BFpack Independent Samples T-Test"
		func:			"bfpackTTestIndependentSamples"
	}
	Analysis
	{
		menu: 		"Paired Samples T-Test"
		title:		"BFpack Paired Samples T-Test"
		func:			"bfpackTTestPairedSamples"
	}
	Analysis
	{
		menu:  		"One Sample T-Test"
		title:		"BFpack One Sample T-Test"
		func:			"bfpackTTestOneSample"
	}
	Analysis
	{
		menu:  		"Multivariate T-Test"
		title:		"BFpack Multivariate T-Test"
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
		menu:   	"Linear (Multivariate) Regression"
		title:		"BFpack Linear (Multivariate) Regression"
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
