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
import "./common" as Common

Form
{


	VariablesForm
	{
		preferredHeight: 250 * preferencesModel.uiScale
		info: qsTr("Assign the variables whose correlations will be tested, plus optional grouping and covariate variables.")

		AvailableVariablesList
		{
			name: 						"variablesList"
			info: qsTr("Available variables from the data set that can be assigned to the analysis.")
		}
		
		AssignedVariablesList
		{
			title: 						qsTr("Variables")
			name: 								"variables"
			singleVariable: 			false
			allowTypeChange: true
			info: qsTr("Select at least two variables whose correlations will be estimated and tested. Supported associations include product-moment, polychoric, tetrachoric, polyserial, and biserial correlations.")
		}

		AssignedVariablesList
		{
			name: 						"groupingVariable"
			title: 						qsTr("Grouping Variable")
			singleVariable: 	true
			allowedColumns: 	["nominal"]
			info: qsTr("Optional nominal grouping variable for testing independent correlations across groups.")
		}

		AssignedVariablesList
		{
			name: 								"covariates"
			title: 								qsTr("Covariates")
			singleVariable: 			false
			allowedColumns: 			["scale", "ordinal"]
			allowTypeChange: true
			info: qsTr("Optional covariates to control for when estimating the correlations.")
		}

	}

	Common.HypothesesWindowStandard{
		parName: qsTr("ρ")
		parameterDescription: qsTr("each correlation coefficient")
	}

	Common.ParametersWindow{}
	
	Common.HypothesesWindowManual{}

	Common.Options{
		bfTy: false
		correlation: true
	}

	// Section
	// {
	// 	title: qsTr("Covariates")
	// 	VariablesForm
	// 	{
	// 		preferredHeight: 150 * preferencesModel.uiScale

	// 		AvailableVariablesList
	// 		{
	// 			name: 						"covariatesList"
	// 			source: 					"variablesList"
	// 		}
			
	// 		AssignedVariablesList
	// 		{
	// 			name: 								"covariates"
	// 			singleVariable: 			false
	// 			allowedColumns: 			["scale", "ordinal"]
	// 		}
	// 	}
	
	// }
}
