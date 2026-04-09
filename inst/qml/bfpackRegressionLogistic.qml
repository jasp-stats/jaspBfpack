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
		info: qsTr("Assign an ordinal or nominal dependent variable and predictor variables for the logistic regression model.")
		AvailableVariablesList
		{
			name: 						"variablesList"
			info: qsTr("Available variables from the data set that can be assigned to the analysis.")
		}
		
		AssignedVariablesList
		{
			name: 						"dependent"
			title: 						qsTr("Dependent Variable")
			singleVariable: 			true
			allowedColumns: 			["ordinal", "nominal"]
			allowTypeChange: true
			info: qsTr("Outcome variable for the logistic model. Ordinal outcomes fit an ordered logistic regression; binary nominal outcomes fit a binary logistic regression.")
		}

		AssignedVariablesList
		{
			name: 						"predictors"
			title: 						qsTr("Predictor Variables")
			id: 							predictors
			allowTypeChange: 	true
			info: qsTr("Predictor variables for the logistic regression model.")
		}
	}


	Common.HypothesesWindowStandard{
		parName: qsTr("β")
		parameterDescription: qsTr("each logistic regression coefficient")
	}
	Common.ParametersWindow{}

	Common.HypothesesWindowManual{}

	Common.Options{
		bfTy: false
		interactions: true
		interactionValues: predictors.columnsNames
		intercept: true
	}
}
