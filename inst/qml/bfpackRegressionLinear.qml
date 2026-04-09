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


import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls
import "./common" as Common

Form
{

	VariablesForm
	{
		info: qsTr("Assign continuous dependent variables and predictor variables for the linear regression model.")
		AvailableVariablesList
		{
			name: 						"variablesList"
			info: qsTr("Available variables from the data set that can be assigned to the analysis.")
		}
		
		AssignedVariablesList
		{
			name: 						"dependent"
			title: 						qsTr("Dependent Variables")
			singleVariable: 			false
			allowedColumns: 			["scale"]
			height : 100*jaspTheme.uiScale
			info: qsTr("One or more continuous outcome variables. Multiple dependent variables fit a multivariate multiple regression.")
		}

		AssignedVariablesList
		{
			name: 						"predictors"
			title: 						qsTr("Predictor Variables")
			id: 							predictors
			allowTypeChange: true
			info: qsTr("Predictor variables for the regression model. Nominal and ordinal predictors are dummy coded automatically when needed.")
		}
	}

	Common.HypothesesWindowStandard{
		parName: qsTr("β")
		parameterDescription: qsTr("each regression coefficient")
	}
	Common.ParametersWindow{}

	Common.HypothesesWindowManual{}

	Common.Options{
		bfTy: true
		interactions: true
		interactionValues: predictors.columnsNames
		intercept: true
	}
}
