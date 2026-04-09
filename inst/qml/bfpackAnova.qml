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
		info: qsTr("Assign dependent variables, fixed factors, and optional covariates for the (M)AN(C)OVA model.")
		AvailableVariablesList
		{
			name: 						"variablesList"
			info: qsTr("Available variables from the data set that can be assigned to the analysis.")
		}

		AssignedVariablesList
		{
			name: 						"dependent"
			title: 						qsTr("Dependent Variables")
			allowedColumns: 			["scale"]
			id: dependent
			info: qsTr("One or more continuous outcome variables. One dependent variable yields an AN(C)OVA; multiple dependent variables yield a MAN(C)OVA.")
		}

		AssignedVariablesList
		{
			name: 						"fixedFactors"
			title: 						qsTr("Fixed Factors")
			allowedColumns: 			["ordinal", "nominal"]
			id: fixedFactors
			allowTypeChange: true
			info: qsTr("Categorical grouping variables whose mean differences and interactions are tested in the model.")
		} 

		AssignedVariablesList
		{
			name: 						"covariates"
			title: 						qsTr("Covariates")
			allowedColumns: 			["scale"]
			id: covariates
			info: qsTr("Continuous predictors to adjust for in the analysis. Adding covariates turns the model into an ANCOVA or MANCOVA.")
 		}
	}

	Common.HypothesesWindowStandard{
		parName: qsTr("μ")
		parameterDescription: qsTr("each adjusted mean or regression effect in the AN(C)OVA model")
	}
	
	Common.ParametersWindow{}


	Common.HypothesesWindowManual{}

	Common.Options{
		bfTy: true
		interactions: true
		anova: true
		interactionValues: fixedFactors.columnsNames.concat(covariates.columnsNames)
		iterationsBf: true
		iterationsBfDefaultNumber: 10000
		intercept: true
	}
}
