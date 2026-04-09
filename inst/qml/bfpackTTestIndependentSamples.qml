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
		preferredHeight: 				jaspTheme.smallDefaultVariablesFormHeight
		info: qsTr("Assign one continuous outcome variable and one nominal grouping variable for the independent samples t-test.")
		
		AvailableVariablesList
		{
			name: 						"variablesList"
			info: qsTr("Available variables from the data set that can be assigned to the analysis.")
		}

		AssignedVariablesList
		{
			name: 						"variables"
			title: 						qsTr("Variables")
			singleVariable: 			true
			allowedColumns: 			["scale"]
			info: qsTr("Continuous outcome variable to compare between the two groups.")
		}

		AssignedVariablesList
		{
			name: 						"groupingVariable"
			title: 						qsTr("Grouping Variable")
			singleVariable: 			true
			allowedColumns: 			["nominal"]
			info: qsTr("Nominal variable defining the two independent groups.")
		}
	}


	Common.HypothesesWindowStandard{
		parName: qsTr("difference")
		specificMu: true
		parameterDescription: qsTr("the group mean difference")
	}

	Common.ParametersWindow{}

	Common.HypothesesWindowManual{}
	

	Common.Options{
		bfTy: true
		variances: true
		iterationsBf: true
		iterationsBfDefaultNumber: 1000000
	}
	
}
