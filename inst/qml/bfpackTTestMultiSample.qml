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
		info: qsTr("Assign the continuous variables whose means will be tested jointly in the multivariate t-test.")
		
		AvailableVariablesList
		{
			name: 						"variablesList"
			info: qsTr("Available variables from the data set that can be assigned to the analysis.")
		}

		AssignedVariablesList
		{
			name: 						"variables"
			title: 						qsTr("Variables")
			singleVariable: 			false
			allowedColumns: 			["scale"]
			info: qsTr("Continuous variables whose means are tested simultaneously.")
		}
	}


	ColumnLayout 
	{
		id: standardHypothesesSection
		Common.HypothesesWindowStandard{
			id: standardHypotheses
			parName: qsTr("μ")
			multiTest: true
			onlyUnequal: true
			parameterDescription: qsTr("the means of the selected variables")
		}
		ComponentsList 
		{
			title: qsTr("Specify test value")
			id: testValues
			visible: standardHypotheses.multiTest
			implicitHeight: 90 * preferencesModel.uiScale
			// implicitWidth: 200 * preferencesModel.uiScale
			source:  "variables"
			name: "testValues"
			info: qsTr("Set the reference mean for each selected variable in the standard hypothesis test.")
			rowComponent: RowLayout {
				Text { text: rowValue }
				DoubleField {
					// implicitWidth: 100 * preferencesModel.uiScale
					name: "testValue"
					fieldWidth: 50
					defaultValue: 0
					negativeValues: true
					info: qsTr("Test value for the mean of %1.").arg(rowValue)
				}
			}
		}
	}
	
	Common.ParametersWindow{}

	Common.HypothesesWindowManual{}

	Common.Options{
		bfTy: true
		iterationsBf: true
		iterationsBfDefaultNumber: 10000
	}
}
