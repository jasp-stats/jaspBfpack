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
		
		AvailableVariablesList
		{
			name: 						"variablesList"
		}

		AssignedVariablesList
		{
			name: 						"variables"
			title: 						qsTr("Variables")
			singleVariable: 			false
			allowedColumns: 			["scale"]
		}
	}


	ColumnLayout 
	{
		Common.HypothesesWindowStandard{
			parName: qsTr("μ")
			multiTest: true
			onlyUnequal: true
		}
		ComponentsList 
		{
			title: qsTr("Specify test value")
			id: testValues
			visible: multiTest
			implicitHeight: 90 * preferencesModel.uiScale
			// implicitWidth: 200 * preferencesModel.uiScale
			source:  "variables"
			name: "testValues"
			rowComponent: RowLayout {
				Text { text: rowValue }
				DoubleField {
					// implicitWidth: 100 * preferencesModel.uiScale
					name: "testValue"
					fieldWidth: 50
					defaultValue: 0
					negativeValues: true
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
