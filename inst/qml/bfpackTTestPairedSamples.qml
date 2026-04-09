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
		info: qsTr("Assign the two continuous variables that form the paired comparison.")
		
		AvailableVariablesList
		{
			name: 						"variablesList"
			info: qsTr("Available variables from the data set that can be assigned to the analysis.")
		}
		AssignedPairsVariablesList
		{
			name: 						"pair"
			title: 						qsTr("Pair")
			allowedColumns: 			["scale"]
			singleVariable: true
			info: qsTr("Pair the two continuous variables whose mean difference will be tested.")
		}
	}

	Common.HypothesesWindowStandard{
		parName: qsTr("difference")
		specificMu: true
		parameterDescription: qsTr("the mean difference between the paired variables")
	}
	Common.ParametersWindow{}

	Common.HypothesesWindowManual{}

	Common.Options{
		bfTy: true
	}
}
