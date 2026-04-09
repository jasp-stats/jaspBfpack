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
		implicitHeight: 200 * preferencesModel.uiScale
		info: qsTr("Assign one outcome variable and one grouping variable for the variance comparison.")

		AvailableVariablesList
		{
			name: 						"variablesList"
			info: qsTr("Available variables from the data set that can be assigned to the analysis.")
		}
		
		AssignedVariablesList
		{	
			title: 						qsTr("Variable")
			name: 								"variables"
			singleVariable: 			true
			allowedColumns: 			["scale", "ordinal"]
			allowTypeChange: true
			info: qsTr("Outcome variable whose group variances will be compared.")
		}

		AssignedVariablesList
		{
			name: 						"groupingVariable"
			title: 						qsTr("Grouping Variable")
			singleVariable: 			true
			allowedColumns: 			["nominal"]
			info: qsTr("Nominal variable defining the groups whose variances are compared.")
		}
	}

	Common.HypothesesWindowStandard{
		onlyUnequal: true
		parameterDescription: qsTr("the group variances")
	}
	Common.ParametersWindow{}

	Common.HypothesesWindowManual{}

	Common.Options{
		bfTy: false
	}
}
