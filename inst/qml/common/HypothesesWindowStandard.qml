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


Group
{
	property string parName: qsTr("mu")
	property bool onlyUnequal: false
	property bool specificMu: false
	property bool multiTest: false
	property string parameterDescription: onlyUnequal ?
		(multiTest ?
			qsTr("the means of the selected variables relative to their test values") :
			qsTr("the group variances")) :
		(parName === qsTr("ρ") ?
			qsTr("the correlation coefficients") :
			qsTr("the parameter %1").arg(parName))
	
    // Layout.columnSpan: 2
	id: standGroup
	title: qsTr("<b>Standard Hypothesis Test</b>")
	info: onlyUnequal ?
		(multiTest ?
			qsTr("Tests whether each selected mean equals its test value or differs from it.") :
			qsTr("Tests whether the group variances are equal or unequal.")) :
		(specificMu ?
			qsTr("Tests whether %1 is equal to, smaller than, or larger than the specified test value.").arg(parameterDescription) :
			qsTr("Tests whether %1 is equal to, smaller than, or larger than zero.").arg(parameterDescription))
	columns: specificMu ? 3 : 2
	// implicitHeight: 100 * preferencesModel.uiScale
	// implicitWidth: 250 * preferencesModel.uiScale

	/* so we check if we need three rows or two rows, using the onlyUnequal property which denotes the two row case
		if that is the case, we check if we are doing a multivariate test, in which case we use the palceholder "test value" which can be
		specified in another element for each variable separately. If we do not have the multivariate test, we are actually doing the bartlett 
		test for variances in which case we have only 0 as the test value, and we need only two columns
		If we have more than only unequal so three hypothesis we distinugish two cases: 1) a case where the test value can be specified (specificMu)
		and the case where it cannot
	*/

	property var hypoString: onlyUnequal ? 
	(multiTest ? 
			[qsTr("H0: ") + parName + " = test value", qsTr("H±: ") + parName + " ≠ test value"] : 
			// variances test
			[qsTr("H0: Equal variances"), qsTr("H±: Unequal variances")]) : 
	specificMu ? 
			[qsTr("H0: ") + parName + " = ", qsTr("H-: ") + parName + " < ", qsTr("H+: ") + parName + " > "] : 
			[qsTr("H0: ") + parName + " = 0 ", qsTr("H-: ") + parName + " < 0 ", qsTr("H+: ") + parName + " > 0 "];

	Text { text: qsTr("Hypotheses") }
	Text { text: qsTr("Test value") ; visible: specificMu}
	Text { text: qsTr("Prior weights") }

	Text { text: standGroup.hypoString[0] }
	DoubleField
	{
			name: "muValue"
			fieldWidth: 50
			defaultValue: 0
			visible: specificMu
			id: muValue
			negativeValues: true
			info: qsTr("Reference value used in the standard hypothesis test for %1.").arg(parameterDescription)
	}

	FormulaField {
			fieldWidth: 50
			name: "priorProbStandard"
			defaultValue: onlyUnequal ? "1" : "2"
			min: 0
			info: qsTr("Prior weight for the first standard hypothesis. Larger values assign more prior probability to that hypothesis.")
	}

	Text { text: standGroup.hypoString[1] }
	Text { text: " " + parseFloat(muValue.value); visible: specificMu}

	FormulaField {
			fieldWidth: 50
			name: "priorProbStandard2"
			defaultValue: onlyUnequal ? "1" : "1"
			min: 0
			info: qsTr("Prior weight for the second standard hypothesis.")
	}

	Text { text: standGroup.hypoString[2] ; visible: !onlyUnequal}
	Text { text: " " + parseFloat(muValue.value); visible: !onlyUnequal & specificMu}

	FormulaField {
			fieldWidth: 50
			name: "priorProbStandard3"
			defaultValue: "1"
			min: 0
			visible: !onlyUnequal
			info: qsTr("Prior weight for the third standard hypothesis.")
	}
}


