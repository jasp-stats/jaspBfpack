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
import QtQuick.Controls as QTCONTROLS

Group
{
	info: qsTr("Specify confirmatory hypotheses with equality (=) and order (<, >) constraints on the available parameters. Separate multiple hypotheses into rows and optionally include a complement hypothesis.")

	Layout.columnSpan: 2

	Layout.fillWidth: true

	preferredWidth: 590 * jaspTheme.uiScale 

	columns: 1
	RowLayout {
		Label { text: qsTr("<b>Manual Hypothesis Test</b>") }
		HelpButton
		{
			toolTip: 					qsTr("Click for more information")
			helpPage:					"forQml/tooltip"
		}
	}
	ComponentsList
	{
		info: qsTr("List of manually specified hypotheses. Use parameter names from the Parameters box, assign prior weights, and tick Include for the hypotheses that should be tested.")
		name: "manualHypotheses"
		title: ""
		minimumItems: 1
		headerLabels: [qsTr("Hypotheses"), qsTr("Prior weight"), qsTr("Include")]
		rowComponent: 
			RowLayout {
				TextField
				{ 
					id: hypothesisTextField
					name: "hypothesisText"
					placeholderText: "..."
					fieldWidth: 400 * jaspTheme.uiScale
					info: qsTr("Enter a constrained hypothesis using the shown parameter names, for example a = 0, a > 0, or a > b > 0.")
				}
				FormulaField
				{
					fieldWidth: 60
					name: "priorProbManual"
					defaultValue: "1"
					min: 0
					info: qsTr("Prior weight for this manual hypothesis.")
				}
				Item {}
				CheckBox
				{
					name: "includeHypothesis"
					info: qsTr("Include this manual hypothesis in the analysis.")
				}
			}
		addBorder: false
	}

	Item
	{
		width: 536 * jaspTheme.uiScale
		height: complement.height
		Rectangle {
			x: 1.4 * jaspTheme.labelSpacing
			y: -10
			color: jaspTheme.white
			radius: jaspTheme.borderRadius
			border.width: 1
			border.color: jaspTheme.borderColor
			width: 397 * jaspTheme.uiScale
			height: complementLabel.implicitHeight + 4
			Label { id: complementLabel; anchors.verticalCenter: parent.verticalCenter; anchors.left: parent.left; anchors.leftMargin: 4; text: qsTr("Complement hypothesis:") }
		}
		FormulaField {
			anchors.right: spacer.left
			y: -10
			fieldWidth: 60
			name: "priorProbComplement"
			defaultValue: "1"
			min: 0
			info: qsTr("Prior weight assigned to the complement hypothesis.")
		}
		Item {
			id: spacer
			width: 31 * jaspTheme.uiScale // Adjust the width as needed for the desired space
			anchors.right: complement.left
		}
		CheckBox {
			anchors.right: parent.right
			y: -10
			id: complement
			name: "complement"
			checked: true
			info: qsTr("Include the complement hypothesis in the hypothesis set.")
		}
	}
}
