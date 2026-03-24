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
	id: manualGroup

	property real hypothesisColumnWidth: 420 * jaspTheme.uiScale
	property real complementColumnWidth: hypothesisColumnWidth + 4 * jaspTheme.uiScale
	property real includeColumnSpacing: 10 * jaspTheme.uiScale

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
				TextArea
				{
					id: hypothesisTextField
					name: "hypothesisText"
					placeholderText: "..."
					width: manualGroup.hypothesisColumnWidth
					height: 60 * jaspTheme.uiScale
					showLineNumber: false
					info: qsTr("Enter a constrained hypothesis using the shown parameter names, for example a = 0, a > 0, or a > b > 0.")
				}
				FormulaField
				{
					fieldWidth: 40
					name: "priorProbManual"
					defaultValue: "1"
					min: 0
					info: qsTr("Prior weight for this manual hypothesis.")
				}
				Item { width: manualGroup.includeColumnSpacing } // Spacer between prior weight and checkbox
				CheckBox
				{
					name: "includeHypothesis"
					info: qsTr("Include this manual hypothesis in the analysis.")
				}
			}
		addBorder: false
	}

		RowLayout
		{
			Item
			{
				Layout.preferredWidth: manualGroup.complementColumnWidth
				Layout.preferredHeight: complementBox.height

				Rectangle
				{
					id: complementBox
					color: jaspTheme.white
					radius: jaspTheme.borderRadius
					border.width: 1
					border.color: jaspTheme.borderColor
					width: parent.width
					height: complementLabel.implicitHeight + 4

					Label
					{
						id: complementLabel
						anchors.verticalCenter: parent.verticalCenter
						anchors.left: parent.left
						anchors.leftMargin: 4
						text: qsTr("Complement hypothesis")
					}
				}
			}
			FormulaField
			{
				fieldWidth: 40
				name: "priorProbComplement"
				defaultValue: "1"
				min: 0
				info: qsTr("Prior weight assigned to the complement hypothesis.")
			}
			Item { width: manualGroup.includeColumnSpacing }
			CheckBox
			{
				id: complement
				name: "complement"
				checked: true
				info: qsTr("Include the complement hypothesis in the hypothesis set.")
			}
		}
	}
