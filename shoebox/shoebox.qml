import QtQuick 2.3
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
 

ApplicationWindow {
    visible: true
    width: 800
    height: 600

    title: qsTr("shoebox")

    menuBar: MenuBar {
        Menu {
            title: qsTr("File")
            MenuItem {
                text: qsTr("&Open")
                onTriggered: console.log("Open action triggered");
            }
            MenuItem {
                text: qsTr("Exit")
                onTriggered: Qt.quit();
            }
        }
    }
 
    TabView {
        Keys.onReleased: onKey("")

        id: sourceTab
        anchors.centerIn: parent
        anchors.fill: parent
        Tab {
            title: "Text"
            ColumnLayout {

                Rectangle {
                    color: white
                    width: 400
                    height: 200
     
                    TextEdit {
                        id: sourceText
                        font.family: "Arial"
                        font.pointSize: 10
                        textFormat: TextEdit.PlainText
                        anchors.fill: parent
                    }
                }

                Button {
                        text: "Compute ..."
                        onClicked: displayText(sourceText.text)
                }
            }
        }
        Tab {
            title: "Dictionary"
            ColumnLayout {
                TextEdit {
                    width: 400
                    height: 200
                    id: dictionaryText
                    font.family: "Arial"
                    font.pointSize: 10
                    textFormat: TextEdit.PlainText
                }
                Button {
                        text: "Load ..."
                        onClicked: (function () { dictionaryText.text = getDictionaryText(); })()
                }
            }
        }
        Tab {
            title: "Affixes"
            TextEdit {
                id: affixesText
                font.family: "Arial"
                font.pointSize: 10
                textFormat: TextEdit.PlainText
                anchors.fill: parent
            }
        }
        Tab {
            title: "Segmentation"
            TextEdit {
                id: segmentationText
                font.family: "Arial"
                font.pointSize: 10
                textFormat: TextEdit.PlainText
                anchors.fill: parent
            }
        }
    }
}