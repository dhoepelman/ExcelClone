package scalaExcel.GUI.modelwrapper

import scalafx.beans.property.ObjectProperty

class ObservableSheetCell extends ObjectProperty[SheetCell] {
  onChange({
    (_, oldValue, newValue) => {
      println("Observable changed from " + {
        if (oldValue == null) "null" else oldValue.verboseString
      } + " to " + newValue.verboseString)
    }
  })
}
