package gui


import java.awt.Dimension

import data.Rectangle

import scala.swing.event.{ButtonClicked, SelectionChanged}
import scala.swing.{BorderPanel, Button, ComboBox, Label, MainFrame}

class TaskFrame (inputRectangles: List[Rectangle], resultRectangles: List[List[Rectangle]]) extends MainFrame{
  title = "Задача о перестановках"

  contents = new BorderPanel {
    val bChoose = new Button("Показать решения");
    val cbAnswerNum = new ComboBox(1 to resultRectangles.length);
    var rgCenter = new RectanglesGrid(inputRectangles);

    cbAnswerNum.visible = false;

      add(bChoose, BorderPanel.Position.North)
      add(rgCenter, BorderPanel.Position.Center)
      add(cbAnswerNum, BorderPanel.Position.East)
      add(Button("Close") {
        sys.exit(0)
      }, BorderPanel.Position.South)

    def repaintAll(): Unit = {
      this.layout(rgCenter) = BorderPanel.Position.Center;
      rgCenter.revalidate;
      rgCenter.repaint();
    }


    listenTo(bChoose);
    listenTo(cbAnswerNum.selection);

    reactions += {
      case ButtonClicked(bChoose) =>
        if (cbAnswerNum.visible == false) {
          if (resultRectangles.length > 0) {
            cbAnswerNum.visible = true;
            bChoose.text = "Показать условие";
            val selectedItem = cbAnswerNum.selection.item - 1;
            rgCenter = new RectanglesGrid(resultRectangles(selectedItem));
            repaintAll();
          }
          else {
            bChoose.text = "Решений не нашлось! :(";
            bChoose.enabled = false;
          }
        }
        else {
          cbAnswerNum.visible = false;
          bChoose.text = "Показать решения";
          rgCenter = new RectanglesGrid(inputRectangles);
          repaintAll();
        }
      case SelectionChanged(cbAnswerNum) =>
        val selectedItem = this.cbAnswerNum.selection.item - 1;
        rgCenter = new RectanglesGrid(resultRectangles(selectedItem));
        repaintAll();
    }

    val s = new Dimension(800,700);
    preferredSize = s;
  }


  visible = true;
}
