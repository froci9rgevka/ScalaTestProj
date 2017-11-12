package gui

import java.awt.Color

import data.Rectangle

import scala.swing.{Button, Color, GridPanel, Label}

class RectanglesGrid(rectangles: List[Rectangle]) extends GridPanel(4, 4){



  contents += new Label(" ");
  contents += new RectangleOwnGrid(rectangles(0));
  contents += new RectangleOwnGrid(rectangles(1));
  contents += new Label(" ");

  for (i <- 2 to 9) {
    contents += new RectangleOwnGrid(rectangles(i));
  }

  contents += new Label(" ");
  contents += new RectangleOwnGrid(rectangles(10));
  contents += new RectangleOwnGrid(rectangles(11));
  contents += new Label(" ");


}
