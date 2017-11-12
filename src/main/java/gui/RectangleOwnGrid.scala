package gui

import java.awt.Color

import data.Rectangle

import scala.swing.{Button, GridPanel, Label}

class RectangleOwnGrid (rectangle: Rectangle) extends GridPanel(3, 3) {

  background = Color.gray

  contents += new Button(rectangle.angle1.toString);
  contents += new Label(" ");
  contents += new Button(rectangle.angle2.toString);
  contents += new Label(" ");
  contents += new Label((rectangle.number+1).toString);
  contents += new Label(" ");
  contents += new Button(rectangle.angle3.toString);
  contents += new Label(" ");
  contents += new Button(rectangle.angle4.toString);


}
