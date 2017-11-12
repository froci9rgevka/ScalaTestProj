package data

import scala.collection.mutable.ListBuffer

class Squad (rectangles: List[Rectangle], sides: List[Int]){
  var rectangle1 = rectangles(0);
  var rectangle2 = rectangles(1);
  var rectangle3 = rectangles(2);
  var rectangle4 = rectangles(3);

  var side1 = sides(0);
  var side2 = sides(1);
  var side3 = sides(2);
  var side4 = sides(3);

  def this (rectangles: List[Rectangle]) ={
    this(rectangles, List(
      rectangles(0).angle2 + rectangles(1).angle1,
      rectangles(1).angle4 + rectangles(3).angle2,
      rectangles(0).angle3 + rectangles(2).angle1,
      rectangles(2).angle4 + rectangles(3).angle3
    ))
  }

  def centerValid() : Boolean = {
    if (rectangle1.angle4 + rectangle2.angle3 + rectangle3.angle2 + rectangle4.angle1 == 10)
      return true
    else
      return false
  }

  def sidesValid() : Boolean = {
    if (side1 < 10
      & side2 < 10
      & side3 < 10
      & side4 < 10)
      return true
    else
      return false
  }

  def isValid() : Boolean = {
    if (centerValid() & sidesValid())
      return true
    else
      return false
  }

  def isContainRect(rectangle: Rectangle) : Boolean = {
    if (rectangle != rectangle1
      & rectangle != rectangle2
      & rectangle != rectangle3
      & rectangle != rectangle4)
      return false
    else
      return true
  }

  def isContainSquadRect(squad: Squad) : Boolean = {
    if (!isContainRect(squad.rectangle1)
      & !isContainRect(squad.rectangle2)
      & !isContainRect(squad.rectangle3)
      & !isContainRect(squad.rectangle4))
      return false
    else
      return true
  }

  // Check is Squads rectangles contained in current composition
  def checkFlags(flagsUsedRectangles: Array[Int]) : Boolean = {
    if (flagsUsedRectangles(rectangle1.number) != 1
      & flagsUsedRectangles(rectangle2.number) != 1
      & flagsUsedRectangles(rectangle3.number) != 1
      & flagsUsedRectangles(rectangle4.number) != 1)
      return true
    else
      return false
  }

  // Fill numbers of Squads rectangles that are contained in current composition
  def fillFlags(flagsUsedRectangles: Array[Int]) : Unit = {
    flagsUsedRectangles(rectangle1.number) = 1;
    flagsUsedRectangles(rectangle2.number) = 1;
    flagsUsedRectangles(rectangle3.number) = 1;
    flagsUsedRectangles(rectangle4.number) = 1;
  }

}
