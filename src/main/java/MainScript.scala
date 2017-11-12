import data.{Rectangle, Squad}
import gui.TaskFrame

import scala.collection.mutable.ListBuffer

object HelloWorld extends App {

  //  read input file
  def readFile(fileName: String): List[Rectangle] = {
    val currentDirectory = new java.io.File(".").getCanonicalPath;
    val lines = scala.io.Source.fromFile(currentDirectory + "/" + fileName).getLines.toList;
    //val rectangles = lines.map(line => new Rectangle(line.split(" ").map(_.toInt).toList));
    val rectangles = ListBuffer[Rectangle]();
    for (i <- lines.indices) {rectangles += new Rectangle(lines(i).split(" ").map(_.toInt).toList , i)};
    return rectangles.toList;
  }

  //  print all answers
  def printOutput(output: List[List[Rectangle]]) =
    output.foreach{answer =>
      answer.foreach{rectangle =>
        print(rectangle.toString);
        print("\n")
      }
      print("\n")
    }

  def isSquadGood(squad :  List[Rectangle]): Boolean ={
    return (squad(0).angle4 + squad(1).angle3 + squad(2).angle2 + squad(3).angle1  == 10);
  }

  //  find groups of 4 rectangles that have center = 10 and all sides < 10
  def findSquads(allRectangles: List[Rectangle]) : List[Squad] = {
    var result = ListBuffer[Squad]();

    for(rect1 <- allRectangles) {
      for (rect2 <- allRectangles)
        if (rect1 != rect2) {
          val side1 = rect1.angle2 + rect2.angle1;
          if (side1 < 10) {
            val sum1 = rect1.angle4 + rect2.angle3;
            if (sum1 <= 10)
              for (rect3 <- allRectangles)
                if (rect1 != rect3
                  & rect2 != rect3){
                  val side3 = rect1.angle3 + rect3.angle1;
                  if (side3 < 10) {
                    val sum2 = sum1 + rect3.angle2;
                    if (sum2 <= 10)
                      for (rect4 <- allRectangles)
                        if (rect1 != rect4
                          & rect2 != rect4
                          & rect3 != rect4) {
                          val sum3 = sum2 + rect4.angle1;
                          if (sum3 == 10) {
                            val side2 = rect2.angle4 + rect4.angle2;
                            if (side2 < 10) {
                              val side4 = rect3.angle4 + rect4.angle3;
                              if (side4 < 10){
                                val squad = new Squad(
                                  List[Rectangle](rect1, rect2, rect3, rect4),
                                  List[Int](side1, side2, side3, side4));
                                result += squad
                              }
                            }
                          }
                        }
                  }
              }
          }
        }
    }

    return result.toList;
  }

  //  find result combination of 4 squads
  def findSquadCombinations(squads : List[Squad]) : List[List[Rectangle]] = {
    var result = ListBuffer[List[Rectangle]]();

    for (squadTop <- squads) {
      val flagsUsedRectangles1 = Array.fill(12)(0);
      squadTop.fillFlags(flagsUsedRectangles1);
      for (squadBot <- squads)
        if (squadBot.checkFlags(flagsUsedRectangles1)) {
          if (squadTop.side4 + squadBot.side1 == 10) {
            val flagsUsedRectangles2 = flagsUsedRectangles1.clone();
            squadBot.fillFlags(flagsUsedRectangles2);
            for (squadRight <- squads) {
              if (flagsUsedRectangles2(squadRight.rectangle2.number) != 1
                & flagsUsedRectangles2(squadRight.rectangle4.number) != 1)
                if (squadRight.rectangle1.number == squadTop.rectangle4.number
                  & squadRight.rectangle3.number == squadBot.rectangle2.number)
                  if (squadTop.side2 + squadRight.rectangle2.angle1 < 10
                    & squadBot.side2 + squadRight.rectangle4.angle3 < 10){
                    val flagsUsedRectangles3 = flagsUsedRectangles2.clone();
                    flagsUsedRectangles3(squadRight.rectangle2.number) = 1;
                    flagsUsedRectangles3(squadRight.rectangle4.number) = 1;
                    for (squadLeft <- squads) {
                      if (flagsUsedRectangles3(squadLeft.rectangle1.number) != 1
                        & flagsUsedRectangles3(squadLeft.rectangle3.number) != 1){
                          if (squadLeft.rectangle2.number == squadTop.rectangle3.number
                            & squadLeft.rectangle4.number == squadBot.rectangle1.number)
                            if (squadTop.side3 + squadLeft.rectangle1.angle2 < 10
                              & squadBot.side3 + squadLeft.rectangle3.angle4 < 10)
                              result += List(squadTop.rectangle1, squadTop.rectangle2,
                                squadLeft.rectangle1, squadLeft.rectangle2, squadRight.rectangle1, squadRight.rectangle2,
                                squadLeft.rectangle3, squadLeft.rectangle4, squadRight.rectangle3, squadRight.rectangle4,
                                squadBot.rectangle3, squadBot.rectangle4);
                      }
                    }
              }
            }
          }
        }
      }

    return result.toList;
  }

  //  beauty_way_to
  //  find groups of 4 rectangles that have center = 10 and all sides < 10
  def findSquads_beauty(allRectanglesList: List[Rectangle]) : List[Squad] = {
    val allRectangles = allRectanglesList.to[ListBuffer];
    var result = ListBuffer[Squad]();

    allRectangles.foreach((rect1: Rectangle) =>
      allRectangles.filter(_!=rect1).foreach((rect2: Rectangle) =>
        allRectangles.filter(rect3 => rect3 !=rect1 & rect3 != rect2).foreach((rect3: Rectangle) =>
          allRectangles.filter(rect4 => rect4 !=rect1 & rect4 != rect2 & rect4 != rect3).foreach((rect4: Rectangle) => {
            val squad = new Squad(List(rect1, rect2, rect3, rect4));
            if (squad.isValid()) result += squad
          }))));

    return result.toList;
  }


  //  beauty_way_to
  //  find groups of 4 rectangles that have center = 10 and all sides < 10
  def findSquads_very_beauty(allRectangles: List[Rectangle]) : List[Squad] = {
    var result = ListBuffer[Squad]();

    for(rect1 <- allRectangles; rect2 <- allRectangles; rect3 <- allRectangles; rect4 <- allRectangles) {
      val rectList = List(rect1, rect2, rect3, rect4);
      if (rectList.distinct.length == rectList.length) {
        val squad = new Squad(rectList);
        if (squad.isValid()) result += squad
      }
    }

    return result.toList;
  }

  override def main(args: Array[String]) = {
    //  read input file
    val input = readFile("input/input.txt");


    //  find answers
    val squads = findSquads(input);
    val answers = findSquadCombinations(squads);

    //  print all answers
    printOutput(answers);

    /*

    ////// check speed
    val timeBegin = System.currentTimeMillis();
    for (i <- 0 to 0) {
      val answers = findSquadCombinations(findSquads(input));
    }
    val timeEnd = System.currentTimeMillis();
    println("Elapsed time for search answers: " + (timeEnd - timeBegin) + " ms \n");
    /////

    //  GUI
    val taskFrame = new TaskFrame(input, answers);



    ////////////////////////////////////////////BEAUTY VS SPEED////////////////////////////////////////////
    ///////////// Check values /////////////
    val squads1 = findSquads(input);
    val squads2 = findSquads_beauty(input);
    val squads3 = findSquads_very_beauty(input);
    if (squads1 == squads2 & squads2 == squads3) println("Squads are the same.");

    ////////////// Check times /////////////
    ////// NOT beauty
    val time1Begin = System.currentTimeMillis();
    for (i <- 0 to 100) {
      val squads = findSquads(input);
    }
    val time1End = System.currentTimeMillis();
    println("Elapsed time for findSquads: " + (time1End - time1Begin) + " ms")
    /////

    ////// beauty
    val time2Begin = System.currentTimeMillis();
    for (i <- 0 to 100) {
      val squads = findSquads_beauty(input);
    }
    val time2End = System.currentTimeMillis();
    println("Elapsed time for findSquads_beauty: " + (time2End - time2Begin) + " ms")
    /////

    ////// very beauty
    val time3Begin = System.currentTimeMillis();
    for (i <- 0 to 100) {
      val squads = findSquads_very_beauty(input);
    }
    val time3End = System.currentTimeMillis();
    println("Elapsed time for findSquads_very_beauty: " + (time3End - time3Begin) + " ms")
    /////
    ///////////////////////////////////////////////////////////////////////////////////////////////////////
    */
  }
}
