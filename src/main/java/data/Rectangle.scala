package data

class Rectangle (angles: List[Int], n: Int){
  val number = n;

  val angle1 = angles(0);
  val angle2 = angles(1);
  val angle3 = angles(2);
  val angle4 = angles(3);


  override def toString: String =
    s"$angle1 $angle2 $angle3 $angle4"
}
