trait Day5Common extends App with util.ChainingSyntax:
  def makeRange(a: Int, b: Int) =
    Range.inclusive(a, b, if (a < b) 1 else -1).toList
  def points(xs: List[Int], ys: List[Int]): List[(Int, Int)]

  io.Source
    .fromResource("Day5.txt")
    .getLines()
    .toList
    .flatMap { case s"$x0,$y0 -> $x1,$y1" =>
      points(makeRange(x0.toInt, x1.toInt), makeRange(y0.toInt, y1.toInt))
    }
    .groupBy(identity)
    .count(_._2.size > 1)
    .pipe(println)

object Day5 extends Day5Common:
  override def points(xs: List[Int], ys: List[Int]) =
    if (xs.size == 1) ys.map(xs.head -> _)
    else if (ys.size == 1) xs.map(_ -> ys.head)
    else List.empty

object Day5B extends Day5Common:
  override def points(xs: List[Int], ys: List[Int]) =
    if (xs.size == 1) ys.map(xs.head -> _)
    else if (ys.size == 1) xs.map(_ -> ys.head)
    else xs.zip(ys)
