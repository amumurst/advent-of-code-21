trait Day1Base extends App with scala.util.ChainingSyntax:
  type Data = Iterator[Int]
  def readInput: Data =
    io.Source.fromResource("Day1.txt").getLines().flatMap(_.toIntOption)
  def readTest: Data =
    Iterator(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

object Day1 extends Day1Base:
  readInput
    .sliding(2)
    .count(ls => ls.tail.head > ls.head)
    .pipe(println)

object Day1B extends Day1Base:
  readInput
    .sliding(3)
    .map(_.sum)
    .sliding(2)
    .count(ls => ls.tail.head > ls.head)
    .pipe(println)
