object Day2 extends App with util.ChainingSyntax:
  io.Source
    .fromResource("Day2.txt")
    .getLines()
    .foldLeft((0, 0)) {
      case ((x, y), s"down $i")    => (x, y + i.toInt)
      case ((x, y), s"up $i")      => (x, y - i.toInt)
      case ((x, y), s"forward $i") => (x + i.toInt, y)
    }
    .pipe((x, y) => x * y)
    .pipe(println)

object Day2B extends App with util.ChainingSyntax:
  io.Source
    .fromResource("Day2.txt")
    .getLines()
    .foldLeft((0, 0, 0)) {
      case ((x, y, a), s"down $i")    => (x, y, a + i.toInt)
      case ((x, y, a), s"up $i")      => (x, y, a - i.toInt)
      case ((x, y, a), s"forward $i") => (x + i.toInt, y + (a * i.toInt), a)
    }
    .pipe((x, y, _) => x * y)
    .pipe(println)
