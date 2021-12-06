trait Day6Common extends App:
  extension (l: List[Long])
    def update(i: Int)(f: Long => Long) = l.updated(i, f(l(i)))

  val startFish = io.Source
    .fromResource("Day6.txt")
    .mkString
    .split(',')
    .map(_.toInt)
    .foldLeft(List.fill(9)(0L))((acc, v) => acc.update(v)(_ + 1))

  def simulate(days: Int) = Iterator
    .iterate(startFish) { case a :: rest => rest.update(6)(_ + a) :+ a }
    .drop(days)
    .next
    .sum

object Day6 extends Day6Common:
  println(simulate(80))
object Day6B extends Day6Common:
  println(simulate(256))
