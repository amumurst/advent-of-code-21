trait Day4Common extends App:
  val (input, boards) = io.Source.fromResource("Day4.txt").getLines().splitAt(1)

  case class Board(rows: List[List[Int]], cols: List[List[Int]]):
    def mark(i: Int): Board =
      Board(cols.map(_.filterNot(_ == i)), rows.map(_.filterNot(_ == i)))
    def hasWon: Boolean = cols.exists(_.isEmpty) || rows.exists(_.isEmpty)
    def sumUnmarked: Int = cols.map(_.sum).sum

  def splitToInt(s: String, c: Char) = s.split(c).flatMap(_.toIntOption).toList

  val players: List[Board] =
    boards
      .filter(_.nonEmpty)
      .grouped(5)
      .map { seq =>
        val arr = seq.map(splitToInt(_, ' ')).toList
        Board(arr, arr.transpose)
      }
      .toList

  def iterFunc(boards: List[Board], number: Int): Either[List[Board], Board]
  def iterate(boards: List[Board], inputs: List[Int]): Int =
    inputs match
      case Nil => throw Exception("Out of inputs")
      case number :: rest =>
        iterFunc(boards.map(_.mark(number)), number) match
          case Right(i)     => i.sumUnmarked * number
          case Left(boards) => iterate(boards, rest)

  println(iterate(players, splitToInt(input.mkString, ',')))

object Day4 extends Day4Common:
  override def iterFunc(boards: List[Board], number: Int) =
    boards.find(_.hasWon).toRight(boards)

object Day4B extends Day4Common:
  override def iterFunc(boards: List[Board], number: Int) =
    boards.filterNot(_.hasWon) match
      case Nil  => Right(boards.head)
      case more => Left(more)
