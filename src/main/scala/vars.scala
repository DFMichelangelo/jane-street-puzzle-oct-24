import Mark.{A, B, C}

val knightMoves: Seq[Coordinate] = Seq(
  Coordinate(2, 1),
  Coordinate(2, -1),
  Coordinate(-2, 1),
  Coordinate(-2, -1),
  Coordinate(1, 2),
  Coordinate(1, -2),
  Coordinate(-1, 2),
  Coordinate(-1, -2))

val marks: Seq[MarkCoordinate] = Seq(
  MarkCoordinate(0, 5, A), MarkCoordinate(1, 5, B), MarkCoordinate(2, 5, B), MarkCoordinate(3, 5, C), MarkCoordinate(4, 5, C), MarkCoordinate(5, 5, C),
  MarkCoordinate(0, 4, A), MarkCoordinate(1, 4, B), MarkCoordinate(2, 4, B), MarkCoordinate(3, 4, C), MarkCoordinate(4, 4, C), MarkCoordinate(5, 4, C),
  MarkCoordinate(0, 3, A), MarkCoordinate(1, 3, A), MarkCoordinate(2, 3, B), MarkCoordinate(3, 3, B), MarkCoordinate(4, 3, C), MarkCoordinate(5, 3, C),
  MarkCoordinate(0, 2, A), MarkCoordinate(1, 2, A), MarkCoordinate(2, 2, B), MarkCoordinate(3, 2, B), MarkCoordinate(4, 2, C), MarkCoordinate(5, 2, C),
  MarkCoordinate(0, 1, A), MarkCoordinate(1, 1, A), MarkCoordinate(2, 1, A), MarkCoordinate(3, 1, B), MarkCoordinate(4, 1, B), MarkCoordinate(5, 1, C),
  MarkCoordinate(0, 0, A), MarkCoordinate(1, 0, A), MarkCoordinate(2, 0, A), MarkCoordinate(3, 0, B), MarkCoordinate(4, 0, B), MarkCoordinate(5, 0, C)
)

val markBoard = MarkBoard(marks)