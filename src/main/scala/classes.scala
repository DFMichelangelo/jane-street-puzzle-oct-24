enum Mark:
  case A,B,C

class Coordinate(val x: Int,val y: Int):
  override def toString = s"($x,$y)"
class MarkCoordinate(x: Int,y: Int,val mark:Mark, visitedOn:Int= -1) extends WalkingCoordinate(x,y,visitedOn):
  override def toString = s"($x,$y, mark=$mark, visitedOn=$visitedOn)"
  def toPosition: String =getCharForNumber(x+1) + (y+1)
class WalkingCoordinate(x: Int,y: Int,var visitedOn:Int= -1) extends Coordinate(x,y):
  def hasBeenVisited: Boolean = visitedOn>=1

private def getCharForNumber(i: Int) = i match
  case 1 =>"a"
  case 2 =>"b"
  case 3 =>"c"
  case 4 =>"d"
  case 5 =>"e"
  case 6 =>"f"

class Board[T <: Coordinate](coordinates: Seq[T]):
  val minX: Int = coordinates.map(_.x).min
  val maxX: Int = coordinates.map(_.x).max
  val minY: Int = coordinates.map(_.y).min
  val maxY: Int = coordinates.map(_.y).max
  val map: Map[(Int, Int), T] = coordinates.map(coord => (coord.x, coord.y) -> coord).toMap
  def findCoordinate(x:Int,y:Int): Option[T] =map.get((x,y))

class VisitingBoard[T<:WalkingCoordinate](coordinates: Seq[T]) extends Board(coordinates):
  def checkVisitedCoordinate(x:Int,y:Int): Boolean = findCoordinate(x,y).exists(_.hasBeenVisited)
  def setVisited(x:Int,y:Int, visitedOn: Int): Option[Unit] = findCoordinate(x,y).map(_.visitedOn=visitedOn)

class MarkBoard(coordinates: Seq[MarkCoordinate]) extends VisitingBoard(coordinates):
  def getMark(x:Int, y:Int): Option[Mark] = findCoordinate(x,y).map(_.mark)
  def isValidMarkToStepIn(x:Int,y:Int, markToStepIn: Mark): Boolean = findCoordinate(x,y).exists(_.mark==markToStepIn)