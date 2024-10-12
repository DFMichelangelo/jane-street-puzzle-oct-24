
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import Mark.{A, B, C}


class Path(val path: ArrayBuffer[Coordinate]=ArrayBuffer.empty):
  def append(coordinate: Coordinate): path.type = path.append(coordinate)
  def pop(): Coordinate =path.remove(path.size-1)
class MarkPath(val path: ArrayBuffer[Mark])


private def isValidXMove(coordinate: Coordinate, board: MarkBoard): Boolean =
  board.minX <= coordinate.x && coordinate.x <= board.maxX

private def isValidYMove(coordinate: Coordinate, board: MarkBoard): Boolean =
  board.minY <= coordinate.y && coordinate.y <= board.maxY

private def isValidMove(coordinate: Coordinate, board: MarkBoard): Boolean =
  isValidXMove(coordinate, board)
    && isValidYMove(coordinate, board)
    && !board.checkVisitedCoordinate(coordinate.x, coordinate.y)


def knightPaths(
                 knightMoves: Seq[Coordinate],
                 initialCoordinate: Coordinate,
                 board:MarkBoard,
                 moveCount:Int,
                 path:Path,
                 target:Coordinate,
                 allPaths:ArrayBuffer[Path],
                 maxMoves: Int):ArrayBuffer[Path]={
  if (initialCoordinate.x == target.x && initialCoordinate.y== target.y)
    allPaths.addOne(Path(path.path.clone()))
  else if (moveCount <= maxMoves){
    knightMoves
      .map(move=>Coordinate(initialCoordinate.x + move.x, initialCoordinate.y + move.y))
      .filter(isValidMove(_, board))
      .foreach{newCoordinate=>
        board.setVisited(newCoordinate.x,newCoordinate.y,moveCount)
        path.append(newCoordinate)
        knightPaths(knightMoves, newCoordinate, board, moveCount + 1, path, target, allPaths,maxMoves)
        board.setVisited(newCoordinate.x,newCoordinate.y, 0)
        path.pop()
      }
  }
  allPaths
}

class ABCContainer(val a:Int,val b:Int,val c:Int):
  def getValue(mark:Mark): Int = mark match
    case Mark.A => a
    case Mark.B => b
    case Mark.C => c

@main
def main(): Unit = {
  // input
  val maxMoves = 36
  val result = 2024
  val abcRange = Range.inclusive(1,50)

  val foundPaths = knightPaths(
    knightMoves,
    Coordinate(0, 0),
    markBoard,
    2,
    Path(),
    Coordinate(5, 5),
    ArrayBuffer.empty[Path], maxMoves)



  println(foundPaths.size)
//  val abc = for {
//        //allowedPath < MarkPath(ArrayBuffer.newBuilder.addAll(Seq(A,A,A,A,C,C,C)))
//        pathsBottomToTop <-knightPaths(knightMoves,Coordinate(0,0),board,0,Path(ArrayBuffer.empty),Coordinate(5,5),ArrayBuffer.empty,11)
//        pathsTopToBottom <-knightPaths(knightMoves,Coordinate(0,5),board,0,Path(ArrayBuffer.empty),Coordinate(5,0),ArrayBuffer.empty,11)
//        abcCont = ABCContainer(3,1,2)
//       // if markBoard.calculateScore(pathsBottomToTop, abcCont)==2024
//       // if markBoard.calculateScore(pathsTopToBottom, abcCont)==2024
//        _=println(s"found ${pathsBottomToTop.path.map(_.toString)}")
//        _=println(s"found ${pathsTopToBottom.path.map(_.toString)}")
//  } yield (pathsBottomToTop,pathsTopToBottom)
}
