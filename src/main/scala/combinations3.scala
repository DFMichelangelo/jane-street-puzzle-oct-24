import Mark.{A, B, C}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object combinations3 {

  private case class ABCContainer(a: Int, b: Int, c: Int):
    override def toString: String = s"a=$a, b=$b, c=$c"

    def getValue(mark: Mark): Int = mark match
      case Mark.A => a
      case Mark.B => b
      case Mark.C => c

  private def checkValidPathRedRev(abc: ABCContainer):ArrayBuffer[ArrayBuffer[MarkCoordinate]]=
    val start = System.currentTimeMillis()
    markBoard.setVisited(5, 5, 1);
    checkValidPath(
      abc,
      knightMoves,
      MarkCoordinate(5, 5, C, 1),
      markBoard,
      2,
      ArrayBuffer(MarkCoordinate(5, 5, C, 1)),
      MarkCoordinate(0, 0, A),
      ArrayBuffer.empty,
      2024
    )

  //private def checkValidPathRed(abc: ABCContainer):ArrayBuffer[ArrayBuffer[MarkCoordinate]]=
  //  markBoard.setVisited(0, 0, 1);
  //  checkValidPath(
  //    abc,
  //    knightMoves,
  //    MarkCoordinate(0, 0, A, 1),
  //    markBoard,
  //    2,
  //    ArrayBuffer(MarkCoordinate(0, 0, A, 1)),
  //    MarkCoordinate(5, 5, C),
  //    ArrayBuffer.empty,
  //    2024-abc.a
  //  )

  //private def checkValidPathBlue(abc: ABCContainer): ArrayBuffer[ArrayBuffer[MarkCoordinate]] =
  //  markBoard.setVisited(0,5, 1);
  //  checkValidPath(
  //    abc,
  //    knightMoves,
  //    MarkCoordinate(0, 5, A, 1),
  //    markBoard,
  //    2,
  //    ArrayBuffer(MarkCoordinate(0, 5, A, 1)),
  //    MarkCoordinate(5, 0, C),
  //    ArrayBuffer.empty,
  //    2024-abc.a
  //  )


  private def checkValidPathBlueRev(abc: ABCContainer) : ArrayBuffer[ArrayBuffer[MarkCoordinate]]=
    markBoard.setVisited(5,0,1);
    checkValidPath(
      abc,
      knightMoves,
      MarkCoordinate(5, 0,C,1),
      markBoard,
      2,
      ArrayBuffer(MarkCoordinate(5,0,C,1)),
      MarkCoordinate(0, 5,A),
      ArrayBuffer.empty,
      2024
    )

  private def isValidXMove(coordinate: Coordinate, board: MarkBoard): Boolean =
      board.minX <= coordinate.x && coordinate.x <= board.maxX

  private def isValidYMove(coordinate: Coordinate, board: MarkBoard): Boolean =
    board.minY <= coordinate.y && coordinate.y <= board.maxY

  private def isValidMove(
                           coordinateToMoveIn: Coordinate,
                           currentCoordinate: MarkCoordinate,
                           board: MarkBoard,
                           abc:ABCContainer,
                           currentScore:Int): Boolean =
    isValidXMove(coordinateToMoveIn, board)
      && isValidYMove(coordinateToMoveIn, board)
      && !board.checkVisitedCoordinate(coordinateToMoveIn.x, coordinateToMoveIn.y)
      && canMoveToStep(board.getMark(coordinateToMoveIn.x,coordinateToMoveIn.y).get, currentCoordinate.mark, abc, currentScore)

  private def canMoveToStep(newMark: Mark, oldMark:Mark, abc: ABCContainer, currentScore:Int) :Boolean=
    if (newMark==oldMark) true
    else currentScore % abc.getValue(newMark) ==0                                                                                                                     

  private def checkValidPath(
                      abc: ABCContainer,
                      knightMoves: Seq[Coordinate],
                      initialCoordinate: MarkCoordinate,
                      board: MarkBoard,
                      moveCount: Int, // 1 based
                      path: ArrayBuffer[MarkCoordinate],
                      target: MarkCoordinate,
                      allPaths: ArrayBuffer[ArrayBuffer[MarkCoordinate]],
                      currentScore:Int): ArrayBuffer[ArrayBuffer[MarkCoordinate]] = {
      if (initialCoordinate.x == target.x && initialCoordinate.y== target.y && currentScore==abc.a)
        allPaths.addOne(path.clone())
      else
        knightMoves
          .map(move => Coordinate(initialCoordinate.x + move.x, initialCoordinate.y + move.y))
          .filter(e=> isValidMove(e, initialCoordinate,board, abc,currentScore) && moveCount<=20)
          .foreach {newCoordinate =>
            board.setVisited(newCoordinate.x, newCoordinate.y, moveCount)
            val newMarkCoordinate = MarkCoordinate(newCoordinate.x,newCoordinate.y,board.getMark(newCoordinate.x,newCoordinate.y).get,moveCount)
            path.addOne(newMarkCoordinate)
            val oldMarkCoordinate = abc.getValue(initialCoordinate.mark)
            val newScore =
              if initialCoordinate.mark==newMarkCoordinate.mark then currentScore - oldMarkCoordinate
              else currentScore/oldMarkCoordinate
            checkValidPath(abc,knightMoves, newMarkCoordinate, board, moveCount + 1, path, target, allPaths,newScore)
            board.setVisited(newCoordinate.x, newCoordinate.y, -1)
            path.remove(path.size-1)
          }
      allPaths
    }

  @main
  def app(): Unit ={
    println("start")
    val start = System.currentTimeMillis()
    val p = for {
      abc<-Seq(ABCContainer(3,1,2)) //Seq(ABCContainer(3,10,22))
      validPathsRed = checkValidPathRedRev(abc)
      if validPathsRed.nonEmpty
      validPathsBlue = checkValidPathBlueRev(abc)
      if validPathsBlue.nonEmpty

    } yield (validPathsRed,validPathsBlue, abc)
    printPaths(p)
    println(s"${ (System.currentTimeMillis() - start)/1000D/60D}  mins")
  }

  private def calculateScore(seq: Seq[Mark], abcContainer: ABCContainer): Int =
    // We use `zip` to combine each element with its previous one, then process the score using `foldLeft`
    var score = abcContainer.getValue(seq.head)
    for (i <-Range.inclusive(1, seq.length-1)){
      val curr = seq(i)
      val prev=seq(i-1)
      if curr==prev then score =score+ abcContainer.getValue(curr)
      else score = score* abcContainer.getValue(curr)
    }
    score


  private def printPaths(paths: Seq[(ArrayBuffer[ArrayBuffer[MarkCoordinate]], ArrayBuffer[ArrayBuffer[MarkCoordinate]], ABCContainer)]): Unit =
    for ((path1, path2, container) <- paths) {
      println(s"Found path: ${container.toString}")
      println("Path 1:")
      path1.toSet.foreach { subPath =>println("score: "+ calculateScore(subPath.reverse.map(_.mark).toSeq,container) + " path: "+ subPath.reverse.map(e=>e.toPosition + "-"+e.mark).mkString(", "))}
      println("Path 2:")
      path2.toSet.foreach { subPath =>println("score: "+ calculateScore(subPath.reverse.map(_.mark).toSeq,container) + " path: "+subPath.reverse.map(e=>e.toPosition + "-"+e.mark).mkString(", "))}
      println() 
    }

}
