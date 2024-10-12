import Mark.{A, B, C}

import java.util.concurrent.Executors
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, ExecutionContext, Future}

object combinations {
  private def product[T](iterables: Seq[Iterable[T]], repeat: Int = 1): Iterator[Seq[T]] = {
    List.fill(repeat)(iterables.map(_.toSeq))
      .flatten.foldLeft(Iterator(Seq.empty[T])) { (result, pool) =>
      result.flatMap(x => pool.iterator.map(y => x :+ y))}
  }

  private def product2[T](iterables: Seq[T], repeat: Int = 1): Iterator[Seq[T]] = {
    List.fill(repeat)(iterables)
      .flatten.foldLeft(Iterator(Seq.empty[T])) { (result, pool) =>
        result.flatMap(x => Iterator(x :+ pool))
      }
  }

  private class ABCContainer(val a: Int, val b: Int, val c: Int):
    override def toString: String = s"a=$a, b=$b, c=$c"
    def getValue(mark: Mark): Int = mark match
      case Mark.A => a
      case Mark.B => b
      case Mark.C => c

  private def calculateScore(seq: Seq[Mark], abcContainer:ABCContainer): Int = {
    // We use `zip` to combine each element with its previous one, then process the score using `foldLeft`
    seq.tail.zip(seq).foldLeft(abcContainer.getValue(seq.head) ) {
      case (score, (curr, prev)) =>
        if (abcContainer.getValue(curr) != abcContainer.getValue(prev)) score *abcContainer.getValue(curr)
        else score +abcContainer.getValue(curr)
    }
  }

  private def checkValidPath(path: Seq[Mark]) = (checkValidPathBottomToTop(path),checkValidPathTopToBottom(path))

  private def checkValidPathBottomToTop(path: Seq[Mark]):ArrayBuffer[ArrayBuffer[MarkCoordinate]]=
    markBoard.setVisited(0,0,1);
    checkValidPath(
      path,
      knightMoves,
      MarkCoordinate(0,0,A,1),
      markBoard,
      2,
      ArrayBuffer.empty,
      MarkCoordinate(5,5,C),
      ArrayBuffer.empty
    )

  private def checkValidPathTopToBottom(path: Seq[Mark]) : ArrayBuffer[ArrayBuffer[MarkCoordinate]]=
    markBoard.setVisited(0,5,1);
    checkValidPath(
    path,
    knightMoves,
    MarkCoordinate(0, 5,A,1),
    markBoard,
    2,
    ArrayBuffer.empty,
    MarkCoordinate(5, 0,C),
    ArrayBuffer.empty
  )

  private def isValidXMove(coordinate: Coordinate, board: MarkBoard): Boolean =
      board.minX <= coordinate.x && coordinate.x <= board.maxX

  private def isValidYMove(coordinate: Coordinate, board: MarkBoard): Boolean =
    board.minY <= coordinate.y && coordinate.y <= board.maxY

  private def isValidMove(coordinate: Coordinate, board: MarkBoard, markToStepIn:Mark): Boolean =
    isValidXMove(coordinate, board)
      && isValidYMove(coordinate, board)
      && !board.checkVisitedCoordinate(coordinate.x, coordinate.y)
      && board.isValidMarkToStepIn(coordinate.x, coordinate.y,markToStepIn)


  private def checkValidPath(
                      pathToFollow:Seq[Mark],
                      knightMoves: Seq[Coordinate],
                      initialCoordinate: MarkCoordinate,
                      board: MarkBoard,
                      moveCount: Int, // 1 based
                      path: ArrayBuffer[MarkCoordinate],
                      target: MarkCoordinate,
                      allPaths: ArrayBuffer[ArrayBuffer[MarkCoordinate]]): ArrayBuffer[ArrayBuffer[MarkCoordinate]] = {
      if (path.size == pathToFollow.size && initialCoordinate.x == target.x && initialCoordinate.y== target.y)
        allPaths.addOne(path.clone())
      else if (moveCount<pathToFollow.size) {
        knightMoves
          .map(move => Coordinate(initialCoordinate.x + move.x, initialCoordinate.y + move.y))
          .filter(isValidMove(_, board,pathToFollow(moveCount)))
          .foreach { newCoordinate =>
            board.setVisited(newCoordinate.x, newCoordinate.y, moveCount)
            val newMarkCoordinate = MarkCoordinate(newCoordinate.x,newCoordinate.y,pathToFollow(moveCount),moveCount)
            path.addOne(newMarkCoordinate)
            checkValidPath(pathToFollow,knightMoves, newMarkCoordinate, board, moveCount + 1, path, target, allPaths)
            board.setVisited(newCoordinate.x, newCoordinate.y, moveCount-1)
            path.remove(path.size-1)
          }
      }
      allPaths
    }

  // Create a thread pool with 4 threads
  implicit val ec: ExecutionContext = ExecutionContext
    .fromExecutor(Executors.newFixedThreadPool(16))

  private def isValidSequence(path:Seq[Mark]): Boolean =
    path.head ==A
      && path.last==C
      && path(path.size-2)!=A
      && path(1)!=C
      && path.count(A=>true) <=12
      && path.count(B=>true) <=12
      && path.count(C=>true) <=12


  def app(): Unit ={
    val start = System.currentTimeMillis()
    val p  = for {
      pathStep<- 20 until 23
      path<-product(Seq(Mark.values),pathStep) //Seq(Seq(A,A,B,B,C,C,C),Seq(A,B,A,A,C,B,C))
      if isValidSequence(path)
      a <- 1 until 51
      b <- 1 until 51
      c <- 1 until 51
      if a+b+c<50
      score=calculateScore(path,ABCContainer(a,b,c))
      if score==2024
      validPaths = checkValidPath(path)
      if validPaths._1.nonEmpty || validPaths._2.nonEmpty
    } yield (validPaths,ABCContainer(a,b,c))

    println(p.map(e=>"\n"+e._1.toString+" "+e._2.toString))
    println(p.size)
    println(s"${ (System.currentTimeMillis() - start)/1000D/60D}  mins")
  }
}
