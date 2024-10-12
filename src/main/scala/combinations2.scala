import Mark.{A, B, C}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object combinations2 {
  private def product[T](iterables: Seq[Iterable[T]], repeat: Int = 1): Iterator[Seq[T]] =
    List.fill(repeat)(iterables.map(_.toSeq))
      .flatten.foldLeft(Iterator(Seq.empty[T])) { (result, pool) =>
      result.flatMap(x => pool.iterator.map(y => x :+ y))}

  private def product2[T](iterables: Seq[T], repeat: Int = 1): Iterator[Seq[T]] =
    List.fill(repeat)(iterables)
      .flatten.foldLeft(Iterator(Seq.empty[T])) { (result, pool) =>
        result.flatMap(x => Iterator(x :+ pool))
      }

  private case class ABCContainer(a: Int, b: Int, c: Int):
    override def toString: String = s"a=$a, b=$b, c=$c"
    def getValue(mark: Mark): Int = mark match
      case Mark.A => a
      case Mark.B => b
      case Mark.C => c

  private def calculateScore(seq: Seq[Mark], abcContainer:ABCContainer): Int =
    // We use `zip` to combine each element with its previous one, then process the score using `foldLeft` 
    seq.tail.zip(seq).foldLeft(abcContainer.getValue(seq.head)) {
      case (score, (curr, prev)) =>
        val currABC = abcContainer.getValue(curr)
        if (currABC != abcContainer.getValue(prev)) score * currABC
        else score + currABC
    }
  
  private def checkValidPathBottomToTop(abc: ABCContainer):ArrayBuffer[ArrayBuffer[MarkCoordinate]]=
    println(abc.toString)
    val start = System.currentTimeMillis()
    markBoard.setVisited(0, 0, 1);
    val a = checkValidPath(
      abc,
      knightMoves,
      MarkCoordinate(0, 0, A, 1),
      markBoard,
      2,
      ArrayBuffer(MarkCoordinate(0, 0, A, 1)),
      MarkCoordinate(5, 5, C),
      ArrayBuffer.empty
    )
    println(s"${(System.currentTimeMillis() - start) / 1000D}  secs")
    //if (a.nonEmpty) println(s"found: $a, abc=$abc")
    a


  private def checkValidPathTopToBottom(abc: ABCContainer) : ArrayBuffer[ArrayBuffer[MarkCoordinate]]=
    markBoard.setVisited(0,5,1);
    val a = checkValidPath(
      abc,
      knightMoves,
      MarkCoordinate(0, 5,A,1),
      markBoard,
      2,
      ArrayBuffer(MarkCoordinate(0,5,A,1)),
      MarkCoordinate(5, 0,C),
      ArrayBuffer.empty
    )
    //if (a.nonEmpty) println(s"found: $a, abc=$abc")
    a

  private def isValidXMove(coordinate: Coordinate, board: MarkBoard): Boolean =
      board.minX <= coordinate.x && coordinate.x <= board.maxX

  private def isValidYMove(coordinate: Coordinate, board: MarkBoard): Boolean =
    board.minY <= coordinate.y && coordinate.y <= board.maxY

  private def isValidMove(coordinate: Coordinate, board: MarkBoard,knightMoves: Seq[Coordinate], abc:ABCContainer,path: ArrayBuffer[MarkCoordinate]): Boolean =
    isValidXMove(coordinate, board)
      && isValidYMove(coordinate, board)
      && !board.checkVisitedCoordinate(coordinate.x, coordinate.y)
      && scoreLessorEqualThan2024(coordinate, board, path,abc)

  private def scoreLessorEqualThan2024(coordinate: Coordinate, board: MarkBoard,path: ArrayBuffer[MarkCoordinate], abc:ABCContainer) = {
    val a = path.toSeq.appended(MarkCoordinate(coordinate.x ,coordinate.y, board.getMark(coordinate.x,coordinate.y).get)).map(_.mark)
    calculateScore(a,abc)<=2024
  }

  private def checkValidPath(
                      abc: ABCContainer,
                      knightMoves: Seq[Coordinate],
                      initialCoordinate: MarkCoordinate,
                      board: MarkBoard,
                      moveCount: Int, // 1 based
                      path: ArrayBuffer[MarkCoordinate],
                      target: MarkCoordinate,
                      allPaths: ArrayBuffer[ArrayBuffer[MarkCoordinate]]): ArrayBuffer[ArrayBuffer[MarkCoordinate]] = {
      if (initialCoordinate.x == target.x && initialCoordinate.y== target.y && calculateScore(path.toSeq.map(_.mark),abc)==2024)
        allPaths.addOne(path.clone())
      else
        knightMoves
          .map(move => Coordinate(initialCoordinate.x + move.x, initialCoordinate.y + move.y))
          .filter(e=> isValidMove(e, board, knightMoves, abc,path) && moveCount<=36)
          .foreach {newCoordinate =>
            board.setVisited(newCoordinate.x, newCoordinate.y, moveCount)
            val newMarkCoordinate = MarkCoordinate(newCoordinate.x,newCoordinate.y,board.getMark(newCoordinate.x,newCoordinate.y).get,moveCount)
            path.addOne(newMarkCoordinate)
            checkValidPath(abc,knightMoves, newMarkCoordinate, board, moveCount + 1, path, target, allPaths)
            board.setVisited(newCoordinate.x, newCoordinate.y, -1)
            path.remove(path.size-1)
          }
      allPaths
    }

  private def isValidSequence(path:Seq[Mark]): Boolean =
    path.head ==A
      && path.last==C
      && path(path.size-2)!=A
      && path(1)!=C
      && path.count(A=>true) <=12
      && path.count(B=>true) <=12
      && path.count(C=>true) <=12
  
  def app(): Unit ={
    println("start")
    val start = System.currentTimeMillis()
    val abcs  = for {
      a <- 1 until 4 // 51 until 1 by -1
      b <- 1 until 4 // 51 until 1 by -1
      c <- 1 until 4 // 51 until 1 by -1
      if a != b && a!=c && b!=c
      if a + b + c < 50
    } yield ABCContainer(a, b, c)

    println(abcs.size)

    val p = for {
      abc<-abcs//Seq(ABCContainer(3,10,22))
      validPathsBottomToTop <- checkValidPathBottomToTop(abc)
      if validPathsBottomToTop.nonEmpty
      validPathsTopToBottom <- checkValidPathTopToBottom(abc)
      if validPathsTopToBottom.nonEmpty
    } yield (validPathsBottomToTop,validPathsTopToBottom, abc)

    println(p.map(e=>e._1.toString+"-------- "+e._2.toString+"-------- "+e._3.toString+"\n\n\n"))
    println(p.size)
    println(s"${ (System.currentTimeMillis() - start)/1000D/60D}  mins")
  }
}
