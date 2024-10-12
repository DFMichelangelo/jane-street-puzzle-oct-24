object ChessboardDFS {

  private val directions = List(
    (2, 1),
    (2, -1),
    (-2, 1),
    (-2, -1),
    (1, 2),
    (1, -2),
    (-1, 2),
    (-1, -2)
  )

  private val boardSize = 6

  private def isValid(x: Int, y: Int, visited: Set[(Int, Int)]): Boolean =
    x >= 0 && y >= 0 && x < boardSize && y < boardSize && !visited.contains((x, y))

  private def dfs(
           start: (Int, Int),
           end: (Int, Int),
           visited: Set[(Int, Int)],
           path: List[(Int, Int)],
         ): List[List[(Int, Int)]] = {
    // If we have reached the destination, return the current path
    if (start == end) return List(path)
    val (x, y) = start
    // Mark the current cell as visited
    val newVisited = visited + start
    // Explore all four possible directions
    directions.flatMap { case (dx, dy) =>
      val newX = x + dx
      val newY = y + dy

      if (isValid(newX, newY, newVisited))
        // Continue the DFS from the new position
        dfs((newX, newY), end, newVisited, path appended (newX, newY))
       else
        // No valid move, return an empty list
        List()
    }
  }

  private def findPaths(start: (Int, Int), end: (Int, Int)): List[List[(Int, Int)]] =
    dfs(start, end, Set(), List(start))


  def main(args: Array[String]): Unit = {
    val start = (0, 0) // starting coordinate
    val end = (5, 5)   // ending coordinate
    val paths = findPaths(start, end)
    println(s"Found ${paths.size} paths:")
    paths.foreach { path =>
      println(path.mkString(" -> "))
    }
  }
}