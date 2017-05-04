import java.io.FileReader
import java.io.StringReader
import scala.io.Source
import java.util.Scanner

object PgnApp extends App {

  def readPgnFiles = {
    val baseDir = "./"
    val pgnFiles = Seq("53ZURICH")
    val games = pgnFiles.foldLeft(Seq[Game]()) { (allGames, fileName) =>
      println(s"Reading $fileName")
      PGNParser.parseAll(PGNParser.pgnfile, new FileReader(baseDir + fileName + ".pgn")) match {
        case PGNParser.Success(games, _) => allGames ++ games
        case ex @ _                      => println(ex); allGames
      }
    }
    println(s"Total games: ${games.length}")
    games.foldLeft(Tree.create()) { (tree, game) =>
      tree.add(game)
    }
  }

  def playChess(tree: Tree) = {
    def read(prevMoves: Seq[String]): Unit = {
      println("Move? (e.g. e4): ")
      val move = scala.io.StdIn.readLine()
      val newMoves = prevMoves.:+(move)
      val machineMove = tree.getMove(newMoves)
      println(machineMove)
      if (move.isEmpty) () else read(newMoves.:+(machineMove))
    }

    while (true) {
      read(Seq())
    }

  }

  val tree = readPgnFiles
  playChess(tree)
}
