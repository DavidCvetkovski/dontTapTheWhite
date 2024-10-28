package tetris.logic

import ddf.minim.{AudioPlayer, Minim}
import engine.random.ScalaRandomGen
import tetris.game.PianoTilesGame

sealed trait TileType
case object BlackTile extends TileType
case object WhiteTile extends TileType

case class Tile(position: Point, tileType: TileType)

case class GameState(
                      tiles: Seq[Tile],
                      score: Int = 0,
                      gameOver: Boolean = false
                    )

class PianoTilesLogic {
  val gridDims: Dimensions = Dimensions(4, 4)
  private val randomGen = new ScalaRandomGen()
  private val TilesPerRow = 4
  private var gameStates: List[GameState] = List(initialGameState)
  private var scoreAdder = 10
  var pixelsPerFrame = 3.0f
  private var speedAdder = 0.02f

  private def initialGameState: GameState = GameState(Seq())

  def clickTile(point: Point): Boolean = {
    if (isGameOver) return false

    val clickedTile = gameStates.last.tiles.find(_.position == point)
    clickedTile match {
      case Some(Tile(_, BlackTile)) =>

        if (getScore > 5000) {
          scoreAdder = 1000000
        } else if (getScore > 1300) {
          scoreAdder = 25
          speedAdder = 0.03f
        } else if (getScore > 300) {
          scoreAdder = 15
          speedAdder = 0.04f
        }

        pixelsPerFrame += speedAdder

        val newScore = gameStates.last.score + scoreAdder
        val updatedTiles = gameStates.last.tiles.filterNot(_.position == point)
        val updatedTiles2 = updatedTiles :+ Tile(point, WhiteTile)
        gameStates = gameStates :+ gameStates.last.copy(tiles = updatedTiles2, score = newScore)
        true

      case Some(Tile(_, WhiteTile)) =>
        gameStates = gameStates :+ gameStates.last.copy(gameOver = true)
        false

      case None => false
    }
  }

  def moveDown(): Unit = {
    if (isGameOver) return

    val movedTiles = gameStates.last.tiles.map { tile =>
      tile.copy(position = Point(tile.position.x, tile.position.y + 1))
    }

    val blackTileAtBottom = movedTiles.exists(tile =>
      tile.tileType == BlackTile && tile.position.y >= gridDims.height - 1)

    if (blackTileAtBottom)
      gameStates = gameStates :+ gameStates.last.copy(gameOver = true)
    else {
      val remainingTiles = movedTiles.filter(_.position.y < gridDims.height)

      val blackTileX = randomGen.randomInt(TilesPerRow)
      val newRow = (0 until TilesPerRow).map { x =>
        Tile(Point(x, -1), if (x == blackTileX) BlackTile else WhiteTile)
      }

      gameStates = gameStates :+ gameStates.last.copy(tiles = remainingTiles ++ newRow)
    }
  }

  def getTileType(p: Point): TileType = {
    gameStates.last.tiles.find(_.position == p).map(_.tileType).getOrElse(WhiteTile)
  }

  def getScore: Int = gameStates.last.score

  def isGameOver: Boolean = gameStates.last.gameOver
}