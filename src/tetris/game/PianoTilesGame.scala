package tetris.game

import engine.GameBase
import engine.graphics.Color
import engine.graphics.Rectangle
import engine.graphics.{Point => GraphicsPoint}
import processing.core.PApplet
import processing.event.MouseEvent
import tetris.logic._
import ddf.minim.Minim
import ddf.minim.AudioPlayer

class PianoTilesGame extends GameBase {
  private val gameLogic: PianoTilesLogic = new PianoTilesLogic()
  private val updateTimer = new UpdateTimer(60.toFloat)
  private val gridDims: Dimensions = gameLogic.gridDims
  private val widthInPixels: Int = 400
  private val heightInPixels: Int = 800
  private val widthPerTile: Float = widthInPixels.toFloat / gridDims.width
  private val heightPerTile: Float = widthPerTile * 2
  private val screenArea: Rectangle = Rectangle(GraphicsPoint(0, 0), widthInPixels.toFloat, heightInPixels.toFloat)

  private var pixelOffset: Float = 0
  private def pixelsPerFrame: Float = gameLogic.pixelsPerFrame

  private val minim = new Minim(this)
  private var music: AudioPlayer = _
  private var musicPlaying = false

  override def draw(): Unit = {
    if (gameLogic.isGameOver)  {
      drawGameOverScreen()
      music.close()
      return
    }
    updateState()
    drawGrid()
    drawScore()
  }

  private def drawGameOverScreen(): Unit = {
    setFillColor(Color.White)
    drawRectangle(Rectangle(GraphicsPoint(0, 0), widthInPixels.toFloat, heightInPixels.toFloat))

    setFillColor(Color.Red)
    drawTextCentered("GAME OVER!", 20, screenArea.center)
    drawTextCentered(s"Final Score: ${gameLogic.getScore}", 16, GraphicsPoint(screenArea.center.x, screenArea.center.y + 30))
  }

  private def drawScore(): Unit = {
    setFillColor(Color.LightBlue)
    drawTextCentered(s"${gameLogic.getScore}", 32, GraphicsPoint(screenArea.center.x, 60))
  }



  private def drawGrid(): Unit = {
    for {
      y <- -1 until gridDims.height
      x <- 0 until gridDims.width
    } {
      val point = Point(x, y)
      drawTile(getTileWithOffset(point), gameLogic.getTileType(point))
    }
  }

  private def getTileWithOffset(p: Point): Rectangle = {
    val leftUp = GraphicsPoint(
      p.x * widthPerTile,
      p.y * heightPerTile + pixelOffset
    )
    Rectangle(leftUp, widthPerTile, heightPerTile)
  }

  private def drawTile(area: Rectangle, tileType: TileType): Unit = {
    val color = tileType match {
      case BlackTile => Color.Black
      case WhiteTile => Color.White
    }
    setFillColor(color)
    drawRectangle(area)
  }

  override def mousePressed(event: MouseEvent): Unit = {
    if (gameLogic.isGameOver) return

    val mouseX = event.getX.toFloat
    val mouseY = event.getY.toFloat

    val tileX = (mouseX / widthPerTile).toInt
    val tileY = if ( (mouseY - pixelOffset) < 0) -1
    else ((mouseY - pixelOffset) / heightPerTile).toInt

    if(gameLogic.clickTile(Point(tileX, tileY))){
      if (!musicPlaying) {
        music.loop()

        musicPlaying = true
      }
    }
  }

  override def settings(): Unit = {
    pixelDensity(displayDensity())
    size(widthInPixels, heightInPixels)
  }

  override def setup(): Unit = {
    updateTimer.init()
    music = minim.loadFile("muzik.mp3")
  }


  private def updateState(): Unit = {
    if (updateTimer.timeForNextFrame()) {
      pixelOffset += pixelsPerFrame

      if (pixelOffset >= heightPerTile) {
        pixelOffset = 0
        gameLogic.moveDown()
      }

      updateTimer.advanceFrame()
    }
  }
}

object PianoTilesGame {
  def main(args: Array[String]): Unit = {
    PApplet.main("tetris.game.PianoTilesGame")
  }
}