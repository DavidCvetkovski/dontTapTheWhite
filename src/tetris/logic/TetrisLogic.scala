package tetris.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.logic.TetrisLogic._


abstract class Tetromino(val anchor: Point, val cellType: CellType) {
  def shape : Seq[Point]
  def rotateLeft(): Tetromino
  def rotateRight(): Tetromino

  def moveLeft(): Tetromino = {
    move(-1, 0)
  }
  def moveDown(): Tetromino = {
    move(0, 1)
  }
  def moveRight(): Tetromino = {
    move(1, 0)
  }

  private def move(deltaX: Int, deltaY: Int): Tetromino = {
    this match {
      case _: StandardTetromino => new StandardTetromino(Point(anchor.x + deltaX, anchor.y + deltaY), cellType)
      case _: OTetromino => new OTetromino(Point(anchor.x + deltaX, anchor.y + deltaY), cellType)
      case _: ITetromino => new ITetromino(Point(anchor.x + deltaX, anchor.y + deltaY), cellType)
    }
  }
}


class StandardTetromino(anchor : Point, cellType : CellType) extends Tetromino(anchor, cellType) {
  override def shape: Seq[Point] = cellType match {
    case JCell => Seq(Point(anchor.x, anchor.y), Point(anchor.x - 1, anchor.y), Point(anchor.x + 1, anchor.y), Point(anchor.x - 1, anchor.y - 1))
    case LCell => Seq(Point(anchor.x, anchor.y), Point(anchor.x - 1, anchor.y), Point(anchor.x + 1, anchor.y), Point(anchor.x + 1, anchor.y - 1))
    case SCell => Seq(Point(anchor.x, anchor.y), Point(anchor.x - 1, anchor.y), Point(anchor.x, anchor.y - 1), Point(anchor.x + 1, anchor.y - 1))
    case TCell => Seq(Point(anchor.x, anchor.y), Point(anchor.x - 1, anchor.y), Point(anchor.x + 1, anchor.y), Point(anchor.x, anchor.y - 1))
    case ZCell => Seq(Point(anchor.x, anchor.y), Point(anchor.x + 1, anchor.y), Point(anchor.x, anchor.y - 1), Point(anchor.x - 1, anchor.y - 1))
  }

  override def rotateLeft(): StandardTetromino = {
    val newShape = shape.map { case Point(x, y) =>
      val relativeX = x - anchor.x
      val relativeY = y - anchor.y
      Point(anchor.x + relativeY, anchor.y - relativeX)
    }

    new StandardTetromino(anchor, cellType) {
      override def shape: Seq[Point] = newShape
    }
  }

  override def rotateRight(): StandardTetromino = {
    val newShape = shape.map { case Point(x, y) =>
      val relativeX = x - anchor.x
      val relativeY = y - anchor.y
      Point(anchor.x - relativeY, anchor.y + relativeX)
    }

    new StandardTetromino(anchor, cellType) {
      override def shape: Seq[Point] = newShape
    }
  }
}

class OTetromino(anchor: Point, cellType: CellType) extends Tetromino(anchor, cellType) {
  override def shape: Seq[Point] = Seq(
    Point(anchor.x, anchor.y - 1),
    Point(anchor.x + 1, anchor.y - 1),
    Point(anchor.x, anchor.y),
    Point(anchor.x + 1, anchor.y)
  )
  override def rotateLeft(): OTetromino = this
  override def rotateRight(): OTetromino = this
}

class ITetromino(anchor: Point, cellType: CellType) extends Tetromino(anchor, cellType) {
  override def shape: Seq[Point] = Seq(
    Point(anchor.x - 1, anchor.y),
    Point(anchor.x, anchor.y),
    Point(anchor.x + 1, anchor.y),
    Point(anchor.x + 2, anchor.y)
  )

  override def rotateLeft(): ITetromino = {
    val newShape = shape.map { case Point(x, y) =>
      val relativeX = x - anchor.x
      val relativeY = y - anchor.y
      Point(anchor.x + relativeY, anchor.y - relativeX)
    }

    new ITetromino(anchor, cellType) {
      override def shape: Seq[Point] = newShape
    }
  }

  override def rotateRight(): ITetromino = {
    val newShape = shape.map { case Point(x, y) =>
      val relativeX = x - anchor.x
      val relativeY = y - anchor.y
      Point(anchor.x - relativeY + 1, anchor.y + relativeX)
    }

    new ITetromino(anchor, cellType) {
      override def shape: Seq[Point] = newShape
    }
  }
}

case class EmptyTetromino(override val anchor: Point = Point(-1, -1), override val cellType: CellType = Empty) extends Tetromino(anchor, cellType) {
  override def shape: Seq[Point] = Seq.empty
  override def rotateLeft(): Tetromino = this
  override def rotateRight(): Tetromino = this
}

case class GameState(spawnNewTetromino : Boolean,
                     cellGrid: Seq[Seq[CellType]],
                     currentTetrominoCellType: CellType,
                     anchor: Point,
                     tetromino: Tetromino = new EmptyTetromino()) {
  val initialTetromino: Tetromino = {
    currentTetrominoCellType match {
      case ICell => new ITetromino(anchor, ICell)
      case JCell => new StandardTetromino(anchor, JCell)
      case LCell => new StandardTetromino(anchor, LCell)
      case OCell => new OTetromino(anchor, OCell)
      case SCell => new StandardTetromino(anchor, SCell)
      case TCell => new StandardTetromino(anchor, TCell)
      case ZCell => new StandardTetromino(anchor, ZCell)
    }
  }
}

/** To implement Tetris, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``tetris`` package.
 */
class TetrisLogic(val randomGen: RandomGenerator,
                  val gridDims : Dimensions,
                  val initialBoard: Seq[Seq[CellType]]) {

  def this(random: RandomGenerator, gridDims : Dimensions) =
    this(random, gridDims, makeEmptyBoard(gridDims))

  def this() =
    this(new ScalaRandomGen(), DefaultDims, makeEmptyBoard(DefaultDims))

  println("GRIDDIMS: ", gridDims.width, gridDims.height)
  println("width / 2", gridDims.width / 2)

  private def isOdd(n: Int): Boolean = {
    if (n % 2 == 1) true else false
  }

  val anchor: Point = Point(if (isOdd(gridDims.width)) gridDims.width / 2 else gridDims.width / 2 - 1, 1)

  val cellType : CellType = randomGen.randomInt(7) match {
    case 0 => ICell
    case 1 => JCell
    case 2 => LCell
    case 3 => OCell
    case 4 => SCell
    case 5 => TCell
    case 6 => ZCell
  }

  private var gameStates : List[GameState] = List(GameState(
    spawnNewTetromino = true,
    cellGrid = initialBoard,
    currentTetrominoCellType = cellType,
    anchor = anchor)
  )

  if (gameStates.last.tetromino == EmptyTetromino()) {
    gameStates = gameStates :+ gameStates.last.copy(tetromino = gameStates.last.initialTetromino)
  }



  println("ZE SHAPEE ", gameStates.last.tetromino.shape)




  // TODO implement me
  def rotateLeft(): Unit = {
    val rotatedTetromino = gameStates.last.tetromino.rotateLeft()

    println("ZE SHAPEE ", rotatedTetromino.shape)

    gameStates = gameStates :+ GameState(
      spawnNewTetromino = false,
      cellGrid = gameStates.last.cellGrid,
      currentTetrominoCellType = gameStates.last.currentTetrominoCellType,
      anchor = gameStates.last.anchor,
      tetromino = rotatedTetromino
    )
  }

  // TODO implement me
  def rotateRight(): Unit = {
    val rotatedTetromino = gameStates.last.tetromino.rotateRight()

    println("ZE SHAPEE ", rotatedTetromino.shape)

    gameStates = gameStates :+ GameState(
      spawnNewTetromino = false,
      cellGrid = gameStates.last.cellGrid,
      currentTetrominoCellType = gameStates.last.currentTetrominoCellType,
      anchor = gameStates.last.anchor,
      tetromino = rotatedTetromino
    )
  }

  // TODO implement me
  def moveLeft(): Unit = {
    val newAnchor = Point(gameStates.last.anchor.x - 1, gameStates.last.anchor.y)
    val movedTetromino = gameStates.last.tetromino.moveLeft()
    gameStates = gameStates :+ GameState(
      spawnNewTetromino = false,
      cellGrid = gameStates.last.cellGrid,
      currentTetrominoCellType = gameStates.last.currentTetrominoCellType,
      anchor = newAnchor,
      tetromino = movedTetromino
    )
  }

  // TODO implement me
  def moveRight(): Unit = {
    val newAnchor = Point(gameStates.last.anchor.x - 1, gameStates.last.anchor.y)
    val movedTetromino = gameStates.last.tetromino.moveRight()
    gameStates = gameStates :+ GameState(
      spawnNewTetromino = false,
      cellGrid = gameStates.last.cellGrid,
      currentTetrominoCellType = gameStates.last.currentTetrominoCellType,
      anchor = newAnchor,
      tetromino = movedTetromino
    )
  }

  // TODO implement me
  def moveDown(): Unit = {
    val newAnchor = Point(gameStates.last.anchor.x - 1, gameStates.last.anchor.y)
    val movedTetromino = gameStates.last.tetromino.moveDown()
    gameStates = gameStates :+ GameState(
      spawnNewTetromino = false,
      cellGrid = gameStates.last.cellGrid,
      currentTetrominoCellType = gameStates.last.currentTetrominoCellType,
      anchor = newAnchor,
      tetromino = movedTetromino
    )
  }

  // TODO implement me
  def doHardDrop(): Unit = {
  }

  // TODO implement me
  def isGameOver: Boolean = false

  // TODO implement me
  def getCellType(p : Point): CellType = {
    if (gameStates.last.tetromino.shape.contains(p))
      gameStates.last.currentTetrominoCellType
    else
      gameStates.last.cellGrid(p.y)(p.x)
  }
}

object TetrisLogic {

  val FramesPerSecond: Int = 5 // change this to speed up or slow down the game

  val DrawSizeFactor = 1.0 // increase this to make the game bigger (for high-res screens)
  // or decrease to make game smaller



  def makeEmptyBoard(gridDims : Dimensions): Seq[Seq[CellType]] = {
    val emptyLine = Seq.fill(gridDims.width)(Empty)
    Seq.fill(gridDims.height)(emptyLine)
  }


  // These are the dimensions used when playing the game.
  // When testing the game, other dimensions are passed to
  // the constructor of GameLogic.
  //
  // DO NOT USE the variable DefaultGridDims in your code!
  //
  // Doing so will cause tests which have different dimensions to FAIL!
  //
  // In your code only use gridDims.width and gridDims.height
  // do NOT use DefaultDims.width and DefaultDims.height


  val DefaultWidth: Int = 10
  val NrTopInvisibleLines: Int = 4
  val DefaultVisibleHeight: Int = 20
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines
  val DefaultDims : Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)


  def apply() = new TetrisLogic(new ScalaRandomGen(),
    DefaultDims,
    makeEmptyBoard(DefaultDims))

}