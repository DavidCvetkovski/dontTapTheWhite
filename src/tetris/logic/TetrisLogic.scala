package tetris.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.logic.TetrisLogic._

sealed trait Direction
case object Left extends Direction
case object Right extends Direction
case object Down extends Direction

abstract class Tetromino(val anchor: Point, val cellType: CellType) {
  def shape : Seq[Point]
  def rotate(direction: Direction): Tetromino
  def move(direction: Direction): Tetromino
  protected def getDelta(direction: Direction): (Int, Int) = {
    direction match {
      case Down => (0, 1)
      case Left => (-1, 0)
      case Right => (1, 0)
    }
  }

  protected val x: Int = anchor.x
  protected val y: Int = anchor.y
}


class StandardTetromino(anchor : Point, cellType : CellType) extends Tetromino(anchor, cellType) {
  override def shape : Seq[Point] = cellType match {
    case JCell => Seq(Point(x, y), Point(x - 1, y), Point(x + 1, y), Point(x - 1, y - 1))
    case LCell => Seq(Point(x, y), Point(x - 1, y), Point(x + 1, y), Point(x + 1, y - 1))
    case SCell => Seq(Point(x, y), Point(x - 1, y), Point(x, y - 1), Point(x + 1, y - 1))
    case TCell => Seq(Point(x, y), Point(x - 1, y), Point(x + 1, y), Point(x, y - 1))
    case ZCell => Seq(Point(x, y), Point(x + 1, y), Point(x, y - 1), Point(x - 1, y - 1))
  }

  private def calculateNewShape(direction: Direction): Seq[Point] = {
    shape.map { case Point(x, y) =>
      val relativeX = x - anchor.x
      val relativeY = y - anchor.y
      if (direction == Left)
        Point(anchor.x + relativeY, anchor.y - relativeX)
      else
        Point(anchor.x - relativeY, anchor.y + relativeX)
    }
  }

  override def rotate(direction: Direction): StandardTetromino = {
    new StandardTetromino(anchor, cellType) {
      override def shape: Seq[Point] = calculateNewShape(direction)
    }
  }

  override def move(direction: Direction): Tetromino = {
    val (deltaX, deltaY) = getDelta(direction)
    val newShape = shape.map{case Point(x, y) => Point(x + deltaX, y + deltaY)}
    val newAnchor = Point(anchor.x + deltaX, anchor.y + deltaY)
    new StandardTetromino(newAnchor, cellType) {
      override def shape: Seq[Point] = newShape
    }
  }
}

class OTetromino(anchor: Point, cellType: CellType) extends Tetromino(anchor, cellType) {
  override def shape: Seq[Point] = Seq(
    Point(x, y - 1),
    Point(x + 1, y - 1),
    Point(x, y),
    Point(x + 1, y)
  )

  override def rotate(direction: Direction): OTetromino = this

  override def move(direction: Direction): Tetromino = {
    val (deltaX, deltaY) = getDelta(direction)
    val newShape = shape.map{case Point(x, y) => Point(x + deltaX, y + deltaY)}
    val newAnchor = Point(x + deltaX, y + deltaY)
    new OTetromino(newAnchor, cellType) {
      override def shape: Seq[Point] = newShape
    }
  }
}

class ITetromino(anchor: Point, cellType: CellType) extends Tetromino(anchor, cellType) {
  override def shape: Seq[Point] = Seq(
    Point(x - 1, y),
    Point(x, y),
    Point(x + 1, y),
    Point(x + 2, y)
  )

  private def calculateNewShape(direction: Direction): Seq[Point] = {
    shape.map { case Point(x, y) =>
      val relativeX = x - anchor.x
      val relativeY = y - anchor.y
      direction match {
        case Left => Point(anchor.x + relativeY, anchor.y - relativeX + 1)
        case Right => Point(anchor.x - relativeY + 1, anchor.y + relativeX)
      }
    }
  }

  override def rotate(direction: Direction): Tetromino = {
    new ITetromino(anchor, cellType) {
      override def shape: Seq[Point] = calculateNewShape(direction)
    }
  }

  override def move(direction: Direction): Tetromino = {
    val (deltaX, deltaY) = getDelta(direction)
    val newShape = shape.map{case Point(x, y) => Point(x + deltaX, y + deltaY)}
    val newAnchor = Point(x + deltaX, y + deltaY)
    new ITetromino(newAnchor, cellType) {
      override def shape: Seq[Point] = newShape
    }
  }
}

case class EmptyTetromino(override val anchor: Point = Point(-1, -1), override val cellType: CellType = Empty) extends Tetromino(anchor, cellType) {
  override def shape: Seq[Point] = Seq.empty
  override def rotate(direction: Direction): Tetromino = this
  override def move(direction: Direction): Tetromino = this
}

case class GameState(spawnNewTetromino : Boolean,
                     cellGrid: Seq[Seq[CellType]],
                     currentTetrominoCellType: CellType,
                     anchor: Point,
                     tetromino: Tetromino = new EmptyTetromino(),
                     gameOver : Boolean = false) {
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


  private val anchor: Point = Point(if (isOdd(gridDims.width)) gridDims.width / 2 else gridDims.width / 2 - 1, 1)

  private val cellType: CellType = getRandomCellType()

  private var gameStates : List[GameState] = List(GameState(
    spawnNewTetromino = true,
    cellGrid = initialBoard,
    currentTetrominoCellType = cellType,
    anchor = anchor)
  )

  if (gameStates.last.tetromino == EmptyTetromino()) {
    gameStates = gameStates :+ gameStates.last.copy(tetromino = gameStates.last.initialTetromino)
  }

  private def isOdd(n: Int): Boolean = {
    if (n % 2 == 1) true else false
  }

  private def getRandomCellType() : CellType = {
    randomGen.randomInt(7) match {
      case 0 => ICell
      case 1 => JCell
      case 2 => LCell
      case 3 => OCell
      case 4 => SCell
      case 5 => TCell
      case 6 => ZCell
    }
  }


  private def isNotValidMove(shape: Seq[Point], board: Seq[Seq[CellType]]): Boolean = {

    shape.exists { case Point(x, y) =>
      if (x < 0 || x >= board.head.length || y < 0 || y >= board.length)
        true
      else
        board(y)(x) != Empty
    }
  }


  // TODO implement me
  def rotateLeft(): Unit = {
    if (isGameOver)
      return

    if (isNotValidMove(gameStates.last.tetromino.rotate(Left).shape, gameStates.last.cellGrid))
      return

    val rotatedTetromino = gameStates.last.tetromino.rotate(Left)

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
    if (isGameOver)
      return

    if (isNotValidMove(gameStates.last.tetromino.rotate(Right).shape, gameStates.last.cellGrid))
      return

    val rotatedTetromino = gameStates.last.tetromino.rotate(Right)

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
    if (isGameOver)
      return

    if (isNotValidMove(gameStates.last.tetromino.move(Left).shape, gameStates.last.cellGrid))
      return

    val movedTetromino = gameStates.last.tetromino.move(Left)

    gameStates = gameStates :+ GameState(
      spawnNewTetromino = false,
      cellGrid = gameStates.last.cellGrid,
      currentTetrominoCellType = gameStates.last.currentTetrominoCellType,
      anchor = movedTetromino.anchor,
      tetromino = movedTetromino
    )
  }

  // TODO implement me
  def moveRight(): Unit = {

    if (isGameOver)
      return

    if (isNotValidMove(gameStates.last.tetromino.move(Right).shape, gameStates.last.cellGrid))
      return

    val movedTetromino = gameStates.last.tetromino.move(Right)
    gameStates = gameStates :+ GameState(
      spawnNewTetromino = false,
      cellGrid = gameStates.last.cellGrid,
      currentTetrominoCellType = gameStates.last.currentTetrominoCellType,
      anchor = movedTetromino.anchor,
      tetromino = movedTetromino
    )
  }

  // TODO implement me
  def moveDown(): Unit = {
    if (isGameOver)
      return

    if (isNotValidMove(gameStates.last.tetromino.move(Down).shape, gameStates.last.cellGrid)) {
      val lastShape = gameStates.last.tetromino.shape
      val lastTetrominoCellType = gameStates.last.currentTetrominoCellType
      val newCellGrid = gameStates.last.cellGrid.zipWithIndex.map { case (row, y) =>
        row.zipWithIndex.map { case (cell, x) =>

          if (lastShape.contains(Point(x, y))) lastTetrominoCellType else cell
        }
      }
      gameStates = gameStates :+ GameState(
        spawnNewTetromino = true,
        cellGrid = newCellGrid,
        currentTetrominoCellType = gameStates.last.currentTetrominoCellType,
        anchor = gameStates.last.anchor,
        tetromino = gameStates.last.tetromino
      )

      gameStates = gameStates :+ gameStates.last.copy(cellGrid = updatedCellGrid(gameStates.last.cellGrid))

      spawnNewTetromino()

      return
    }
    val movedTetromino = gameStates.last.tetromino.move(Down)
    gameStates = gameStates :+ GameState(
      spawnNewTetromino = false,
      cellGrid = gameStates.last.cellGrid,
      currentTetrominoCellType = gameStates.last.currentTetrominoCellType,
      anchor = movedTetromino.anchor,
      tetromino = movedTetromino
    )

  }

  private def gameOver(shape : Seq[Point], board : Seq[Seq[CellType]]) : Boolean = {
    shape.exists{case Point(x, y) => board(y)(x) != Empty}
  }

  private def updatedCellGrid(cellGrid : Seq[Seq[CellType]]) : Seq[Seq[CellType]] = {
    val fullRowIndices = cellGrid.zipWithIndex.collect {
      case (row, index) if !row.contains(Empty) => index
    }

    removeFullRows(cellGrid, fullRowIndices)
  }

  private def removeFullRows(cellGrid : Seq[Seq[CellType]], fullRowIndices : Seq[Int]) : Seq[Seq[CellType]] = {
    val numCols = if (cellGrid.nonEmpty) cellGrid.head.length else 0
    val emptyRows = Seq.fill(fullRowIndices.length)(Seq.fill(numCols)(Empty))

    emptyRows ++ cellGrid.zipWithIndex.filterNot { case (_, index) => fullRowIndices.contains(index) }.map(_._1)
  }

  private def spawnNewTetromino() : Unit = {
    val newCellType = getRandomCellType()

    gameStates = gameStates :+ gameStates.last.copy(spawnNewTetromino = true,
      currentTetrominoCellType = newCellType,
      anchor = anchor,
      tetromino = EmptyTetromino()
    )

    if (gameOver(gameStates.last.initialTetromino.shape, gameStates.last.cellGrid)) {
      gameStates = gameStates.tail :+ gameStates.last.copy(gameOver = true)
    }

    if (gameStates.last.tetromino == EmptyTetromino()) {
      gameStates = gameStates :+ gameStates.last.copy(tetromino = gameStates.last.initialTetromino)
    }

  }

  // TODO implement me
  def doHardDrop(): Unit = {
    while (!gameStates.last.spawnNewTetromino) {
      moveDown()
    }
  }

  // TODO implement me
  def isGameOver: Boolean = gameStates.last.gameOver

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