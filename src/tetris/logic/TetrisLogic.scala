package tetris.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.logic.TetrisLogic._


abstract class Tetromino(val anchor: Point, val cellType: CellType) {
  def shape : Seq[Point]
  def rotateLeft(): Tetromino
  def rotateRight(): Tetromino

  def moveLeft(): Tetromino
  def moveDown(): Tetromino
  def moveRight(): Tetromino

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
//    println("ROTATING AROUND ANCHOR: ", anchor)

    val newShape = shape.map { case Point(x, y) =>
      val relativeX = x - anchor.x
      val relativeY = y - anchor.y
      Point(anchor.x - relativeY, anchor.y + relativeX)
    }

    new StandardTetromino(anchor, cellType) {
      override def shape: Seq[Point] = newShape
    }
  }

  override def moveDown(): Tetromino = {
    val newShape = shape.map{case Point(x, y) => Point(x, y + 1)}
    val newAnchor = Point(anchor.x, anchor.y + 1)
    new StandardTetromino(newAnchor, cellType) {
      override def shape: Seq[Point] = newShape
    }
  }

  override def moveLeft(): Tetromino = {
    val newShape = shape.map{case Point(x, y) => Point(x - 1, y)}
    val newAnchor = Point(anchor.x - 1, anchor.y)
    new StandardTetromino(newAnchor, cellType) {
      override def shape: Seq[Point] = newShape
    }
  }

  override def moveRight(): Tetromino = {
    val newShape = shape.map{case Point(x, y) => Point(x + 1, y)}
    val newAnchor = Point(anchor.x + 1, anchor.y)
    new StandardTetromino(newAnchor, cellType) {
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
  override def moveDown(): Tetromino = {
    val newShape = shape.map{case Point(x, y) => Point(x, y + 1)}
    val newAnchor = Point(anchor.x, anchor.y + 1)
    new OTetromino(newAnchor, cellType) {
      override def shape: Seq[Point] = newShape
    }
  }

  override def moveLeft(): Tetromino = {
    val newShape = shape.map{case Point(x, y) => Point(x - 1, y)}
    val newAnchor = Point(anchor.x - 1, anchor.y)
    new OTetromino(newAnchor, cellType) {
      override def shape: Seq[Point] = newShape
    }
  }

  override def moveRight(): Tetromino = {
    val newShape = shape.map{case Point(x, y) => Point(x + 1, y)}
    val newAnchor = Point(anchor.x + 1, anchor.y)
    new OTetromino(newAnchor, cellType) {
      override def shape: Seq[Point] = newShape
    }
  }
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
      Point(anchor.x + relativeY, anchor.y - relativeX + 1)
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

  override def moveDown(): Tetromino = {
    val newShape = shape.map{case Point(x, y) => Point(x, y + 1)}
    val newAnchor = Point(anchor.x, anchor.y + 1)
    new ITetromino(newAnchor, cellType) {
      override def shape: Seq[Point] = newShape
    }
  }

  override def moveLeft(): Tetromino = {
    val newShape = shape.map{case Point(x, y) => Point(x - 1, y)}
    val newAnchor = Point(anchor.x - 1, anchor.y)
    new ITetromino(newAnchor, cellType) {
      override def shape: Seq[Point] = newShape
    }
  }

  override def moveRight(): Tetromino = {
    val newShape = shape.map{case Point(x, y) => Point(x + 1, y)}
    val newAnchor = Point(anchor.x + 1, anchor.y)
    new ITetromino(newAnchor, cellType) {
      override def shape: Seq[Point] = newShape
    }
  }
}

case class EmptyTetromino(override val anchor: Point = Point(-1, -1), override val cellType: CellType = Empty) extends Tetromino(anchor, cellType) {
  override def shape: Seq[Point] = Seq.empty
  override def rotateLeft(): Tetromino = this
  override def rotateRight(): Tetromino = this
  override def moveDown(): Tetromino = this
  override def moveLeft(): Tetromino = this
  override def moveRight(): Tetromino = this

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

  private val anchor: Point = Point(if (isOdd(gridDims.width)) gridDims.width / 2 else gridDims.width / 2 - 1, 1)

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

  val cellType: CellType = getRandomCellType()

  private var gameStates : List[GameState] = List(GameState(
    spawnNewTetromino = true,
    cellGrid = initialBoard,
    currentTetrominoCellType = cellType,
    anchor = anchor)
  )

  if (gameStates.last.tetromino == EmptyTetromino()) {
    gameStates = gameStates :+ gameStates.last.copy(tetromino = gameStates.last.initialTetromino)
  }



//  println("ZE SHAPEE ", gameStates.last.tetromino.shape)


  private def isNotValidMove(shape: Seq[Point], board: Seq[Seq[CellType]]): Boolean = {

    shape.exists { case Point(x, y) =>
      if (x < 0 || x >= board.head.length || y < 0 || y >= board.length)
        true
      else
        board(y)(x) != Empty
    }
  }
//
//  private def touchingGround(shape: Seq[Point], board: Seq[Seq[CellType]]): Boolean = {
//    val maxY = shape.map(_.y).max
//    val lowestPoints = shape.filter(_.y == maxY)
//    if (groundUnder(lowestPoints.map{case Point(x, y) => Point(x, y)}, board))
//      true
//    else
//      false
//  }
//
//  private def groundUnder(points: Seq[Point], board: Seq[Seq[CellType]]) : Boolean = {
//    points.exists { case Point(x, y) =>
//      if (x < 0 || x >= board.head.length || y < 0 || y >= board.length)
//        true
//      else
//        board(y)(x) != Empty
//    }
//  }


  // TODO implement me
  def rotateLeft(): Unit = {

    if (isNotValidMove(gameStates.last.tetromino.rotateLeft().shape, gameStates.last.cellGrid))
      return

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

    if (isNotValidMove(gameStates.last.tetromino.rotateRight().shape, gameStates.last.cellGrid))
      return

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

    if (isNotValidMove(gameStates.last.tetromino.moveLeft().shape, gameStates.last.cellGrid))
      return
//    val newAnchor = Point(gameStates.last.anchor.x - 1, gameStates.last.anchor.y)
    val movedTetromino = gameStates.last.tetromino.moveLeft()



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

    if (isNotValidMove(gameStates.last.tetromino.moveRight().shape, gameStates.last.cellGrid))
      return
//    val newAnchor = Point(gameStates.last.anchor.x - 1, gameStates.last.anchor.y)
    val movedTetromino = gameStates.last.tetromino.moveRight()
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

//    if (touchingGround(gameStates.last.tetromino.moveDown().shape, gameStates.last.cellGrid)) {
//      println(" TOUCHING GROUNSNSNSNNSNS")
//      return
//    }



    if (isNotValidMove(gameStates.last.tetromino.moveDown().shape, gameStates.last.cellGrid)) {
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

      spawnNewTetromino()

      return
    }
    //    val newAnchor = Point(gameStates.last.anchor.x - 1, gameStates.last.anchor.y)
    val movedTetromino = gameStates.last.tetromino.moveDown()
    gameStates = gameStates :+ GameState(
      spawnNewTetromino = false,
      cellGrid = gameStates.last.cellGrid,
      currentTetrominoCellType = gameStates.last.currentTetrominoCellType,
      anchor = movedTetromino.anchor,
      tetromino = movedTetromino
    )
  }

  private def spawnNewTetromino() : Unit = {
    val newCellType = getRandomCellType()

    gameStates = gameStates :+ gameStates.last.copy(spawnNewTetromino = true,
      currentTetrominoCellType = newCellType,
      anchor = anchor,
      tetromino = EmptyTetromino()
    )

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