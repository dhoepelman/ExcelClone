package scalaExcel.model

class UndoRedo[A](val current : A,
                  val undoRedoBufferSize : Int = 10,
                  private val undoStack : List[A] = List(),
                  private val redoStack : List[A] = List()) {

  /** Do an operation, keeping the current value available as an undo option **/
  def next(nw : A) : UndoRedo[A] = new UndoRedo(
    nw,
    undoRedoBufferSize,
    add(current, undoStack),
    List()
  )

  /** Redo the previous undone operation **/
  def redo() : UndoRedo[A] = {
    if(canRedo())
      new UndoRedo(
        redoStack.head,
        undoRedoBufferSize,
        add(current, undoStack),
        redoStack.tail
      )
    else
      this
  }

  /** Undo the previous operation **/
  def undo() : UndoRedo[A] = {
    if(canUndo())
      new UndoRedo(
        undoStack.head,
        undoRedoBufferSize,
        undoStack.tail,
        add(current, redoStack)
      )
    else
      this
  }

  def canUndo() = undoStack.nonEmpty
  def canRedo() = redoStack.nonEmpty

  private def add(a : A, stack : List[A]) = a :: stack.take(undoRedoBufferSize - 1)
}

