package scalaExcel.model

class UndoRedo[A](val current : A,
                  private val undoStack : List[A] = List(),
                  private val redoStack : List[A] = List()) {
  val undoRedoBufferSize = 10

  /** Do an operation, keeping the current value available as an undo option **/
  def next(nw : A) : UndoRedo[A] = new UndoRedo(
    nw,
    add(current, undoStack),
    redoStack
  )

  /** Redo the previous undone operation **/
  def redo() : (UndoRedo[A], Option[A]) = {
    if(canRedo()) {
      (new UndoRedo(
        redoStack head,
        add(current, undoStack),
        redoStack tail
      ), Some(redoStack head))
    } else {
      noop
    }
  }

  /** Undo the previous operation **/
  def undo() : (UndoRedo[A], Option[A]) = {
    if(canUndo())
      (new UndoRedo(
        undoStack head,
        undoStack tail,
        add(current, redoStack)
      ), Some(undoStack head))
    else
      noop
  }

  def canUndo() = undoStack nonEmpty
  def canRedo() = redoStack nonEmpty

  private def add(a : A, stack : List[A]) = a :: stack.take(undoRedoBufferSize - 1)

  private val noop = (this, None)


}

