package scalaExcel.model

/**
 * Provides undo and redo functionality
 * @param current The current version of the value
 * @param maxSize The number of undo's and redo's that can be done
 * @tparam T Any immutable value
 */
class UndoRedo[T](val current : T,
                  val maxSize : Int = 10,
                  private val undos : List[T] = List(),
                  private val redos : List[T] = List()) {

  /** Do an operation, keeping the current value available as an undo option **/
  def next(v : T) = new UndoRedo(
    v,
    maxSize,
    add(current, undos),
    List()
  )

  /** Redo the previous undone operation **/
  def redo() =
    if(redos.nonEmpty)
      new UndoRedo(
        redos.head,
        maxSize,
        add(current, undos),
        redos.tail
      )
    else
      this

  /** Undo the previous operation **/
  def undo() =
    if(undos.nonEmpty)
      new UndoRedo(
        undos.head,
        maxSize,
        undos.tail,
        add(current, redos)
      )
    else
      this

  private def add(a : T, stack : List[T]) = a :: stack.take(maxSize - 1)
}

