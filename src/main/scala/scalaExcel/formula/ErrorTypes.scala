
package scalaExcel.formula

/**
 * @param expr What users see when this error is encountered
 */
sealed abstract class ErrType(val expr: String)

/**
 * #DIV/0! Trying to divide by 0
 **/
case object DivBy0 extends ErrType("#DIV/0!")

/**
 * #N/A! A formula or a function inside a formula cannot find the referenced data
 **/
case object NA extends ErrType("#N/A!")

/**
 * #NAME? Text in the formula is not recognized
 **/
case object InvalidName extends ErrType("#NAME?")

/**
 * #NUM! A formula has invalid numeric data for the type of operation
 **/
case object NotNumeric extends ErrType("#NUM!")

/**
 * #NULL! A space was used in formulas that reference multiple ranges; a comma separates range references
 **/
case object Null extends ErrType("#NULL!")

/**
 * #REF! A reference is invalid
 **/
case object InvalidRef extends ErrType("#REF!")

/**
 * #VALUE! The wrong type of operand or function argument is used
 **/
case object InvalidValue extends ErrType("#VALUE!")

/**
 * #CIRCULAR! Custom error type for circular references
 */
case object CircularRef extends ErrType("#CIRCULAR!")

/**
 * #PARSE! Custom error type for parse errors
 */
case object ParserErr extends ErrType("#PARSE!")

/**
 * #ARGS! Custom error type for argument number/type errors
 */
case object FunctionsArgs extends ErrType("#ARGS!")