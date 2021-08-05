package pye

import klib.fp.types._

final case class ErrorResponse(
    errors: Array[ErrorResponse.Error],
) {

  def to_? : ?[Nothing] =
    Dead(errors.map(_.toThrowable).toList)

}
object ErrorResponse {

  final case class Error(
      message: String,
      stackTrace: Array[Error.StackTrace],
  ) {

    def toThrowable: Throwable = {
      val thr = Message(message)
      thr.setStackTrace(stackTrace.map(_.toStackTraceElement))

      thr
    }

  }
  object Error {

    final case class StackTrace(
        fileName: String,
        className: String,
        methodName: String,
        lineNumber: Int,
    ) {

      def toStackTraceElement: StackTraceElement =
        new StackTraceElement(className, methodName, fileName, lineNumber)

    }
    object StackTrace {

      def fromStackTraceElement(ste: StackTraceElement): StackTrace =
        StackTrace(
          fileName = ste.getFileName,
          className = ste.getClassName,
          methodName = ste.getMethodName,
          lineNumber = ste.getLineNumber,
        )

    }

    def fromThrowable(throwable: Throwable): Error =
      Error(
        message = throwable.getMessage,
        stackTrace = throwable.getStackTrace.map(StackTrace.fromStackTraceElement),
      )

  }

  def fromDead(dead: Dead[Throwable]): ErrorResponse =
    ErrorResponse(
      errors = dead.errors.toArray.map(Error.fromThrowable),
    )

}
