package klib.webServer

import klib.fp.types._

final case class ErrorResponse(
    errors: Array[ErrorResponse.Error],
)
object ErrorResponse {

  final case class Error(
      message: String,
      stackTrace: Array[Error.StackTrace],
  )
  object Error {

    final case class StackTrace(
        fileName: String,
        className: String,
        methodName: String,
        lineNumber: Int,
    )
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
