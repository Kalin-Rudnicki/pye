package pye

object CommonRaise {

  sealed trait SubmitOr[+O]
  case object Submit extends SubmitOr[Nothing]
  object SubmitOr {
    final case class Or[+O](or: O) extends SubmitOr[O]
  }

}
