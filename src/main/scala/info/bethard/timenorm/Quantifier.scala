package info.bethard.timenorm

abstract class Quantifier(val timeMLValue: Option[String]) {
  def &(that: Quantifier): Quantifier = {
    if (this == that && this.timeMLValue.isEmpty) {
      this
    } else {
      throw new IllegalArgumentException(
        "cannot combine %s and %s".format(this, that))
    }
  }
}

object Quantifier {
  case object None extends Quantifier(scala.None)
  case object Every extends Quantifier(Some("EVERY"))
  
  val values = Seq[Quantifier](Every)
  
  private val stringToQuantifier =
    (for (quantifier <- values; name <- quantifier.timeMLValue) yield name -> quantifier).toMap
  
  def valueOf(timeMLValue: String): Quantifier = this.stringToQuantifier(timeMLValue)
}