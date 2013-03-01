package info.bethard.timenorm

abstract class Modifier(val timeMLValueOption: Option[String]) {
  def &(that: Modifier): Modifier = {
    if (this == that) {
      this
    } else if (this == Modifier.Exact) {
      that
    } else if (that == Modifier.Exact) {
      this
    } else {
      throw new IllegalArgumentException(
        "cannot combine %s and %s".format(this, that))
    }
  }
}

object Modifier {
  case object Exact extends Modifier(None)
  case object Before extends Modifier(Some("BEFORE"))
  case object After extends Modifier(Some("AFTER"))
  case object OnOrBefore extends Modifier(Some("ON_OR_BEFORE"))
  case object OnOrAfter extends Modifier(Some("ON_OR_AFTER"))
  case object LessThan extends Modifier(Some("LESS_THAN"))
  case object MoreThan extends Modifier(Some("MORE_THAN"))
  case object EqualOrLess extends Modifier(Some("EQUAL_OR_LESS"))
  case object EqualOrMore extends Modifier(Some("EQUAL_OR_MORE"))
  case object Start extends Modifier(Some("START"))
  case object Mid extends Modifier(Some("MID"))
  case object End extends Modifier(Some("END"))
  case object Approx extends Modifier(Some("APPROX"))
  
  val values = Seq(
      Exact,
      Before,
      After,
      OnOrBefore,
      OnOrAfter,
      LessThan,
      MoreThan,
      EqualOrLess,
      EqualOrMore,
      Start,
      Mid,
      End,
      Approx)
  
  private val stringToModifier =
    (for (modifier <- values; value <- modifier.timeMLValueOption) yield value -> modifier).toMap
  
  def valueOf(timeMLValue: String): Modifier = this.stringToModifier(timeMLValue)
}