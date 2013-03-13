package info.bethard.timenorm

import org.threeten.bp.temporal.TemporalUnit

case class Frequency(val times: Int, val unit: Option[TemporalUnit] = None) {
  def &(that: Frequency): Frequency = {
    if (this == that && this.unit.isEmpty) {
      this
    } else {
      throw new IllegalArgumentException(
        "cannot combine %s and %s".format(this, that))
    }
  }
}
