package info.bethard.timenorm.formal

import java.time.temporal.ChronoUnit

import info.bethard.anafora.{Properties, Data, Entity}

object AnaforaReader {

  def number(entity: Entity): Number = Number(entity.properties("Value").toInt)

  def period(entity: Entity)(implicit data: Data): Period = entity.`type` match {
    case "Period" => simplePeriod(entity)
  }

  def simplePeriod(entity: Entity)(implicit data: Data): SimplePeriod = SimplePeriod(
    ChronoUnit.valueOf(entity.properties("Type").toUpperCase()),
    number(entity.properties.entity("Number")),
    entity.properties.get("Modifier") match {
      case None => Modifier.Exact
      case Some(string) => ???
    })

  def interval(properties: Properties)(implicit data: Data): Interval = properties("Interval-Type") match {
    case "DocTime" => DocumentCreationTime
  }

  def lastPeriod(entity: Entity)(implicit data: Data): LastPeriod = {
    LastPeriod(interval(entity.properties), period(entity.properties.entity("Period")))
  }

  def temporal(entity: Entity)(implicit data: Data): Temporal = entity.`type` match {
    case "Number" => number(entity)
    case "Period" => simplePeriod(entity)
    case "Last" =>
      (entity.properties("Semantics"),
        entity.properties.has("Period"),
        entity.properties.has("Repeating-Interval"),
        entity.properties.has("Repeating-Interval-Number")) match {
        case ("Standard", true, false, false) => lastPeriod(entity)
        case _ => ???
      }
  }
}