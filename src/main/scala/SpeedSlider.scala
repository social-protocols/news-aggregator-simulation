package simulation

import colibri._
import outwatch._
import outwatch.dsl._

object SpeedSlider {
  def apply(maxSpeed: Subject[Double], currentSpeed: Observable[Double]) = {
    val min = 0.0
    val max = 10000.0

    val buttonStyle = cls := "btn btn-ghost btn-xs mr-1"

    div(
      cls := "flex",
      div("speed: "),
      button("stop", onClick.use(0.0) --> maxSpeed, buttonStyle),
      button("realtime", onClick.use(1.0) --> maxSpeed, buttonStyle),
      button("max", onClick.use(max) --> maxSpeed, buttonStyle),
      div(
        input(
          tpe   := "range",
          cls   := "range",
          width := "300px",
          onInput.value.map(_.toDouble) --> maxSpeed,
          value <-- maxSpeed.map(_.toString),
          minAttr := s"${min}",
          maxAttr := s"${max}",
        ),
        div(
          cls := "h-1 bg-accent rounded",
          currentSpeed.map(speed => width := s"${speed / max * 100}%"),
        ),
      ),
      div(
        cls := "ml-2 shrink-0",
        currentSpeed.combineLatestMap(maxSpeed)((speed, maxSpeed) => f"$speed%.0fx / $maxSpeed%.0fx"),
      ),
    )
  }

}
