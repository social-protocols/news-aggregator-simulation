package simulation

import outwatch._
import outwatch.dsl._
import colibri._
import scala.scalajs.js
import js.timers.setInterval
import scala.util.Random

object App {
  val app = {
    val refreshMs = 500
    val minFps    = 30

    val maxSpeed           = Subject.behavior(500.0)
    val lastSpeeds         = flatland.ArrayQueueDouble.create(minFps)
    def avgSpeed           = lastSpeeds.sum / lastSpeeds.length
    var totalSeconds: Long = 0L

    val batchDurationMs = 1000.0 / minFps

    setInterval(batchDurationMs.toDouble) {
      if (maxSpeed.now() > 0) {
        val maxStepsDouble = maxSpeed.now() / minFps
        val fractionalStep = if (Random.nextDouble() < maxStepsDouble - maxStepsDouble.toInt) 1 else 0
        val maxSteps: Int  = maxStepsDouble.toInt + fractionalStep
        // println(f"${nowMs()}: $maxStepsDouble%.2f -> $maxSteps%d")
        def nowMs()     = System.nanoTime().toDouble / 1000000
        val startMs     = nowMs()
        def elapsedMs() = nowMs() - startMs
        var count       = 0
        try {
          while (count < maxSteps && elapsedMs() < batchDurationMs) {
            Simulation.nextStep()
            count += 1
          }
        } catch {
          case e: Throwable =>
            e.printStackTrace()
            maxSpeed.onNext(0)
        }
        totalSeconds += count
        lastSpeeds.add(count * minFps)
      } else {
        lastSpeeds.add(0)
      }
    }

    div(
      cls := "p-5",
      div(
        cls := "flex",
        SpeedSlider(maxSpeed, Observable.intervalMillis(refreshMs).map(_ => avgSpeed))(cls := "mr-2"),
        Observable
          .intervalMillis(refreshMs)
          .map(_ => div(s"(${timeSpanFromSeconds(totalSeconds)})")),
      ),

      // visualization runs independently of simulation
      div(
        cls := "flex items-start",
        Observable.intervalMillis(refreshMs).map[VDomModifier] { _ =>
          div(
            cls := "pt-5 text-xs",
            div(
              span("quality < 1", cls := "bg-blue-500 text-white font-bold px-2 rounded"),
              span(" "),
              span(" > 1", cls := "bg-blue-500 text-white font-bold px-2 rounded"),
            ),
            div(
              span("log(score)", cls := "bg-green-500 text-white font-bold px-2 rounded"),
              " + ",
              span("log(gravity)", cls := "bg-red-500 text-white font-bold px-2 rounded"),
              " = log(upvotes)",
            ),
            div(
              cls := "flex",
              showPage(
                "Newpage",
                (Simulation.submissions.id.length - 1) to Math.max(Simulation.submissions.id.length - 30, 0) by -1,
              )(cls := "w-60 pr-5"),
              showPage(
                "Frontpage",
                Simulation.submissions.frontPageIndices.take(30),
              )(cls := "w-60 pr-5"),
            ),
          )
        },
        Plot.upvoteQualityPlot(
          Observable.intervalMillis(refreshMs).map(_ => 0 until Simulation.submissions.id.length),
          labelX = "quality",
          labelY = "upvotes",
        )(
          attr("width")  := s"300",
          attr("height") := s"300",
        ),
        Plot.voteOverRankDistribution(
          Observable
            .intervalMillis(refreshMs)
            .map(_ => (Simulation.stats.frontpageUpvotesOnRanks, Simulation.stats.frontpageUpvotesOnRanksTimestamps)),
          maxX = 0.03,
          referenceData = Data.voteGainOnTopRankPerSecond,
        )(
          attr("width")  := s"300",
          attr("height") := s"400",
        ),
        Plot.voteOverRankDistribution(
          Observable
            .intervalMillis(refreshMs)
            .map(_ => (Simulation.stats.newpageUpvotesOnRanks, Simulation.stats.newpageUpvotesOnRanksTimestamps)),
          maxX = 0.001,
          referenceData = Data.voteGainOnNewRankPerSecond,
        )(
          attr("width")  := s"300",
          attr("height") := s"400",
        ),
      ),
    )
  }

  def showPage(title: String, submissionIndices: Iterable[Int]) =
    div(
      div(title, cls := "font-bold"),
      submissionIndices.map(showSubmission).toSeq,
    )

  def showSubmission(submissionIndex: Int): HtmlVNode = {
    import Simulation.submissions
    val title      = s"Story ${submissions.id(submissionIndex)}"
    val ageSeconds = Simulation.age(submissions.submissionTimeSeconds(submissionIndex))
    val ageHours   = ageSeconds / 3600.0
    val gravity    = Math.pow(ageHours + 2, 1.8)
    val upvotes    = submissions.upvotes(submissionIndex)
    val quality    = submissions.quality(submissionIndex)
    val score      = submissions.rankingFormulaValue(submissionIndex)
    val subtitle   = s"${upvotes} points, ${timeSpanFromSeconds(ageSeconds)} ago"

    def bar(fraction: Double) =
      div(
        cls   := "h-1",
        width := s"${fraction * 100}%",
      )

    val maxLog = 10
    div(
      cls := "mt-2",
      div(title),
      div(subtitle, cls := "opacity-50"),
      bar(1)(bar(Math.min(quality / 2.0, 1.0))(cls := "bg-blue-400"))(
        cls := "relative",
        // indicator for quality = 1
        div(cls := "absolute top-0 left-1/2 h-1 bg-base-100", width := "1px"),
      ), {
        val logScore   = Math.log(score + 1)   // +1 so the result stays positive
        val logGravity = Math.log(gravity + 1) // +1 so the result stays positive

        div(
          cls := "h-1 flex",
          bar(logScore / maxLog)(cls   := "bg-green-400 shrink-0"),
          bar(logGravity / maxLog)(cls := "bg-red-400 shrink-0"),
        )
      },
    )
  }

  def timeSpanFromSeconds(seconds: Long): String = {
    val ageHours = seconds / 3600
    val ageMin   = (seconds % 3600) / 60
    s"${if (ageHours == 0) s"${ageMin}min" else s"${ageHours}h"}"
  }
}
