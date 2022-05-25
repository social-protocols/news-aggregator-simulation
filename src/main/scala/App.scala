package simulation

import outwatch._
import outwatch.dsl._
import colibri._
import cats.effect.SyncIO

object App {
  val app = {
    val tickTime     = Subject.behavior(1000)
    val speed        = Subject.behavior(0.0)
    val resetTrigger = Subject.behavior(())

    val refreshMs = 500
    val subSteps  = 600
    var lastStep  = Simulation.nowSeconds

    val tick = resetTrigger
      .combineLatest(tickTime)
      .switchMap { case (_, tickTime) =>
        Observable
          .intervalMillis(tickTime)
          .map(_ => if (tickTime == 0) subSteps else 1)
      }
      .publish

    tick.value.foreach { substeps =>
      flatland.loop(substeps) { _ =>
        try {
          Simulation.nextStep()
        } catch {
          case e: Throwable =>
            e.printStackTrace()
            tickTime.onNext(100000)
        }
      }
    }

    div(
      tick.value.scan(0L)((sum, substeps) => sum + substeps).sampleMillis(refreshMs).map(timeSpanFromSeconds),
      div("speed: ", speed, "x"),
      managed(SyncIO(tick.connect())),
      SpeedSlider(tickTime),

      // visualization runs independently of simulation
      div(
        cls := "flex items-start",
        tick.value.sampleMillis(refreshMs).map { _ =>
          speed.onNext((Simulation.nowSeconds - lastStep).toDouble * 1000.0 / refreshMs)
          lastStep = Simulation.nowSeconds

          div(
            cls := "pt-5 px-5 text-xs",
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
          tick.value.sampleMillis(refreshMs).map(_ => 0 until Simulation.submissions.id.length),
          labelX = "quality",
          labelY = "upvotes",
        )(
          attr("width")  := s"300",
          attr("height") := s"300",
        ),
        Plot.voteOverRankDistribution(
          tick.value
            .sampleMillis(refreshMs)
            .map(_ => (Simulation.stats.frontpageUpvotesOnRanks, Simulation.stats.frontpageUpvotesOnRanksTimestamps)),
          maxX = 0.03,
          referenceData = Data.voteGainOnTopRankPerSecond,
        )(
          attr("width")  := s"300",
          attr("height") := s"400",
        ),
        Plot.voteOverRankDistribution(
          tick.value
            .sampleMillis(refreshMs)
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
        div(cls := "absolute top-0 left-1/2 w-1 h-1 bg-base-100"),
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
