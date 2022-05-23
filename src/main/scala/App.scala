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
          case e =>
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

          VDomModifier(
            showPage(
              "New",
              (Simulation.submissions.id.length - 1) to Math.max(Simulation.submissions.id.length - 30, 0) by -1,
            ),
            showPage(
              "Frontpage",
              Simulation.submissions.frontPageIndices.take(30),
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
      cls := "p-5 w-80",
      div(title),
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

    div(
      cls := "mt-2",
      div(
        title,
      ),
      bar(1)(bar(Math.min(quality / 2.0, 1.0))(cls := "bg-blue-400"))(
        cls := "bg-gray-100 relative",
        div(cls := "absolute top-0 left-1/2 w-1 h-1 bg-blue-700"),
      ),
      bar(Math.min(upvotes, 500) / 500.0)(cls        := "bg-violet-400"),
      bar(ageSeconds.toDouble / (3600.0 * 48.0))(cls := "bg-green-500"),
      bar(gravity / (20 * 20))(cls                   := "bg-red-400"),
      bar(score / 5)(cls                             := "bg-black"),
      div(subtitle, cls                              := "text-sm opacity-50"),
    )
  }

  def timeSpanFromSeconds(seconds: Long): String = {
    val ageHours = seconds / 3600
    val ageMin   = (seconds % 3600) / 60
    s"${if (ageHours == 0) s"${ageMin}min" else s"${ageHours}h"}"
  }
}
