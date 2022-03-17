package simulation

import outwatch._
import outwatch.dsl._
import colibri._
import org.scalajs.dom
import flatland._

object Plot {

  def upvoteQualityPlot(submissionIndices: Observable[Iterable[Int]]) =
    canvas(
      cls := "border border-gray-400",
      managedElement { elem =>
        val canvasElement = elem.asInstanceOf[dom.HTMLCanvasElement]
        val width         = canvasElement.width.toDouble;
        val height        = canvasElement.height.toDouble;

        val context =
          elem
            .asInstanceOf[dom.HTMLCanvasElement]
            .getContext("2d")
            .asInstanceOf[dom.CanvasRenderingContext2D];

        // bottom left is (0,0)
        context.translate(0, height);
        context.scale(1, -1);

        context.fillStyle = "rgb(59, 130, 246, 0.3)"

        submissionIndices.foreach { submissionIndices =>
          context.clearRect(0, 0, width, height)

          var minX = Double.PositiveInfinity
          var maxX = 0.0
          Simulation.submissions.quality.foreachElement { x =>
            if (x > maxX) maxX = x
            if (x < minX) minX = x
          }

          var minY = Double.PositiveInfinity
          var maxY = 0.0
          Simulation.submissions.upvotes.foreachElement { y =>
            if (y > maxY) maxY = y.toDouble
            if (y < minY) minY = y.toDouble
          }

          @inline def transform(x: Double, min: Double, max: Double, size: Double) = (x - min) / (max - min) * size
          @inline def transformX(x: Double)                                        = transform(x, minX, maxX, width)
          @inline def transformY(y: Double)                                        = transform(y, minY, maxY, height)

          submissionIndices.foreach { submissionIndex =>
            val x = transformX(Simulation.submissions.quality(submissionIndex))
            val y = transformY(Simulation.submissions.upvotes(submissionIndex).toDouble)
            context.beginPath()
            context.arc(x, y, 3, 0, Math.PI * 2)
            context.fill()
          }
        }
        Cancelable(() => ())
      },
    )

  def voteOverRankDistribution(ranks: Observable[ArrayQueueInt]) =
    canvas(
      cls := "border border-gray-400",
      managedElement { elem =>
        val canvasElement = elem.asInstanceOf[dom.HTMLCanvasElement]
        val width         = canvasElement.width.toDouble;
        val height        = canvasElement.height.toDouble;

        val context =
          elem
            .asInstanceOf[dom.HTMLCanvasElement]
            .getContext("2d")
            .asInstanceOf[dom.CanvasRenderingContext2D];

        // bottom left is (0,0)
        // context.translate(0, height);
        // context.scale(1, -1);

        val upvotesPerRank = new Array[Int](Data.updateSize)

        ranks.foreach { ranks =>
          context.clearRect(0, 0, width, height)

          var maxRank = 0
          ranks.foreachElement { rank =>
            upvotesPerRank(rank) += 1
            if (rank > maxRank) maxRank = rank
          }

          // simulation data
          {
            context.fillStyle = "rgb(59, 130, 246)"
            var maxX = 0.0
            upvotesPerRank.foreachElement { x =>
              if (x > maxX) maxX = x.toDouble
            }

            @inline def transform(x: Double, min: Double, max: Double, size: Double) = (x - min) / (max - min) * size
            @inline def transformX(x: Double)                                        = transform(x, 0, maxX, width)
            @inline def transformY(y: Double)                                        = transform(y, 0, maxRank.toDouble + 1, height)

            upvotesPerRank.foreachIndexAndElement { (rank, upvotes) =>
              val y     = transformY(rank.toDouble)
              val ynext = transformY(rank.toDouble + 1)
              context.fillRect(0, y, transformX(upvotes.toDouble), ynext - y)
            }
          }

          // reference data
          {
            context.fillStyle = "rgb(246, 136, 59)"
            var maxX = 0.0
            Data.voteGainOnTopRankCounts.foreachElement { x =>
              if (x > maxX) maxX = x.toDouble
            }

            @inline def transform(x: Double, min: Double, max: Double, size: Double) = (x - min) / (max - min) * size
            @inline def transformX(x: Double)                                        = transform(x, 0, maxX, width)
            @inline def transformY(y: Double)                                        = transform(y, 0, maxRank.toDouble + 1, height)

            upvotesPerRank.foreachIndex { rank =>
              val upvotes = if (rank < Data.voteGainOnTopRankCounts.length) Data.voteGainOnTopRankCounts(rank) else 0
              val y       = transformY(rank.toDouble)
              val ynext   = transformY(rank.toDouble + 1)
              context.fillRect(0, y, transformX(upvotes.toDouble), (ynext - y) / 2)
            }
          }

        }
        Cancelable(() => ())
      },
    )

}
