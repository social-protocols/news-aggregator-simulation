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

  def voteOverRankDistribution(ranks: Observable[(ArrayQueueInt, ArrayQueueLong)]) =
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
        ranks.foreach { case (ranks, timestamps) =>
          context.clearRect(0, 0, width, height)

          // clear upvotesPerRank
          ranks.foreachElement { rank =>
            upvotesPerRank(rank) = 0
          }

          var maxRank = 0
          ranks.foreachElement { rank =>
            upvotesPerRank(rank) += 1
            if (rank > maxRank) maxRank = rank
          }

          var maxX = 0.0
          Data.voteGainOnTopRankPerSecond.foreachElement { x =>
            if (x > maxX) maxX = x.toDouble
          }

          // simulation data
          {
            context.fillStyle = "rgb(59, 130, 246)"
            // upvotesPerRank.foreachElement { x =>
            //   if (x > maxX) maxX = x.toDouble
            // }
            val timeInterval = (timestamps.last - timestamps.first).toDouble

            @inline def transform(x: Double, min: Double, max: Double, size: Double) = (x - min) / (max - min) * size
            @inline def transformX(x: Double)                                        = transform(x, 0, maxX, width)
            @inline def transformY(y: Double)                                        = transform(y, 0, maxRank.toDouble + 1, height)

            upvotesPerRank.foreachIndexAndElement { (rank, upvotes) =>
              val y     = transformY(rank.toDouble)
              val ynext = transformY(rank.toDouble + 1)
              context.fillRect(0, y, transformX(upvotes.toDouble / timeInterval), ynext - y)
            }
          }

          // reference data
          {
            context.fillStyle = "rgb(246, 136, 59)"

            @inline def transform(x: Double, min: Double, max: Double, size: Double) = (x - min) / (max - min) * size
            @inline def transformX(x: Double)                                        = transform(x, 0, maxX, width)
            @inline def transformY(y: Double)                                        = transform(y, 0, maxRank.toDouble + 1, height)

            upvotesPerRank.foreachIndex { rank =>
              val upvotes =
                if (rank < Data.voteGainOnTopRankPerSecond.length) Data.voteGainOnTopRankPerSecond(rank) else 0
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
