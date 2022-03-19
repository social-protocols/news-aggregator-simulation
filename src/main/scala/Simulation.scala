package simulation

import util.Random.{nextDouble => nextRandomDouble}

import flatland._

object Simulation {
  var nowSeconds     = 0L
  var nextSubmission = Data.nextSubmissionArrivalDelay.sample(1).head
  var nextVote       = Data.nextVoteArrivalDelay.sample(1).head

  def age(submissionTimeSeconds: Long) = submissionTimeSeconds - nowSeconds

  object submissions {
    val id                    = flatland.ArrayQueueLong.create(Data.updateSize)
    val quality               = flatland.ArrayQueueDouble.create(Data.updateSize)
    val upvotes               = flatland.ArrayQueueInt.create(Data.updateSize)
    val submissionTimeSeconds = flatland.ArrayQueueLong.create(Data.updateSize)
    val rankingFormulaValue   = flatland.ArrayQueueDouble.create(Data.updateSize)

    // val cumulativeVoteOnRankProbability = new Array[Double](Data.updateSize)

    private val frontpageIndicesArray = new Array[Int](Data.updateSize)
    val frontPageIndices              = flatland.ArraySliceInt(frontpageIndicesArray, 0, 0)

    def add(
      id: Long = nextId,
      quality: Double = Data.qualityDistribution.sample(1).head,
      upvotes: Int = 1,
      submissionTimeSeconds: Long = nowSeconds,
      rankingFormulaValue: Double = 0,
    ) = {
      // println(submissionTimeSeconds)
      // circular buffer overwrites last if full
      this.id.add(id)
      this.quality.add(quality)
      this.upvotes.add(upvotes)
      this.submissionTimeSeconds.add(submissionTimeSeconds)
      this.rankingFormulaValue.add(rankingFormulaValue)

      recalculateFrontpage()
    }

    def recalculateFrontpage() = {
      // update raking formula values
      this.submissionTimeSeconds.foreachIndexAndElement { (i, submissionTimeSeconds) =>
        this.rankingFormulaValue(i) = rankingFormula(this.upvotes(i), age(submissionTimeSeconds))
      }
      // extract indices of frontpage candidates
      var i = 0
      this.upvotes.foreachIndexAndElement { (queueIndex, upvotes) =>
        if (upvotes >= Data.minScoreToAppearOnFrontpage) {
          frontpageIndicesArray(i) = queueIndex
          i += 1
        }
      }

      scala.util.Sorting.stableSort(
        this.frontpageIndicesArray,
        (a: Int, b: Int) => this.rankingFormulaValue(a) > this.rankingFormulaValue(b),
        0,
        i,
      )

      frontPageIndices.length = i
    }

    private var lastId: Long = 0
    def nextId: Long         = {
      val next = lastId + 1
      lastId = next
      next
    }
  }

  object stats {
    val frontpageUpvotesOnRanks =
      flatland.ArrayQueueInt.create(20000) // should cover more than a day (~16000 upvotes / day)
  }

  def nextStep() = {
    // println(s"nextStep ($nowSeconds), next submission: $nextSubmission")
    val submissionArrives = nowSeconds >= nextSubmission
    if (submissionArrives) {
      // println("submit")
      submissions.add()
      val delta = Data.nextSubmissionArrivalDelay.sample(1).head
      nextSubmission += delta
    }

    if (nowSeconds % Data.updateIntervalSeconds == 0) {
      submissions.recalculateFrontpage()
    }

    val voteArrives = nowSeconds >= nextVote
    if (voteArrives) {
      castVote()
      nextVote += Data.nextVoteArrivalDelay.sample(1).head
    }

    nowSeconds += 1
  }

  def rankingFormula(upvotes: Int, ageSeconds: Long): Double = {
    // http://www.righto.com/2013/11/how-hacker-news-ranking-really-works.html
    val ageHours = ageSeconds / 3600.0
    Math.pow(upvotes - 1.0, 0.8) / Math.pow(ageHours + 2, 1.8)
  }

  def castVote() = {
    val frontpageSize = submissions.frontPageIndices.length
    if (frontpageSize > 0 && nextRandomDouble() > Data.newFrontPageVotingRatio) {
      // frontpage

      // var lastCumulativeProbability = 0.0
      // loop(Math.min(frontpageSize, Data.voteGainOnTopRankPerSecond.length)) { rank =>
      //   val selectedSubmission = submissions.frontPageIndices(rank)
      //   val probability        = submissions.quality(selectedSubmission) * Data.voteGainOnTopRankPerSecond(rank)
      //   lastCumulativeProbability = lastCumulativeProbability + probability
      //   submissions.cumulativeVoteOnRankProbability(rank) = lastCumulativeProbability
      // }
      // val selectedPosition          = nextRandomDouble() * lastCumulativeProbability
      // var selectedRank              = 0
      // while (selectedPosition > submissions.cumulativeVoteOnRankProbability(selectedRank))
      //   selectedRank += 1
      // val selectedSubmission = submissions.frontPageIndices(selectedRank)
      // submissions.upvotes(selectedSubmission) += 1
      // stats.frontpageUpvotesOnRanks.add(selectedRank)

      var didVote = false
      while (!didVote) {
        val selectedRank = Data.voteGainOnTopRankDistribution.sample(1).head
        if (selectedRank < frontpageSize) {
          val selectedSubmission = submissions.frontPageIndices(selectedRank)
          if (Data.qualityDistribution.sample(1).head < submissions.quality(selectedSubmission)) {
            submissions.upvotes(selectedSubmission) += 1
            stats.frontpageUpvotesOnRanks.add(selectedRank)
            didVote = true
          }
        }
      }
    }
    else {
      // newpage
      val storyCount = submissions.upvotes.length
      if (storyCount > 0) {
        var didVote = false
        while (!didVote) {
          val selectedRank = Data.voteGainOnNewRankDistribution.sample(1).head
          if (selectedRank < storyCount) {
            val selectedSubmission = storyCount - 1 - selectedRank
            if (Data.qualityDistribution.sample(1).head < submissions.quality(selectedSubmission)) {
              submissions.upvotes(selectedSubmission) += 1
              didVote = true
            }
          }
        }
      }
    }
  }
}
