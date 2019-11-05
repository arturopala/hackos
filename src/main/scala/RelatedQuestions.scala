import scala.annotation.tailrec

/**
  * For the purposes of this problem, suppose that Quora has n questions,
  * and question i takes ti time to read. Some questions are related to each other.
  * If we connect related questions by edges, we get an undirected graph such that
  * there exists exactly one path from any question to another.
  * In other words, the graph of related questions is a tree.
  *
  * Every time Steve reads a question, he will see a list of questions that are directly
  * related to it and will navigate at random to one that he hasn't read yet (all related
  * questions have an equal chance of being viewed). Steve will stop reading once there
  * are no unread related questions left.
  *
  * Given the number of related questions n, an array that contains the estimated
  * reading time for each question t, and an array containing the pairs of
  * related questions edges, which question should we show to Steve first so
  * that we minimize his total expected reading time? It is guaranteed that there is one unique question that is optimal.
  *
  * Here's how the total expected time for question i with q related questions is calculated:
  *
  * Take the time ti that it will take Steve to read this question;
  * Recursively calculate the expected_timej for each related question j without considering the ith question;
  * Add to ti the sum of expected_timej for each j, divided by q, i.e. the answer will be equal to
  * ti + sum(expected_timej) / q.
  */
object RelatedQuestions {

  def solve(n: Int, times: Array[Int], edges: Array[Array[Int]]): Int = {
    val graph: Map[Int, Set[Int]] =
      edges
        .flatMap(a => Seq((a(0), a(1)), (a(1), a(0))))
        .groupBy(_._1)
        .mapValues(_.map(_._2).toSet)
        .withDefaultValue(Set())

    println(graph.toSeq.map { case (f, t) => s"$f -> ${t.mkString(",")}" }.mkString("\n"))

    @tailrec
    def calculateReadingTime(nodes: List[(Int, Int)], read: Set[Int], weight: Double, acc: Double): Double =
      nodes match {
        case Nil => acc
        case (i, d) :: tail =>
          if (read.contains(i)) 0
          else {
            val unread = graph(i).diff(read)
            val next = tail ++ unread.map(j => (j, unread.size)).toList
            calculateReadingTime(next, read + i, weight / d, acc + times(i) * weight)
          }
      }

    val readingTimes =
      graph.keySet.map(i => (i, calculateReadingTime(List((i, 1)), Set(), 1d, 0d)))
    println(s"results are:\n\t${readingTimes.map { case (i, t) => f"$i: $t%4.2f" }.mkString("\n\t")}")
    if (readingTimes.isEmpty) 0 else readingTimes.minBy(_._2)._1
  }

}
