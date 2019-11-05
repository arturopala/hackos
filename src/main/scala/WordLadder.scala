/**
  * Given two words, beginWord and endWord, and a wordList of approved words,
  * find the length of the shortest transformation sequence from beginWord to endWord such that:
  *
  * Only one letter can be changed at a time
  * Each transformed word must exist in the wordList.
  * Return the length of the shortest transformation sequence,
  * or 0 if no such transformation sequence exists.
  *
  * Note: beginWord does not count as a transformed word.
  * You can assume that beginWord and endWord are not empty and are not the same.
  */
object WordLadder {

  def solve(beginWord: String, endWord: String, wordList: Array[String]): List[String] = {
    val dictionary = wordList.toSet

    println(s"problem: $beginWord > $endWord with word list of size ${wordList.length}: [${wordList.mkString(",")}]")

    val letters: Map[Int, Set[Char]] =
      (wordList.toSet + endWord + beginWord)
        .flatMap(_.zipWithIndex.map(_.swap))
        .groupBy(_._1)
        .mapValues(_.map(_._2).toSet)

    def mutate(word: String, pos: Int, char: Char): String =
      word.substring(0, pos) + char + word.substring(pos + 1, word.length)

    import scala.annotation.tailrec

    @tailrec
    def search(words: List[(String, List[String])]): List[String] =
      words match {
        case Nil => Nil
        case (w, path) :: tail =>
          if (w == endWord) {
            path
          } else {
            val ns =
              if (path.size == dictionary.size) Nil
              else
                (for {
                  p <- 0 until w.length
                  l <- letters(p) - w.charAt(p)
                  next = mutate(w, p, l)
                  if dictionary.contains(next) && !path.contains(next) && !tail.exists(_._1 == next)
                } yield (next, next :: path)).toList
            search(
              tail ::: ns
            )
          }
      }

    val shortest = search((beginWord, List(beginWord)) :: Nil)
    if (shortest.isEmpty) println("there is no path available.")
    else println(s"the shortest path is ${shortest.size} steps: ${shortest.reverse.mkString(" > ")}")
    println()
    shortest
  }

}
