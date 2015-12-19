
import ammonite.ops._
import scala.util.Random.nextInt
import org.jsoup.Jsoup

// based off of http://agiliq.com/blog/2009/06/generating-pseudo-random-text-with-markov-chains-u/
object Markov {

  object Transcripts {
    val archiveUrl = "http://www.thisamericanlife.org/radio-archives"

    def fetchLatestId() = Jsoup.connect(archiveUrl).get()
      .select(".episode-archive h3")
      .first().text()
      .split(':').headOption
      .map(_.toInt)

    def episodeUrl(id: Int) = s"http://www.thisamericanlife.org/radio-archives/episode/$id/transcript"

    def fetchTranscript(id: Int) = {
      val url = episodeUrl(id)
      val doc = Jsoup.connect(url).get()
      Option(doc.select(".radio-wrapper").first()).map { wrapper =>
        // drop the heading describing the transcript
        wrapper.select(".radio").remove()
        wrapper.text()
      }
    }

    def download(f: (Int, String) => Any) = {
      fetchLatestId().foreach { max =>
        (max until 0 by -1).foreach { id =>
          fetchTranscript(id).map(f(id, _))
        }
      }
    }

    val resourcesDir = cwd /'src / 'main / 'resources

    def load() = (ls! resourcesDir | read).flatMap(tokenize).toIndexedSeq
  }

  def tokenize(text: String) =
    text.trim.split(' ')
      .filter(_.nonEmpty)
      .map(_.trim).toList

  case class Chain(words: IndexedSeq[String]) {
    type Key = (String, String)
    val init = Map[Key, List[String]]().withDefaultValue(List[String]())
    val grouped = words.sliding(3).foldLeft(init) { case (acc, Seq(a, b, c)) => {
        val key = (a, b)
        acc + (key -> (c :: acc(key)))
      }
    }

    def choose(key: Key) =
      grouped.get(key).map(values => values(nextInt(values.size)))

    def generate(count: Int) = {
      // choose a random starting word
      val startIndex = nextInt(words.size - 1)
      val init = (List[String](), (words(startIndex), words(startIndex + 1)))
      (0 until count).foldLeft(init) { case (cur@(acc, key@(a, b)), _) =>
        choose(key).fold(cur)(c => (a :: acc, (b, c)))
      }._1.reverse
    }
  }

  def main(args: Array[String]): Unit = {
    //TODO: get min number / fallback to max
    args.lift(0) match {
      case Some("update") =>
        Transcripts.download { case (id, result) =>
          val file = Transcripts.resourcesDir / s"this-american-life-$id-transcript"
          println(s"writing '$file'")
          write.over(file, result)
          // a tad bit of throttling
          Thread.sleep(100)
        }
      case _ =>
        val count = args.lift(0).fold(20)(_.toInt)
        val chain = Chain(Transcripts.load())
        (0 until 10).map { _ =>
          val results = chain.generate(count)
          println(results.mkString(" "))
          println
        }
    }
  }
}

