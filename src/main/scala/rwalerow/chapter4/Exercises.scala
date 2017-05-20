package rwalerow.chapter4

object Exercises {

  def variance(xs: Seq[Double]): Option[Double] = {
    if(xs.nonEmpty) {
      val mean = xs.sum / xs.size
      Some(xs.map(x => math.pow(x - mean, 2)).sum / xs.size)
    } else None
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if(xs.isEmpty) Left("Collection is empty")
    else Right(xs.sum / xs.size)


}
