package w3c.sw

object QuotientSet {
  def apply[A]():QuotientSet[A] = QuotientSet(Map[A,Set[A]]())
}

case class QuotientSet[A](quotient:Map[A,Set[A]]) {

  def +(x:A, y:A):QuotientSet[A] = {
    val newQ = (this(x), this(y)) match {
      case (Some((xx, eqX)), Some((yy, eqY))) => quotient - yy + (xx -> (eqX ++ eqY))
      case (None, None) => quotient + (x -> Set(x, y))
      case (Some((xx, eqX)), None) => quotient + (xx -> (eqX + y))
      case (None, Some((yy, eqY))) => quotient + (yy -> (eqY + x))
    }
    QuotientSet(newQ)
  }

  def apply(x:A):Option[(A,Set[A])] = {
    val filtered = for((rep,set) <- this.quotient ; if set contains x) yield(rep,set)
    filtered.size match {
      case 0 => None
      case 1 => Some(filtered.iterator.next)
      case _ => error("you shouldn't be here")
    }
  }

}
