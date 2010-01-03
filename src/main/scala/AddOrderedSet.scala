package w3c.sw
import scala.collection.immutable._

class AddOrderedSet[A](list:List[A]) extends Set[A] {

  def contains(elem: A): Boolean = list.contains(elem)
  def iterator: Iterator[A] = list.reverse.iterator
  def + (elem: A) : AddOrderedSet[A] = if (this contains elem) this else new AddOrderedSet(elem :: list)
  def - (elem: A) : AddOrderedSet[A] = new AddOrderedSet(list filterNot (_ == elem))

}

object AddOrderedSet {
  def apply[A]():AddOrderedSet[A] = AddOrderedSet(List[A]())
  def apply[A](list:List[A]):AddOrderedSet[A] = new AddOrderedSet(list)
  def apply[A](args:A*):AddOrderedSet[A] = AddOrderedSet(args.toList)
}
