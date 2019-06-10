
case class Predicate (vb : Verb, dt : Noun, idt : Option[Noun] = None)


case class Event (Subject : Noun, Pred : Predicate)
