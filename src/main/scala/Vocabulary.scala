import scala.collection.immutable.HashMap

trait Verb {var key : String; var IsPositive : Boolean}
trait tNoun {var key : String; var Attack : Int; var Defence : Int; var IsPositive : Boolean = true}

class Noun(vkey : String, vattack : Int, vdefence : Int) extends tNoun{
  var key = vkey
  var Attack = vattack
  var Defence = vdefence
}

case class Verb_II (_key : String, _IsPositive : Boolean) extends Verb(){
  var key = _key
  var IsPositive = _IsPositive
}

case class Verb_III (_key : String, _IsPositive : Boolean, IsFor : Boolean) extends Verb{
  var key = _key
  var IsPositive = _IsPositive
}

case class Subject (_key : String, _attack : Int, _defence : Int) extends Noun (_key, _attack, _defence)

case class DT (_key : String, _attack : Int, _defence : Int) extends Noun(_key, _attack, _defence)

case class IDT (_key : String, _attack : Int, _defence : Int) extends Noun(_key, _attack, _defence)


class Vocabulary {
  var VerbTable = new HashMap [String, Verb] ()
  var NounTable = new HashMap [String, Noun] ()

  def AddVerb(v : Verb) : Unit = {
    VerbTable += (v.key -> v)
  }

  def AddNoun(n : Noun) : Unit = {
    NounTable += (n.key -> n)
  }

  def GetVerb(vk : String) : Verb = {
    VerbTable.get(vk) match {
      case Some(v) => v
      case None => null
    }
  }

  def GetNoun(nk : String) : Noun = {
    NounTable.get(nk) match {
      case Some(n) => n
      case None => null
    }
  }

  def PrintAllNouns : Unit = {
    for((k,v) <- NounTable)
      println(k)
  }

  def PrintAllVerbs : Unit = {
    for((k,v) <- VerbTable)
      println(k)
  }
}
