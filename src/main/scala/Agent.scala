class Agent (_attk : Int, _defence : Int, PList : List[Noun], NList : List[Noun]) extends Noun ("I", _attk, _defence) {
  IsPositive = true

  // core logic start from here
  def Appraisal (evnt : Event) : List[String] = {

    // Evaluate Event Positiveness
    EvalEventPos(evnt.Subject, evnt.Pred) match {
      case true => List[String]("Joy")
      case false =>
        // for negative event
        // (IsCoppable, IsRisky)
        EvalCopingPotential(ExtractAcctUnit(evnt), this) match {
          case (true, true) => List("Sadness", "Anger", "Fear")
          case (true, false) => List("Sadness", "Anger")
          case (false, true) => List("Sadness", "Fear")
          case (false, false) => List("Sadness")
        }
    }
    // return output
  }

  // Positiveness Evaluation
  // Evaluate 2-element predicate positiveness
  def EvalEventPos(sub: Noun, pred : Predicate): Boolean = {
    // Evaluate subject positiveness
    SetNounPositiveness(sub)

    // Evaluate Predicate pos and then event pos
    (sub.IsPositive, EvalPredPos(pred)) match {
      case (true, true) | (false, true) => true
      case (true, false) | (false, false) => false
    }
  }

  // set the positiveness of a noun
  def SetNounPositiveness(n : Noun) : Unit = {
    if (PList.contains(n))
      n.IsPositive = true
    else if (NList.contains(n))
      n.IsPositive = false
  }



  // evaluate predicate positiveness
  def EvalPredPos(p : Predicate) : Boolean = {
    p match {
      case Predicate(v, dt, None) =>
        SetNounPositiveness(dt) // set direct target positiveness
        (v.IsPositive, dt.IsPositive) match {
          case (true, true) | (false, false) => true
          case (true, false) | (false, true) => false
        }
      case Predicate(v, dt, Some(idt)) =>
        SetNounPositiveness(dt)
        SetNounPositiveness(idt)
        // Reducing Verb III temporarily to Verb_II to test Predicate_II logic
        (EvalPredPos(Predicate(Verb_II(v.key, v.IsPositive), dt, None)), v, idt.IsPositive) match {
          // 2-element pos | isFor | idt Pos
          case (true, Verb_III(_,_,true), true) => true
          case (true, Verb_III(_,_,true), false) => false
          case (true, Verb_III(_,_,false), true) => false
          case (true, Verb_III(_,_,false), false) => true
          case (false, Verb_III(_,_,true), true) => false
          case (false, Verb_III(_,_,true), false) => true
          case (false, Verb_III(_,_,false), true) => true
          case (false, Verb_III(_,_,false), false) => false
          case _ => println("No Match in EvalPredPos"); false
        }
    }
  }

  // extract Accountable Unit from negative event
  // need to figure out derived negative image of positive subject
  def ExtractAcctUnit(e : Event) : Noun = {
    (e.Subject.IsPositive, EvalPredPos(e.Pred)) match {
      case (false, false) => e.Subject
      case (true, false) => e.Subject
      case (false, true) => e.Subject
      case (true, true) => println("Positive emotion not supposed to go here"); e.Subject
    }
  }

  // evaluate coping potential
  def EvalCopingPotential(au : Noun, self : Noun) : (Boolean, Boolean) = {
    return (au.Defence - self.Attack < 10, self.Defence - au.Attack < 10)
  }

}
