// CSC594 Subject-Predicate Event Appraisal System
// Jilei Hao
// 2019-06-09

import scala.collection.mutable.ListBuffer

object Main extends App{
  // Initialize Vocabulary
  println("Welcome to SPEAS!")
  println("Initializing Vocabulary Database")
  var VocabularyDB = InitializeVocabularyDB()

  // Initialize Agent Setting
  println("Initializing Agent Setting...")
  var PList = new ListBuffer[Noun]
  var NList = new ListBuffer[Noun]

  // Build positive list
  PList += VocabularyDB.GetNoun("Messi")
  PList += VocabularyDB.GetNoun("GrandMa")
  PList += VocabularyDB.GetNoun("Elephant")
  PList += VocabularyDB.GetNoun("ButterFly")
  PList += VocabularyDB.GetNoun("Bird")
  PList += VocabularyDB.GetNoun("Eagle")
  PList += VocabularyDB.GetNoun("Match")
  PList += VocabularyDB.GetNoun("This")
  PList += VocabularyDB.GetNoun("House")

  // Build negative list
  NList += VocabularyDB.GetNoun("Tank")
  NList += VocabularyDB.GetNoun("Night-King")
  NList += VocabularyDB.GetNoun("Snake")

  var agent = new Agent(15, 15, PList.toList, NList.toList)

  var quit = false;

  while (!quit){
    println("\r\nType in event in \"Subject|Verb|DT|IDT\" format. n - for all Nouns, v - for all verbs, q - quit")

    var userInput = scala.io.StdIn.readLine()

    if (userInput == "n")
      VocabularyDB.PrintAllNouns
    else if (userInput == "v")
      VocabularyDB.PrintAllVerbs
    else if (userInput == "q")
      quit = true
    else{
      var inputArray = userInput.split('|')


      var sub = VocabularyDB.GetNoun(inputArray(0))
      var v = VocabularyDB.GetVerb(inputArray(1))
      var dt = VocabularyDB.GetNoun(inputArray(2))

      println("Subject: " + sub.key)
      println("Verb: " + v.key)
      println("DT: " + dt.key)

      var result : List[String] = null


      if (inputArray.length > 3){

        var idt = VocabularyDB.GetNoun(inputArray(3))
        println("IDT: " + idt.key)

        result = agent.Appraisal(Event(sub, Predicate(v, dt, Some(idt))))
      }
      else{
        result = agent.Appraisal(Event(sub, Predicate(v, dt, None)))
      }


      println("\r\n=====Result==================")
      for (s <- result){
        println(s)
      }
    }
  }

  def InitializeVocabularyDB() : Vocabulary = {
    var ret = new Vocabulary

    ret.AddNoun(new Noun("Messi", 18, 8))
    ret.AddNoun(new Noun("GrandMa", 15, 3))
    ret.AddNoun(new Noun("Elephant", 29, 35))
    ret.AddNoun(new Noun("Tank", 40, 50))
    ret.AddNoun(new Noun("ButterFly", 1, 2))
    ret.AddNoun(new Noun("Bird", 3, 30))
    ret.AddNoun(new Noun("Eagle", 25, 40))
    ret.AddNoun(new Noun("Night-King", 200, 200))
    ret.AddNoun(new Noun("Snake", 25, 10))
    ret.AddNoun(new Noun("Match", 10, 10))
    ret.AddNoun(new Noun("This", 15, 15))
    ret.AddNoun(new Noun("House", 10, 10))


    // 2-Element Verb: (key, isPos)
    ret.AddVerb(Verb_II("Help", true))
    ret.AddVerb(Verb_II("Damage", false))
    ret.AddVerb(Verb_II("Attack", false))

    // 3-Element Verb: (key, isPos, isFor)
    ret.AddVerb(Verb_III("Build-for", true, true))
    ret.AddVerb(Verb_III("Create-for", false, true))
    ret.AddVerb(Verb_III("Win-against", true, false))
    ret.AddVerb(Verb_III("Lose-against", false, false))

    ret
  }
}

