package com.prototype

import scala.collection.mutable._
import org.scalacheck.Prop._
import org.scalacheck.Gen._
import org.scalacheck.Commands
import org.scalacheck.{Prop, Arbitrary, Gen, Properties}
import scala.collection.JavaConversions._
import scala.Some
import java.util.List;
import org.scalacheck.Arbitrary._
import com.prototype.ServiceClient._
import com.google.common.base.Optional;

object ServiceSpecification2 extends Commands {

  type Sut = ServiceClient
  // TODO: make it random
  val nonEmptyString: Gen[String] = Gen.value("A string")

  val genTask: Gen[Task] =
    for {
      name <- nonEmptyString
      description <- nonEmptyString
    } yield new Task(Optional.of(name), Optional.of(description))

  val genPerson: Gen[Person] =
    for {
      name <- nonEmptyString
      age <- arbitrary[Int]
      tasks <- Gen.listOf(genTask)
    } yield new Person(name, Optional.of(age), Optional.of(tasks))

  val serviceClient : ServiceClient = new ServiceClient("http://localhost:3000")

  case class State(
    var currentCredentials: Option[(String, String)],
    var currentSession : Option[String],
    var registeredUsers : Set[String],
    var usersData: Map[String, Person])

  def initialState = {
    State(None, None, Set(), Map())
  }

  case object GetPersonList extends Command {
    def run(s: State) = {
      serviceClient.getPersonList(); 
    }

    def nextState(s: State) = s

    postConditions += {
      case (s0, s1, r: ServerResponse[List[Person]]) => 
        s0 == s1 && r.getStatusCode == 200 //&& s0.registeredUsers == Set(asScalaBuffer(r.getContent).map(x => x.getName))
      case _ => false
    }
  }

  case object GetPersonInfo extends Command {
    var personExists = true;
    def run(s: State) = {
      val people = s.usersData.keys
      if (people.isEmpty) {
        personExists = false; 
        serviceClient.getPerson("inexistent_entry");
      } else {
        serviceClient.getPerson(Gen.oneOf(people.toSeq).sample.get)
      }
    }

    def nextState(s: State) = {
      println("Get person info lol")
      s
    }

    postConditions += {
      case (s0, s1, r: ServerResponse[Person]) => 
        s0 == s1 && (personExists && r.getStatusCode == 200) || (!personExists && r.getStatusCode == 404)
      case _ => false
    }
  }

  case class AddPerson(person:Person) extends Command {
    var id = ""
    var result:ServerResponse[String] = _

    def run(s:State) = {
      result
    }

    def nextState(s: State) = {
      println("Adding person")
      result = serviceClient.postPerson(person)
      s.registeredUsers += person.getName
      s.usersData += id -> person
      s
    }
  }

  val genAddPerson : Gen[AddPerson] = for {
    person <- genPerson
  } yield AddPerson(person)
  // TODO implement
  def genCommand(state: State) = oneOf(GetPersonInfo, GetPersonList, genAddPerson)

  def toScalaCollection[T](l: java.util.List[T]): ArrayBuffer[T] = {
    val result = ArrayBuffer[T]()
    var i :Int = 0;
    while (i < l.length) {
      result.append(l.get(i));
      i += 1
    }
    result
  }
}

object Runner {
  val rnd = new java.util.Random(100)
  val parms = org.scalacheck.Test.Params(3, 4, 3, 6, rnd, 1)

  def apply() = {
    ServiceSpecification2.check(parms)
  }

  def main(args: Array[String]) = apply()
}
