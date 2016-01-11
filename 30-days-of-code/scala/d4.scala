class Person {

  var age: Int = 0

  def this(initial_Age:Int) = {
    // Add some more code to run some checks on initial_Age
    this()
    if (initial_Age < 0) 
      println("This person is not valid, setting age to 0.")
    else 
      age = initial_Age
  }        

  def amIOld(): Unit = {
    // Do some computations in here and print out the correct statement to the console 
    val printStr = age match {
      case x if x < 13 => "You are young."
      case x if x < 18 => "You are a teenager."
      case _ => "You are old."
    }
    println(printStr)
  }

  def yearPasses(): Unit = {
    // Increment the age of the person in here
    age += 1
  }  

}
object Solution {

  def main(args: Array[String]) {
    var T=scala.io.StdIn.readInt()
    var i=0
    for(i<-1 to T){        
      var age=scala.io.StdIn.readInt()
      var p=new Person(age)
      p.amIOld()
      var j=0
      for(j<-1 to 3){
        p.yearPasses()
      }
      p.amIOld()
      System.out.println()

    }


  }
}
