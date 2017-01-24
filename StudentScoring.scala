/*
Write a method which takes no parameter and generates a Map with key student name and value as ScoreCard. As there can be more than one student with same name, the logic we have to follow is that, if two or more student has same name the key shold be the name of the student and the values (ScoreCard s) should be in a List, otherwise the key should be the student name and value should be the case class ScoreCard. e.g. Map should be Map[String, AnyRef]. 

Write a method which takes input as student name and print the score cards. If it finds one or more than one score card  print all of them other wise print "No data found". The print should be in increasing order of the student id.
*/
case class Student(id: Long, name: String) 
case class Marks(subjectId: Int,studentId: Long, marksObtained: Float) 
case class ScoreCard(studentId: Long, marks: Map[Long,Float],percent: Float)

object StudentScoring extends App{
 	
  val studentList: List[Student] = List(Student(1, "Kunal"), Student(2, "kunal"), Student(3, "Shivangi"), Student(4, "Mahesh"))
  val marksList: List[Marks]= List( Marks(1, 1, 100),Marks(1, 2, 100), Marks(1, 3, 67),Marks(1, 4, 70),Marks(2, 1, 90),Marks(2, 2,100),Marks(2, 3, 90),Marks(2, 4, 95),Marks(3, 1, 85),Marks(3, 2, 80),Marks(3, 3, 80),Marks(3, 4, 80),Marks(4, 1, 60),Marks(4, 2, 60),Marks(4, 3, 60),Marks(4, 4, 60),Marks(4, 1, 60),Marks(4, 2, 60),Marks(4, 3, 60),Marks(4, 4, 60))
 
		
             def reportCardGenerator():List[(String,List[Float])]={
    val tempPair1=marksList.groupBy(x=> x.studentId)                  
    val tempList1=tempPair1.map(x=> (x._1,x._2.map(y=>y.marksObtained))).toList // Creating a list having tuples of total marks with associated studentID
    val result=for(x<-tempList1;y<-studentList if(x._1==y.id))yield (y.name,x._2) // combining the names and marks to get a new list of toppers
    val  percentage=marksList.groupBy(_.studentId).map(_._2.map(_.marksObtained).sum/4)
	println(percentage)
    result
  
}

val myMap=reportCardGenerator().toMap
val newMap=myMap.groupBy(x=>x._1)
println(myMap)

def printScoreCard(name: String): Unit =
{
//   println(name+" marks "+myMap.get(name))
}

printScoreCard("Shivangi")


}





