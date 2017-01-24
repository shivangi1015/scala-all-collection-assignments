/*
Write a method which takes no parameter and generates a Map with key student name and value as ScoreCard. As there can be more than one student with same name, the logic we have to follow is that, if two or more student has same name the key shold be the name of the student and the values (ScoreCard s) should be in a List, otherwise the key should be the student name and value should be the case class ScoreCard. e.g. Map should be Map[String, AnyRef]. 

Write a method which takes input as student name and print the score cards. If it finds one or more than one score card  print all of them other wise print "No data found". The print should be in increasing order of the student id.
*/
case class Student(id: Long, name: String) 
case class Marks(subjectId: Long,studentId: Long, marksObtained: Float) 
case class ScoreCard(studentId: Long, marks: Map[Long,Float],percent: Float)

object StudentScoring extends App{
 	
  val studentList: List[Student] = List(Student(1, "Kunal"), Student(2, "kunal"), Student(3, "Shivangi"), Student(4, "Mahesh"))
  val marksList: List[Marks]= List( Marks(1, 1, 100),Marks(1, 2, 100), Marks(1, 3, 67),Marks(1, 4, 70),Marks(2, 1, 90),Marks(2, 2,100),Marks(2, 3, 90),Marks(2, 4, 95),Marks(3, 1, 85),Marks(3, 2, 80),Marks(3, 3, 80),Marks(3, 4, 80),Marks(4, 1, 60),Marks(4, 2, 60),Marks(4, 3, 60),Marks(4, 4, 60),Marks(4, 1, 60),Marks(4, 2, 60),Marks(4, 3, 60),Marks(4, 4, 60))
 
def generateReportCard()={
val res=studentList.map{std=>
val v1=marksList.filter(x=>x.studentId==std.id).map(z=>(z.subjectId,z.marksObtained) )
val temp=v1.sorted.toMap 
val temp_res=marksList.filter(x=>x.studentId==std.id).map(z=>z.marksObtained)
val result=Map(std.name->ScoreCard(std.id,temp,temp_res.sum/5))
println(result)
}
}
generateReportCard()
println(" ")
def displayReportCard(name:String)={
val list_id=studentList.filter(x=>x.name==name)
list_id.map{ls=>
               val temp_res=marksList.filter(x=>x.studentId==ls.id).map(z=>z.marksObtained)
               val per=temp_res.sum/5
                       
               print(ls.name+"   "+ls.id+"   ")
               temp_res.map(x=>print("  "+x))
               print("    "+per+"%")
               println(" ")
 }
}
println(displayReportCard("Mahesh"))
println(displayReportCard("Shivangi"))

}




