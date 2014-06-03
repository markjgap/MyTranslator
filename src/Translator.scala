/*Authors: Kyle Jensen and Mark Gapasin
 * 
 */

object Translator {

 def main(args: Array[String]): Unit = {
       
     // Spanish to English database
     // Translator's stock study list
	 val db = Map(
	          // first set
	          ("hola" -> "hello"),
              ("adios" -> "bye"),
              ("gracias" -> "thank you"),
              ("de nada" -> "welcome"),
              ("bien" -> "good"),
              ("malo" -> "bad"), 
              ("numero" -> "number"),
              ("uno" -> "one"),
              ("dos" -> "two"),
              ("tres" -> "three"),
              // second set
              ("buenos dias" -> "good morning"),
              ("buenas tardes" -> "good afternoon"),
              ("buenas noches" -> "good evening"),
              ("el padre" -> "father"),
              ("la madre" -> "mother"),
              ("el hermano" -> "brother"),
              ("la hermana" -> "sister"),
              ("el tio" -> "uncle"),
              ("el primo" -> "cousin"),
              ("la familia" -> "family")
              )
    
    // creates a new Map with (index -> spanishWord from db ) 
    // this database will be used to create the questions if user
    // wants to use translators stock study list using index to iterate
    // through lists in sets of 10 words 
    var db1 : Map[Int, String]= Map()
    val indexedMap = db.keys // creates a list of spanish words from stock list
    indexedMap.zipWithIndex foreach { 
	   case (value,index) => val v = value
	                         val i = index
                             db1 += (i -> v) // creates a new Map(index -> value)
                             }
                                        
   var goOn = true
   var db3 = Map[String, String]() // initiates user personalized list
   var spanW = "null" // added spanish word
   var englW = "null" // added english word
   var answer = "null" // user's answer/response
   
   
   println("=======================================================")
   println("Welcome to our Spanish/English Translator")
   println("Please choose an option:")
   println("Enter 1 - Create my own list to study")
   println("Enter 2 - Use Translator's study list")
   println("=======================================================")
   
   answer = readLine()
   
   //this whole block of code asks the user if they want to add their own words
   //and then loops through and lets then add as many as they want
   if(answer == "1"){
     while (goOn == true){
       println("=======================================================")
       println("Enter 1 - Add words to my list")
       println("Enter 2 - Remove words from my list")
       println("Enter 3 - View my personalized list")
       println("Enter 4 - Start Translator")
       println("Enter 5 - Clear your list")
       println("Enter 6 - Quit Translator")
       println("=======================================================")
	   answer = readLine()
	   
	   if (answer == "1"){
         println("Enter your Spanish word : ")
	     spanW = readLine()// user inputs spanish word
	     println("Enter your English word : ")
	     englW = readLine() // user inputs english word
	     db3 += (spanW -> englW) //adds span/eng words to list
	     goOn = true }
       
       if (answer == "2"){
         println("=======================================================")
         println("***** My Personalized List *****")
         db3 foreach {
           case(key, value) => println("Spanish: " + key + " --> English: " + value) }
         println("=======================================================\n")
         println("Enter spanish word to remove: ")
         answer = readLine()
         db3 -= answer 
         println("=======================================================")
         println("***** My UPDATED Personalized List *****")
         db3 foreach {
           case(key, value) => println("Spanish: " + key + " --> English: " + value) }
         println("=======================================================\n")
         goOn = true 
         }
       
       if (answer == "3"){
         println("=======================================================")
         println("***** My Personalized Study List *****")
         db3 foreach {
           case(key, value) => println("Spanish: " + key + " --> English: " + value) }
         println("=======================================================")
         goOn = true }
       
       if (answer == "4"){
         var ok = true
         var count = 0 // keeps count of how many question you answer correct
         println("Welcome to Translator. Please enter the correct English translation for each Spanish word. Buena Suerte!!!\n")
         db3 foreach{
           case(key, value) =>
               println("Spanish: " + key)
               val input = readLine() //prompts for user to input answer
               ok = input == value
               if (ok){
                 println("Excellente!\n")
                 count += 1 }
               else{
                 println("Sorry, nice try. Answer is: " + value + "\n") }
               }
         println("********** You correctly answered " + count + " out of " + db3.size + " **********\n")
       }
       
       if (answer == "5"){
         val db4 = Map[String, String]() //makes a new Map that is empty
         db3 = db4 //sets the old filled map to the new empty Map
       }
       
       if (answer == "6"){
         goOn = false }
       }
     }
   
	// Translator using stock list 
   if(answer == "2"){         
     var goOn = true        
     var count = 0 // keeps count of how many questions you answer correct 
     var key = 0 // used as key for db1 database
     println("Welcome to Translator. Please enter the correct English translation for each Spanish word. Buena Suerte!!!\n")   
    
     while (goOn) {
       // each set contains 10 questions
       for(i <- 0 to 9) {
         var ok = true
	     val question = db1(key) // returns associated value of key in db1 database
	     print("Spanish : ")
	     println(question)
	     print("English : ")
         val input = readLine() // prompts user input
         ok = input == (db(question)) //checks user's answer
       
         if (ok) {
           println("Excellente!\n")
	       count += 1
	       key += 1 }
         else {
           println("Sorry, nice try. Answer is : " + (db(question)) + "\n")
           key += 1 }
         }
       
       // if user gets >= 80% correct on first set, unlocks more words
	   if (count >= 8 && key < 11){
	     println("********** Congratulations! You have UNLOCKED more words! **********\n")  
	   }
	   // if user gets 100% correct on second set,.
	   else if (count == 20){
	     println("You answered " + count + "/" + key + "\n")
	     println("********** Congratulations! You are now a certified bilingual spearker!!! **********\n")
	     goOn = false
	   }
	   // if user gets >= 80% correct on second set.
	   else if (count >= 16 && key > 10){
	      println("You answered " + count + "/" + key + "\n")
	      println("Excellent job!!!")
	      goOn = false
	   }
	   // if user gets at <80% correct, end translator
	   else {
	     println("You answered " + count + "/" + key)
	     println("Scores of 80% or more unlocks more words! Study harder and try again!")
	     goOn = false
	     }
	   } 
	 }
   }//end of main

}
