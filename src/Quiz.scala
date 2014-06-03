/*Authors: Kyle Jensen and Mark Gapasin
 * 
 */

import swing._
import swing.event._
import TabbedPane._
import BorderPanel.Position._
import scala.io.Source
import scala.util._
import scala.util.Random._
import scala.swing.RadioMenuItem

object Quiz {
  
  case class Spanglish(spanish: String, english: String)
    
  // function that creates a Spanglish object 
  def createObj(line: String) = {
      val p = line.split("\t") // elements are separated by tabs
      val spanish = p(0) 
      val english = p(1)
      Spanglish(spanish, english)  
  }
    
  // imports text file
  val source = Source.fromFile("Spanglish.txt")
     
  // reads text file line by line
  val lines = source.getLines
    
  // puts each line into a list
  val db = lines.map(createObj).toList
    
  source.close

  def main(args: Array[String]): Unit = {
    
    val tabs = new TabbedPane {
      
      pages += new Page("Multiple Choice", new BorderPanel {
        
        var start = db.head
    
        // displays Spanish word to answer
        val spanishField = new Label(db.head.spanish) 
    
        // holds English word to compare user answer with
        val englishField = new Label(db.head.english)  
        
        // verifies that first random word is not the same as the answer
        def check (x : Spanglish) = {
          var newWord = db(Random.nextInt(db.length))
          while(x == newWord){
            newWord = db(Random.nextInt(db.length))
          }
          newWord
        }
        
        // verifies that the second random word is not the same as the answer and first random word
        def check2 (x : Spanglish, x2 : Spanglish ) = {
          var newWord = db(Random.nextInt(db.length))
          while(x == newWord | x2 == newWord){
            newWord = db(Random.nextInt(db.length))
          }
          newWord
        }
        
        var result = check(start)
        var result2 = check2(start, result)
        
        val questionField = new Label("What is the English translation for " + spanishField.text)
        
        var possibleAnswers = List(englishField.text, result.english, result2.english)
        
        // shuffles possible answers so they are never in the same order
        var shuffleList = shuffle(possibleAnswers)

        var mutex = new ButtonGroup
        
        var answer1 = new RadioButton{
          text = shuffleList.head  
        }
        var answer2 = new RadioButton{
          text = shuffleList.tail.head
        }   
        var answer3 = new RadioButton{
          text = shuffleList.last
        }
        var invisibleRadioButton = new RadioButton {
          this.visible = false
        }
        
        // creates a list of 4 radio buttons for each question.
        // fourth radio button is invisible and used to hold the selection at the
        // beginning of each question
        var radios = List(answer1, answer2, answer3, invisibleRadioButton)
        
        mutex.buttons ++= radios 
        
        // selects invisible button in initial start of quiz
        mutex.select(invisibleRadioButton)
        
        val radioButtons = new BoxPanel(Orientation.Vertical) {
          contents ++= radios
        }
        
        val submitButton = new Button {
          text = "Submit Answer"
        }    
        
        // copy of database. used to help iterate through list.
        var dbCopy = db
        // keeps track of number of correct answers
        var correctTotal = 0
        
        // Label listens to submit button and checks which radio button was selected for users answer
        val answerLabel = new Label {
          text = ""
          listenTo(submitButton, radioButtons)
          reactions += {
            case ButtonClicked(submitButton) =>
              // if user correctly answers question
              if(mutex.selected.get.text == englishField.text){
                correctTotal += 1
                text = ("Excellente!   " + spanishField.text + " -> " + englishField.text)
                if (dbCopy.tail.isEmpty){
                    text = ("Excellente!   " + spanishField.text + " -> " + englishField.text)
                    text = ("You got " + correctTotal + "/" + db.length )
                    submitButton.visible = false 
                    restartButton.visible = true
                }
                else {  
                dbCopy = dbCopy.tail
                start = dbCopy.head
                spanishField.text = (dbCopy.head.spanish) 
                englishField.text = (dbCopy.head.english)  

                result = check(start)
                result2 = check2(start, result)
                
                possibleAnswers = List(englishField.text, result.english, result2.english)
                shuffleList = shuffle(possibleAnswers)                
                answer1.text = shuffleList.head
                answer2.text = shuffleList.tail.head
                answer3.text = shuffleList.last
                mutex.select(invisibleRadioButton)      
                questionField.text = ("What is the English translation for " + spanishField.text)
                }
              }
              // else user incorrectly answers question
              else {
                text = ("Lo siento, you are incorrect!   " + spanishField.text + " -> " + englishField.text)
                if(dbCopy.tail.isEmpty){
                  text = ("You got " + correctTotal + "/" + db.length)
                  submitButton.visible = false
                  restartButton.visible = true
                }
                else {
                  dbCopy = dbCopy.tail
                  start = dbCopy.head
                  spanishField.text = (dbCopy.head.spanish) 
                  englishField.text = (dbCopy.head.english) 
                  
                  result = check(start)
                  result2 = check2(start, result)
                
                  possibleAnswers = List(englishField.text, result.english, result2.english)
                  shuffleList = shuffle(possibleAnswers)                
                  answer1.text = shuffleList.head
                  answer2.text = shuffleList.tail.head
                  answer3.text = shuffleList.last
                  mutex.select(invisibleRadioButton)      
                  questionField.text = ("What is the English translation for " + spanishField.text)
                  }
                }
              }
          }
        
        // Function reloads first image
        // executes when restart button is clicked
        def restartQuiz {
          listenTo(restartButton)
          reactions += {
            case ButtonClicked(_) =>
              dbCopy = db
              correctTotal = 0
            
              start = db.head
            
              // displays spanish word to answer
              spanishField.text = (db.head.spanish) 
            
              // holds english word to compare user answer with
              englishField.text = (db.head.english)
            
              result = check(start)
              result2 = check2(start, result)
              possibleAnswers = List(englishField.text, result.english, result2.english)
              shuffleList = shuffle(possibleAnswers)                
              answer1.text = shuffleList.head
              answer2.text = shuffleList.tail.head
              answer3.text = shuffleList.last
              mutex.select(invisibleRadioButton)
              questionField.text = ("What is the English translation for " + spanishField.text)
              spanishField.text =  " " //clear answer
              answerLabel.text = "" //clear message
              submitButton.visible = true
              restartButton.visible = false
              }
          }
      
          // creates a new button for restarting quiz
          // executes when itself is clicked
          val restartButton = new Button{
            text = "Restart Quiz"
            listenTo(this)
            reactions += {
              case ButtonClicked(_) =>
              restartQuiz
            }
          }
          restartButton.visible = false

          // beginning of layout for  multiple choice tabbed page
          border = Swing.EmptyBorder(30, 30, 30, 30)
          layout += questionField -> North
          layout += new BorderPanel{
            border = Swing.EmptyBorder(30, 0, 30, 0)
            layout += radioButtons -> Center
            layout += answerLabel -> South
          } -> Center
          layout += new GridPanel (2,1){
            border = Swing.EmptyBorder(0, 50, 0, 50)
            contents += restartButton
            contents += submitButton
          } ->South
      })
      
      pages += new Page("Fill In Blank", new BorderPanel {

        // displays spanish word to answer
        val spanishField = new Label(db.head.spanish) 
    
        // holds english word to compare user answer with
        val englishField = new TextField(db.head.english)   
    
        // user input field to answer question
        val userAnswerField = new TextField("")
        var correctTotal = 0

        // creates a button to check answer
        val answerButton = new Button {
          text = "Check Answer"  
        }
        
        var dbCopy = db
        
        // creates a label will display if user correctly or incorrectly answered question
        // executes when answer button when clicked
        val answerLabel = new Label {
          text = " "
          listenTo(answerButton)
          reactions += {
            case ButtonClicked(_) =>
              // if user correctly answers question
              if(englishField.text == userAnswerField.text){
                correctTotal += 1
                text = ("Excellente!   " + spanishField.text + " -> " + englishField.text)
                if (dbCopy.tail.isEmpty){
                  text = ("You got " + correctTotal + "/" + db.length )
                  answerButton.visible = false
                  restartButton.visible = true
                }
                else {
                  dbCopy = dbCopy.tail
                  spanishField.text = dbCopy.head.spanish
                  englishField.text = dbCopy.head.english
                  userAnswerField.text = ""
                }
              }
              // else user incorrectly answers question
              else {
                text = ("Lo siento, you are incorrect!   " + spanishField.text + " -> " + englishField.text)
                if(dbCopy.tail.isEmpty){
                  text = ("You got " + correctTotal + "/" + db.length)
                  answerButton.visible = false
                  restartButton.visible = true
                 }
                 else {
                   dbCopy = dbCopy.tail
                   spanishField.text = dbCopy.head.spanish
                   englishField.text = dbCopy.head.english
                   userAnswerField.text = ""
                 }
               }
             }
          }
        
        // Function reloads first image
        // executes when restart button is clicked
        def restartQuiz {
          listenTo(restartButton)
          reactions += {
            case ButtonClicked(_) =>
              dbCopy = db
              
              // displays Spanish word to answer
              spanishField.text = dbCopy.head.spanish
    
              // holds English word to compare user answer with
              englishField.text = dbCopy.head.english
              
              // user input field to answer question
              userAnswerField.text = ("")
              
              correctTotal = 0

              answerButton.visible = true
              restartButton.visible = false
              answerLabel.text = ""
              }
          }
        
      
          // creates a new button for restarting quiz
          // executes when itself is clicked
         val restartButton = new Button{
            text = "Restart Quiz"
            listenTo(this)
            reactions += {
              case ButtonClicked(_) =>
              restartQuiz
            }
          }
          restartButton.visible = false        
      
      // beginning of layout for fill in the blank tabbed page 
      layout += new GridPanel(5,1) {
        border = Swing.EmptyBorder(20, 20, 50, 20)
        contents += new Label {
          text = "Translate the Spanish word into English: "
        }
        contents += spanishField
        contents += new BorderPanel {
          border = Swing.EmptyBorder(10, 20, 10, 20)
          layout += new Label{
            text = "My Answer: "
          } -> West
          layout += userAnswerField -> Center
        }
        contents += answerLabel
        contents += new GridPanel(2,1) {
          border = Swing.EmptyBorder(0, 40, 0, 40)
          contents += restartButton
          contents += answerButton  
        }
      } -> Center   
    })
  }
    
  val ui: Panel = new BorderPanel {
    layout(tabs) = BorderPanel.Position.Center
  }
  
  val top = new MainFrame {
    contents = ui
    title = "Translator v2.0 | Quiz"
    centerOnScreen
    size = new Dimension(450,400)
    
    // this allows the user to close window without exiting app
    import javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE
    peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)
    override def closeOperation() { close() }
    }
  top.open
  }

}