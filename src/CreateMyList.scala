/*Authors: Kyle Jensen and Mark Gapasin
 * 
 */

import scala.swing._
import BorderPanel.Position._
import scala.swing.event._
import TabbedPane._
import javax.swing.ImageIcon
import scala.io.Source
import scala.swing.RadioMenuItem
import scala.util.Random


object CreateMyList {
      
	case class Spanglish(spanish: String, english: String) //creating the class to hold the spanish and english words
    var db = List[Spanglish]()  //initialize the list that we will use for all the work

    
    def main(args: Array[String]) = {
  
	//these are the two texts boxes the user types in
	//when making their own list
	val userEnglishField = new TextField("")
    val userSpanishField = new TextField("")
    
    //here begins the configuration of the first gui that pops up
    //different than the gui that pops up when you click "start quiz"
    val tabPane = new TabbedPane{ 
      
      ///////// start of Creating List tab ////////// 
      pages += new Page("Create My List", new BorderPanel {
      
      //making most of the buttons  
      val addButton = new Button{
        text = "Add to list!"
      } 
      
      val clearButton = new Button{
        text = "Clear Your List!" 
      }
      
      val openButton = new Button {
        text = "Choose existing list"
      }
      
      val saveButton = new Button {
        text = "Save Your List"
      }
      
      //a label to listen to the clearButton
      //when clicked, clears db, the list of Spanglish 
      var clearLabel = new Label {
        listenTo(clearButton)
        reactions += {
          case ButtonClicked(clearButton) =>
            db = List[Spanglish]() 
        } 
      }
      
     //label to listen to the addButton and the two text fields
     //userSpanishField and userEnglishField
     //when clicked, as long as both fields are filled, it will 
     //add the word to db 
     var addLabel = new Label {
        listenTo(addButton, userSpanishField, userEnglishField)
        reactions += {
          case ButtonClicked(addButton) => {
            if (userEnglishField.text.length > 0 && userSpanishField.text.length > 0) {
              val addSpan = new Spanglish(userSpanishField.text, userEnglishField.text)
              db = db:+addSpan
              userEnglishField.text = ""
              userSpanishField.text = ""
            }
          }
        }
     }     
     
    //ListView that displays the words as you add them to the list 
    //Displays Spanish word only 
    val database = new ListView(db.map(_.spanish)) {  
      listData = Seq(" ")
      listenTo(addButton, clearButton, addLabel, clearLabel, openButton, saveButton) 
      reactions += {
        case ButtonClicked(`addButton`) =>  
          listData = db.map(_.spanish)
        case ButtonClicked(`clearButton`)  =>  
          listData = Seq(" ")
        case ButtonClicked(`openButton`) =>
          openFile
          listData = db.map(_.spanish)
        case ButtonClicked(`saveButton`) =>
          saveFile
      	}
      
      }
    
    //configuring layout for Create My List Tab
    border = Swing.EmptyBorder(0, 30, 10, 30)
    layout += new GridPanel(3,1) {
      contents += new BorderPanel{
        border = Swing.EmptyBorder(30, 0, 0, 0)
        layout += new Label {
          text = "Enter your Spanish Word  "
        } -> West 
        layout += userSpanishField -> Center
      }
      contents += new BorderPanel{
        border = Swing.EmptyBorder(0, 0, 30, 0)
        layout += new Label {
          text = "Enter your English Word  "
        } -> West
        layout += userEnglishField -> Center
        }
      contents += new GridPanel(2,2) {
        border = Swing.EmptyBorder(0,10,0,10)
        contents += addButton
        contents += clearButton
        contents += saveButton
        contents += openButton
      }
      } -> Center
      layout += new ScrollPane(database) -> South
      })
      
   ///////// start of Study List tab //////////  
   pages += new Page("Study My List", new BorderPanel {
     
    val theFrame = new BorderPanel()    
    val spanishField = new Label("")
    val englishField = new Label("") 
     
    //button when clicked will display the list in the ListView database
    val loadButton = new Button {
      text = "Load My List"
      spanishField.text = ""
      englishField.text = ""
    } 
    
    //this list view displays your list when you click load list
    //when you click the work, your English word and Spanish word 
    //will be displayed
    val database = new ListView(db.map(_.spanish)) {
      listData = Seq("Please load your list to view")
      listenTo(loadButton, selection) 
      reactions += {
        case ButtonClicked(_) =>  //partial function only listen to ButtonClikced
          if (db.isEmpty){ // if user does not initially create a list
            spanishField.text = " "
            englishField.text = " " 
            listData = Seq("There is no list to load")
          }
          else listData = db.map(_.spanish)  // loads list
        case event: SelectionChanged =>  //partial function only listen to SelectionChanged
          if (db.isEmpty){ // if user clears list
            spanishField.text = " "
            englishField.text = " " 
            listData = Seq("There is no list to load")
          }
          else {
          val theDB = db(selection.leadIndex)
          spanishField.text = theDB.spanish
          englishField.text = theDB.english  
          }
       }
     }  
    
        // beginning of layout for study list page
        border = Swing.EmptyBorder(10, 30, 10, 30)
        layout += new BorderPanel {
          layout += new GridPanel(3,1){
            contents += new Label("Please select a Spanish word to translate.")
            contents += new BorderPanel{
              border = Swing.EmptyBorder(10, 50, 10, 50)
              layout += new Label("Spanish: ") -> West
              layout += spanishField -> Center
             }
            contents += new BorderPanel{
              border = Swing.EmptyBorder(10, 50, 10, 50)
              layout += new Label("English: ") -> West
              layout += englishField -> Center
            }           
          } -> North
        } -> Center 
        layout += new BorderPanel {
          layout += new GridPanel (1,1){
            border = Swing.EmptyBorder(10, 50, 10, 50)
            contents += loadButton
          } -> North
          layout += new ScrollPane(database) {
          }-> Center
        } -> South
        
    }) // end of study list page      
    
    //page for multiple choice quiz
    pages += new Page("Quiz", new BorderPanel {
      val quizButton = new Button{
        text = "Start Quiz"
      }
      
      //listens to quizButton
      //when quizButton is clicked, calls the quiz function 
      //only if new list length is >= 3
      var clearLabel = new Label {
        text = " "
        listenTo(quizButton)
        reactions += {
          case ButtonClicked(quizButton) =>
            if (db.length >= 3){
              quiz
              text = " "
            }
            else text = "Your list is too short. Must be 3 or more words to continue."
        } 
      }
      
      // layout configuration for the quiz tab
      layout += new BorderPanel{
        layout += new GridPanel(1,1){
          border = Swing.EmptyBorder(150, 100, 130, 100)
          contents += quizButton
        }-> Center
        border = Swing.EmptyBorder(0, 0, 10, 0)
        layout += clearLabel -> South
      }-> Center
    	})     
    }
  
    
    val ui: Panel = new BorderPanel {
      layout(tabPane) = BorderPanel.Position.Center
    }
    
    // set values for the main window on CreateMyList
    val mainFrame = new MainFrame {
    contents = ui
    title = "Translator v2.0 | Managing Personal List"
    centerOnScreen
    size = new Dimension(450,400)
    
    // this allows the user to close window without exiting app
    import javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE
    peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)
    override def closeOperation() { close() }
    }
    mainFrame.open
  }
	
  //function used to choose and open a text file with
  //a list in it!
  def openFile {
    val chooser = new FileChooser(null)
    chooser.showOpenDialog(null)
    val src = Source.fromFile(chooser.selectedFile)  //setting src to the selected file
    val lines = src.getLines
    while (lines.hasNext){  
    val spanWord = lines.next
    val engWord = lines.next
    val use = new Spanglish(spanWord, engWord)
    db = db:+use
    }
  src.close()
  }
  
   //function used to save your current list to a text file
  def saveFile{
    val chooser = new FileChooser(null)
    chooser.showSaveDialog(null)
    val pw = new java.io.PrintWriter(chooser.selectedFile) //makes a printwriter to the selected file
    var index = 0
    while (index < db.length){ 
        var spanObj = db(index)
        var spanWord = spanObj.spanish
    	var engWord = spanObj.english
    	pw.println(spanWord)
    	pw.println(engWord)
    	index = index+1
  }
    pw.close()
  }
  
  //this function pops up the windows and lets you do two different quizes
 import scala.util.Random._
  
 def quiz{    
    //this is where all of the UI goes for the quiz
    val quizPane = new TabbedPane{
      
      //multiple choice page
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
            
              // displays Spanish word to answer
              spanishField.text = (db.head.spanish) 
            
              // holds English word to compare user answer with
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
      
      //fill in the blank page
      pages += new Page("Fill in the Blank", new BorderPanel {
        
        // displays Spanish word to answer
        val spanishField = new Label(db.head.spanish) 
    
        // holds English word to compare user answer with
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
    // defines new window to add to the mainframe contents
    val gui: Panel = new BorderPanel {
      layout(quizPane) = BorderPanel.Position.Center
    }
    val mainframe2 = new MainFrame{
    contents = gui 
    title = "Translator v2.0 | Quiz"
    centerOnScreen
    size = new Dimension(450,400)
    
    // this allows the user to close window without exiting app
    import javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE
    peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)
    override def closeOperation() { close() }
    }
   mainframe2.open
  } 
}