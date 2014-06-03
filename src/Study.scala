/*Authors: Kyle Jensen and Mark Gapasin
 * 
 */

import scala.swing._
import BorderPanel.Position._ // for BorderPanel location
import scala.swing.event._ // for events
import TabbedPane._ 
import javax.swing.ImageIcon // for importing images
import scala.io.Source // for importing txt file

object Study {
  
  case class Spanglish(spanish: String, english: String, loc: String)
    
  // function that creates a Spanglish object 
  def createObj(line: String) = {
    val p = line.split("\t") // elements are separated by tabs
    val spanish = p(0) 
    val english = p(1)
    val loc = p(2)
    Spanglish(spanish, english,loc)   
  }
    
    // imports text file
    val source = Source.fromFile("Spanglish.txt")
  
    // reads text file line by line
    val lines = source.getLines

    
    // puts each line into a List
    val db = lines.map(createObj).toList
   
    source.close
  
  def main(args: Array[String]) = {
     
    val tabs = new TabbedPane {
      
      ////////// start of Study List page //////////
      pages += new Page("Study List", new BorderPanel { 
        
        val spanishField = new Label("")
        val englishField = new Label("")    
        
        // creates a ListView from the db List displaying the Spanish word of each Spanglish object
        // executes when user makes a selection in  list view
        val database = new ListView(db.map(_.spanish)) {
          listenTo(selection) 
          reactions += {
            case event: SelectionChanged =>  //partial function only listens to SelectionChanged
            val theDB = db(selection.leadIndex)
            spanishField.text = theDB.spanish
            englishField.text = theDB.english
          }
        }    
    
      // beginning of layout for study list page
        border = Swing.EmptyBorder(10, 10, 10, 10)
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
            layout += new ScrollPane(database) -> South
          } -> Center     
      }) // end of study list page
    
      ////////// Start of Study Images page //////////
      pages += new Page("Study Images", new BorderPanel {
        
        val spanishField = new Label("")    
        val englishField = new TextField("")  
        val locField = new Label (db.head.loc)
        var dbCopy = db
  
      // creates a Label panel which reads in new Images and displays them
      val myImage = new Label { 
        icon = new ImageIcon(locField.text)
      }      
      
      // Displays Spanish word that relates to image
      def displayAnswer {
        if(dbCopy.isEmpty){
            spanishField.text = ""
            }
            else {
            spanishField.text =  dbCopy.head.spanish
            }
        }

      // creates a button that when clicked displays answer 
      // executes with itself is clicked
      val answerButton = new Button{
        text = "Answer"
          listenTo(this)
          reactions += {
          case ButtonClicked(_) =>
            displayAnswer
        }         
      }      
 
      // Loads images to panel depending on what button is clicked.
      // If next button is clicked loads next image
      // If restart button is clicked load first image
      def imgPanel = new BorderPanel{
        layout += myImage -> Center   
        listenTo(restartButton, nextButton)
        reactions += {
          case ButtonClicked(`restartButton`) =>
            restartImage
          case ButtonClicked(`nextButton`) =>  
            changeImage
        }     
        preferredSize = new Dimension(100,100)
       }
      
      // Function loads next image to view when nextButton is clicked
      def changeImage {
        if(dbCopy.length == 1){
          spanishField.text = "End of Tutorial"  
            }
        else {
            dbCopy = dbCopy.tail
            locField.text = dbCopy.head.loc
            myImage.icon  = new ImageIcon(locField.text)
            spanishField.text =  "" //clear answer
            }
      }
      
      // creates a new button for Next Image
      val nextButton = new Button{
        text = "Next Image"
      }
      
      // Function reloads first image
      // executes when restart button is clicked
      def restartImage {
        listenTo(restartButton)
        reactions += {
          case ButtonClicked(_) =>
            dbCopy = db
            locField.text = dbCopy.head.loc
            myImage.icon  = new ImageIcon(locField.text)
            spanishField.text =  "" //clear answer
        }
      }
      
      // creates a new button for reloading first image
      // executes when itself is clicked
      val restartButton = new Button{
        text = "Restart Tutorial"
          listenTo(this)
          reactions += {
          case ButtonClicked(_) =>
            restartImage
        }
      }
      
        // beginning of layout for study image page
        layout += imgPanel -> Center
        layout += new GridPanel(2,1) {
          border = Swing.EmptyBorder(10, 30, 10, 30)
          contents += spanishField
          contents += new GridPanel(1,3) {
            contents += restartButton
            contents += answerButton 
            contents += nextButton 
          }
        } -> South
    }) // end of study images page
  }
  
  // creates a panel with tabs  
  val ui: Panel = new BorderPanel {
    layout(tabs) = BorderPanel.Position.Center
  }
   
  // mainframe that displays entire page
  val top = new MainFrame { 
    title = "Translator v2.0 | Study"
    contents = ui
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

