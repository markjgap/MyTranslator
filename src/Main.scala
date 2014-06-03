/*Authors: Kyle Jensen and Mark Gapasin
 * 
 */

import swing._
import javax.swing.ImageIcon // for importing image
import BorderPanel.Position._ //for borderpanel location

object Main {
  
  def main(args: Array[String]) = {
    val frame = new MainFrame { //this is the frame for the whole main main.scala GUI
      title = "Translator v2.0"
      
      // Creates a new value of a read in image
      val myImage = new Label { 
        icon = new ImageIcon("src/images/translator.gif") //starts looking in main folder
      } 
      //this is placing the translator image on the frame
      //and the phrase "translate yourself" at the bottom
      contents = new BorderPanel {
        layout += myImage -> Center
        layout += new Label {
          border = Swing.EmptyBorder(0, 0, 70, 0)
          text = "Translate yourself."
        } -> South
        } 
      
      //starting the setup of the MenuBar options
      menuBar = new MenuBar {
        contents += new Menu ("Start") {
          contents += new MenuItem(Action("Study") { //click "study" and it calls the Study.scala file
            Study.main(args)
            })
          contents += new MenuItem(Action("Translator Quiz") { //click "translator quiz" and it calls the Quiz.scala file
            Quiz.main(args)
            })
          contents += new Separator
          contents += new MenuItem(Action("Translator Ver 1") {//click "Translator Ver 1" and it calls the Translator.scala file
            Translator.main(args)
            })
          contents += new Separator
          contents += new MenuItem(Action("Exit") { //click exit and it exits (big surprise)
            sys.exit(0)
            })
          } 
        contents += new Menu ("My List") { 
          contents += new MenuItem(Action("Manage My List") { //click "Manage My List" under "My List" and it calls CreateMyList.scala
            CreateMyList.main(args)
          })

    }
  }
  // sets size of Window    
  size = new Dimension(450,300)
  // Centers window on the middle of screen
  centerOnScreen

  }// end of mainFrame
    
    frame.open
  }

}
