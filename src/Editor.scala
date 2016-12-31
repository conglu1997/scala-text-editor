// Editor.scala
// Copyright (c) 2015 J. M. Spivey

import Text.Immutable
import Undoable.Change

/** The editor state extended with methods for editor commands. */
class Editor extends Undoable[Editor.Action] {
    /** The buffer being edited. */
    private val ed = new EdBuffer

    /** The display. */
    private var display: Display = null
    
    /** Whether the command loop should continue */
    private var alive = true
    
    /** Show the buffer on a specified display */
    def activate(display: Display) {
        this.display = display
        display.show(ed)
        ed.register(display)
        ed.initDisplay()
    }

    /** Ask for confirmation if the buffer is not clean */
    def checkClean(action: String) = {
        if (!ed.isModified) {
            true
        } else {
            val question = 
                "Buffer modified -- really %s?".format(action)
            MiniBuffer.ask(display, question)
        }
    }

    /** Load a file into the buffer */
    def loadFile(fname: String) { ed.loadFile(fname) }

    /** Command: Move the cursor in the specified direction */
    def moveCommand(dir: Int) {
        var p = ed.point
        val row = ed.getRow(p)

        dir match {
            case Editor.LEFT => 
                if (p > 0) p -= 1
            case Editor.RIGHT =>
                if (p < ed.length) p += 1
            case Editor.UP =>
                p = ed.getPos(row-1, goalColumn())
            case Editor.DOWN =>
                p = ed.getPos(row+1, goalColumn())
            case Editor.HOME =>
                p = ed.getPos(row, 0)
            case Editor.END =>
                p = ed.getPos(row, ed.getLineLength(row)-1)
            case Editor.CTRLHOME =>
                // Task 1 implementation
                p = 0
            case Editor.CTRLEND =>
                // Task 1 implementation
                p = ed.length
            case Editor.PAGEDOWN =>
                p = ed.getPos(row + Editor.SCROLL, 0)
                display.scroll(+Editor.SCROLL)
            case Editor.PAGEUP =>
                p = ed.getPos(row - Editor.SCROLL, 0)
                display.scroll(-Editor.SCROLL)
            case _ =>
                throw new Error("Bad direction")
        }

        ed.point = p
    }

    /** Command: Transpose two characters (Task 2) */
    def transposeCommand : Change = {
        val p = ed.point
        ed.transpose(p)
        new ed.Transposition(p)
    }

    /** Command: Convert the letters of the word at the current editing position to uppercase (Task A) */
    def toUpperCommand: Change = {
        var p = ed.point
        if (p == ed.length || !ed.charAt(p).isLetterOrDigit) {
            null
        } else {
            // Move p to the point that the word starts at.
            while (p > 0 && ed.charAt(p-1).isLetterOrDigit) p -= 1
            var length = 1
            while (p + length < ed.length && ed.charAt(p + length).isLetterOrDigit) length += 1

            val ch = ed.getRange(p, length)

            // Use setChar to avoid insertions and deletions
            for (i <- 0 until length) {
                ed.setChar(p + i, ed.charAt(p + i).toUpper)
            }

            new ed.AmalgToUpper(p, ch)
        }
    }

    /** Command: Insert a character */
    def insertCommand(ch: Char): Change = {
        val p = ed.point
        ed.insert(p, ch)
        ed.point = p+1
        new ed.AmalgInsertion(p, ch)
    }

    /** Command: Delete in a specified direction */
    def deleteCommand(dir: Int): Change = {
        var p = ed.point
        var ch : Immutable = null
        dir match {
            case Editor.LEFT =>
                if (p == 0) { beep(); return null }
                p -= 1
                ch = ed.getRange(p, 1)
                ed.deleteChar(p)
                ed.point = p
            case Editor.RIGHT =>
                if (p == ed.length) { beep(); return null }
                ch = ed.getRange(p, 1)
                ed.deleteChar(p)
            case Editor.END =>
                if (p == ed.length) { beep(); return null }
                if (ed.charAt(p) == '\n') {
                    // Delete the new-line and join the two lines
                    ch = ed.getRange(p, 1)
                    ed.deleteChar(p)
                } else {
                    // Delete to the end of the line
                    val row = ed.getRow(p)
                    val numChars = ed.getPos(row, ed.getLineLength(row)-1) - p
                    ch = ed.getRange(p, numChars)
                    ed.deleteRange(p, numChars)
                }
            case _ =>
                throw new Error("Bad direction")
        }
        new ed.Deletion(p, ch)
    }
    
    /** Command: Save the file */
    def saveFileCommand() {
        val name = 
            MiniBuffer.readString(display, "Write file", ed.filename)
        if (name != null && name.length > 0)
            ed.saveFile(name)
    }

    /** Command: Set the mark (Task 7) */
    def markCommand() : Unit = {
        ed.mark = ed.point
    }

    /** Command: Switch the mark and cursor (Task 7) */
    def switchMarkCommand() : Unit = {
        val tmp = ed.point
        ed.point = ed.mark
        ed.mark = tmp
    }

    /** Prompt for a file to read into the buffer.  */
    def replaceFileCommand() {
        if (! checkClean("overwrite")) return
        val name = 
            MiniBuffer.readString(display, "Read file", ed.filename)
        if (name != null && name.length > 0) {
            ed.point = 0
            ed.loadFile(name)
            ed.initDisplay()
            reset()
        }
    }

    /** Command: recenter and rewrite the display */
    def chooseOrigin() { 
        display.chooseOrigin() 
        ed.forceRewrite()
    }
    
    /** Quit, after asking about modified buffer */
    def quit() {
        if (checkClean("quit")) alive = false
    }


    // Command execution protocol
    
    /** Goal column for vertical motion. */
    private var goal = -1
    private var prevgoal = 0
    
    /** Execute a command, wrapping it in actions common to all commands */
    def obey(cmd: Editor.Action): Change = {
        prevgoal = goal; goal = -1
        display.setMessage(null)
        val before = ed.getState()
        val change = cmd(this)
        val after = ed.getState()
        ed.update()
        ed.wrapChange(before, change, after)
    }
    
    /** The desired column for the cursor after an UP or DOWN motion */
    private def goalColumn() = {        
        /* Successive UP and DOWN commands share the same goal column,
         * but other commands cause it to be reset to the current column */
        if (goal < 0) {
            val p = ed.point
            goal = if (prevgoal >= 0) prevgoal else ed.getColumn(p)
        }
        
        goal
    }

    /** Beep */
    def beep() { display.beep() }

    /** Read keystrokes and execute commands */
    def commandLoop() {
        activate(display)

        while (alive) {
            val key = display.getKey()
            Editor.keymap.find(key) match {
                case Some(cmd) => {
                    if(perform(cmd)) {
                        // Task 9: Increment the "time-keeping" timestamp on each editing action.
                        // Guarantees unique timestamp for each editing action.
                        ed.timestamp = ed.timestamp + 1
                        // Set the buffer timestamp to the timekeeping timestamp.
                        ed.buffer_timestamp = ed.timestamp
                    }
                }
                case None => beep()
            }
        }
    }
}

object Editor {
    /** Direction for use as argument to moveCommand or deleteCommand. */
    val LEFT =     1
    val RIGHT =    2
    val UP =       3
    val DOWN =     4
    val HOME =     5
    val END =      6
    val PAGEUP =   7
    val PAGEDOWN = 8
    val CTRLHOME = 9
    val CTRLEND =  10
    
    /** Amount to scroll the screen for PAGEUP and PAGEDOWN */
    val SCROLL = Display.HEIGHT - 3

    /** Possible value for damage. */
    val CLEAN = 0
    val REWRITE_LINE = 1
    val REWRITE = 2

    /** Main program for the entire Ewoks application. */
    def main(args: Array[String]) {
        if (args.length > 1) {
            Console.err.println("Usage: ewoks [file]")
            scala.sys.exit(2)
        }

        val terminal = new Terminal("EWOKS")
        terminal.activate()
        val app = new Editor()
        val display = new Display(terminal)
        app.activate(display)
        if (args.length > 0) app.loadFile(args(0))
        app.commandLoop()
        scala.sys.exit(0)
    }

    /** Keymap for editor commands */
    type Action = (Editor => Change)

    // This implicit conversion allows methods that return Unit to
    // be used as commands, not contributing to the undo history
    import scala.language.implicitConversions
    implicit def fixup(v: Unit): Change = null

    val keymap = Keymap[Action](
        Display.RETURN -> (_.insertCommand('\n')),
        Display.RIGHT -> (_.moveCommand(RIGHT)),
        Display.LEFT -> (_.moveCommand(LEFT)),
        Display.UP -> (_.moveCommand(UP)),
        Display.DOWN -> (_.moveCommand(DOWN)),
        Display.HOME -> (_.moveCommand(HOME)),
        Display.END -> (_.moveCommand(END)),
        Display.CTRLHOME -> (_.moveCommand(CTRLHOME)),
        Display.CTRLEND -> (_.moveCommand(CTRLEND)),
        Display.PAGEUP -> (_.moveCommand(PAGEUP)),
        Display.PAGEDOWN -> (_.moveCommand(PAGEDOWN)),
        Display.ctrl('?') -> (_.deleteCommand(LEFT)),
        Display.DEL -> (_.deleteCommand(RIGHT)),
        Display.ctrl('A') -> (_.moveCommand(HOME)),
        Display.ctrl('B') -> (_.moveCommand(LEFT)),
        Display.ctrl('D') -> (_.deleteCommand(RIGHT)),
        Display.ctrl('E') -> (_.moveCommand(END)),
        Display.ctrl('F') -> (_.moveCommand(RIGHT)),
        Display.ctrl('G') -> (_.beep),
        Display.ctrl('K') -> (_.deleteCommand(END)),
        Display.ctrl('L') -> (_.chooseOrigin),
        Display.ctrl('M') -> (_.markCommand),
        Display.ctrl('N') -> (_.moveCommand(DOWN)),
        Display.ctrl('O') -> (_.switchMarkCommand),
        Display.ctrl('P') -> (_.moveCommand(UP)),
        Display.ctrl('Q') -> (_.quit),
        Display.ctrl('R') -> (_.replaceFileCommand),
        Display.ctrl('T') -> (_.transposeCommand),
        Display.ctrl('U') -> (_.toUpperCommand),
        Display.ctrl('W') -> (_.saveFileCommand),
        Display.ctrl('Y') -> (_.redo),
        Display.ctrl('Z') -> (_.undo)
    )

    for (ch <- Display.printable)
        keymap += ch -> (_.insertCommand(ch.toChar))
}
