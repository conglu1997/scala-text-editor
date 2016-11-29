// EdBuffer.scala
// Copyright (c) 2015 J. M. Spivey

import java.io.{Reader, Writer, FileReader, FileWriter, IOException}
import Undoable.Change

/** The state of an editing session */
class EdBuffer {
    /** The text being edited. */
    private val text = new PlaneText()

    /** The display. */
    private var display: Display = null
    
    // State components that are preserver by undo and redo

    /** Current editing position. */
    private var _point = 0

    /** Current marked position. */
    private var _mark = 0

    // State components that are not restored on undo

    /** File name for saving the text. */
    private var _filename = ""

    /** Dirty flag */
    private var modified = false


    /** Register a display */
    def register(display: Display) { this.display = display }

    /** Mark the buffer as modified */
    private def setModified() { modified = true }

    /** Test whether the text is modified */
    def isModified = modified
    

    // Display update
    
    /** Extent that the display is out of date. */
    private var damage = EdBuffer.CLEAN
    
    /** If damage = REWRITE_LINE, the line that should be rewritten */
    private var damage_line = 0

    /** Note damage to the display. */
    private def noteDamage(rewrite: Boolean) {
        val newdamage = 
            if (rewrite) EdBuffer.REWRITE else EdBuffer.REWRITE_LINE
        damage = Math.max(damage, newdamage)
        damage_line = text.getRow(point)
    }
    
    /** Force a display rewrite */
    def forceRewrite() { noteDamage(true) }

    /** Update display with cursor at point */
    def update() { update(point) }

    /** Update display with cursor at arbitrary position */
    def update(pos: Int) {
        display.refresh(damage, text.getRow(pos), text.getColumn(pos))
        damage = EdBuffer.CLEAN
    }
    
    /** Initialise display */
    def initDisplay() {
        noteDamage(true)
        update()
    }


    // Accessors

    def point = _point

    def point_=(point: Int) {
        if (damage == EdBuffer.REWRITE_LINE && getRow(point) != damage_line)
            damage = EdBuffer.REWRITE
        _point = point
    }

    // Task 7
    def mark = {
        if (0 <= _mark && _mark <= length){
            // Always give a well-defined value for mark
            _mark
        } else {
            _point
        }
    }

    def mark_=(mark: Int) {
        _mark = mark
    }

    def filename = _filename

    private def filename_=(filename: String) { _filename = filename }


    // Delegate methods for text
    
    def charAt(pos: Int) = text.charAt(pos)

    def getRow(pos: Int) = text.getRow(pos)

    def getColumn(pos: Int) = text.getColumn(pos)
    
    def getPos(row: Int, col: Int) = text.getPos(row, col)

    def length = text.length

    def getLineLength(row: Int) = text.getLineLength(row)

    def getRange(pos: Int, len: Int) = text.getRange(pos, len)

    def numLines = text.numLines

    def fetchLine(n: Int, buf: Text) { text.fetchLine(n, buf) }

    def writeFile(out: Writer) { text.writeFile(out) }


    // Mutator methods

    /** Transpose two characters */
    def transpose(pos: Int) {
        noteDamage(false)
        val row = getRow(pos)
        if (pos == getPos(row, 0)){
            // If the pointer is at the start/end of a line - transpose the inner two characters
            if (getLineLength(row) > 2) transpose(pos + 1)
        } else if (pos == getPos(row, getLineLength(row)-1)) {
            if (getLineLength(row) > 2) transpose(pos - 1)
        } else {
            // Switch the two characters
            val ch = charAt(pos-1)
            setChar(pos-1, charAt(pos))
            setChar(pos, ch)
            // Shift the point up by one
            if (pos < length) point = pos + 1
        }
        setModified()
    }

    /** Set a character */
    def setChar(pos: Int, ch: Char) {
        noteDamage(false)
        text.set(pos, ch)
        setModified()
    }

    /** Delete a character */
    def deleteChar(pos: Int) {
        val ch = text.charAt(pos)
        noteDamage(ch == '\n' || getRow(pos) != getRow(point))
        // Shift the mark
        if (pos <= mark) mark -= 1
        text.deleteChar(pos)
        setModified()
    }

    /** Delete a range of characters. */
    def deleteRange(pos: Int, len: Int) {
        noteDamage(true)
        // Shift the mark - note that we may delete the mark in the process
        if (pos <= mark) {
            mark -= len
        }
        text.deleteRange(pos, len)
        setModified()
    }
    
    /** Insert a character */
    def insert(pos: Int, ch: Char) {
        noteDamage(ch == '\n' || getRow(pos) != getRow(point))
        // Shift the mark
        if (pos <= mark) mark += 1
        text.insert(pos, ch)
        setModified()
    }
    
    /** Insert a string */
    def insert(pos: Int, s: String) {
        noteDamage(true)
        // Shift the mark
        if (pos <= mark) mark += s.length
        text.insert(pos, s)
        setModified()
    }
    
    /** Insert an immutable text. */
    def insert(pos: Int, s: Text.Immutable) {
        noteDamage(true)
        // Shift the mark
        if (pos <= mark) mark += s.length
        text.insert(pos, s)
        setModified()
    }
    
    /** Insert a Text. */
    def insert(pos: Int, t: Text) {
        noteDamage(true)
        // Shift the mark
        if (pos <= mark) mark += t.length
        text.insert(pos, t)
        setModified()
    }

    /** Load a file into the buffer. */
    def loadFile(name: String) {
        filename = name
        text.clear()
        
        try {
            val in = new FileReader(name)
            text.insertFile(0, in)
            in.close()
        } catch {
            case e: IOException =>
                MiniBuffer.message(display, "Couldn't read file '%s'", name)
        }
        
        modified = false
        // Reset the mark
        mark = 0
        noteDamage(true)
    }
    
    /** Save contents on a file */
    def saveFile(name: String) {
        filename = name
    
        try {
            val out = new FileWriter(name)
            text.writeFile(out)
            out.close()
            modified = false
        } catch {
            case e: IOException =>
                MiniBuffer.message(display, "Couldn't write '%s'", name)
        }
    }


    /** Make a Memento that records the current editing state */
    def getState() = new Memento()
    
    /** An immutable record of the editor state at some time.  The state that
     * is recorded consists of just the current point. */
    class Memento {
        private val pt = point
        // Store the mark information as well (Task 7)
        private val mk = mark
        
        /** Restore the state when the memento was created */
        def restore() { point = pt ; mark = mk }
    }

    /** Change that records an insertion */
    class Insertion(pos: Int, text: Text.Immutable) extends Change {
        def undo() { deleteRange(pos, text.length) }
        def redo() { insert(pos, text) }
    }

    /** Change that records a transposition (Task 2) */
    class Transposition(pos: Int) extends Change {
        def undo() { transpose(pos) }
        def redo() { transpose(pos) }
    }

    /** Insertion that can be amalgamated with adjacent, similar changes */
    class AmalgInsertion(val pos: Int, ch: Char) extends Change {
        /** The text inserted by all commands that have merged with this one */
        private val text = new Text(ch)

        def undo() { deleteRange(pos, text.length) }

        def redo() { insert(pos, text) }

        override def amalgamate(change: Change) = {
            change match {
                case other: AmalgInsertion =>
                    if (text.charAt(text.length-1) == '\n'
                            || other.pos != this.pos + this.text.length) 
                        false
                    else {
                        text.insert(text.length, other.text)
                        true
                    }

                case _ => false
            }
        }
    }

    /** Generalised change that records a deletion (Task 3) */
    class Deletion(pos: Int, deleted: Text.Immutable) extends Change {
        def undo() { insert(pos, deleted) }
        def redo() { deleteRange(pos, deleted.length) }
    }

    def wrapChange(before: Memento, change: Change, after: Memento) = {
        if (change == null)
            null
        else
            new EditorChange(before, change, after)
    }

    /** Wrapper for text changes that preserves other state */
    class EditorChange(before: Memento, 
            private val change: Change,
            private var after: Memento) extends Change {

        def undo() {
            change.undo(); before.restore()
        }
            
        def redo() {
            change.redo(); after.restore()
        }
        
        def amalgamate(other: EditorChange) = {
            if (! change.amalgamate(other.change))
                false
            else {
                after = other.after
                true
            }
        }

        override def amalgamate(other: Change) =
            amalgamate(other.asInstanceOf[EditorChange])
    }
}

object EdBuffer {
    /** Possible value for damage. */
    val CLEAN = 0
    val REWRITE_LINE = 1
    val REWRITE = 2
}
