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

    /** Task 9 - Timestamp for editing buffer that is incremented on each editing action.
      * This is used to ensure each editing action has a unique timestamp. */
    private var _timestamp = 0

    /** Timestamp for the current file and display buffer contents **/
    private var _file_timestamp = 0
    private var _buffer_timestamp = 0

    // State components that are not restored on undo

    /** File name for saving the text. */
    private var _filename = ""

    /** Register a display */
    def register(display: Display) { this.display = display }

    /** Test whether the text is modified - (Task 9) regard the text as modified if the buffer differs from the file.*/
    def isModified = buffer_timestamp != file_timestamp
    

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

    def filename = _filename

    private def filename_=(filename: String) { _filename = filename }

    // Task 7 Accessors
    def mark = {
        // Always give a well-defined value for mark
        if (_mark < 0 || _mark > length){ _mark = 0 }
        _mark
    }

    def mark_=(mark: Int) {
        _mark = mark
    }

    // Task 9 Accessors
    def buffer_timestamp = _buffer_timestamp

    def buffer_timestamp_=(buffer_timestamp: Int) { _buffer_timestamp = buffer_timestamp }

    def file_timestamp = _file_timestamp

    private def file_timestamp_=(file_timestamp: Int) { _file_timestamp = file_timestamp }

    def timestamp = _timestamp

    def timestamp_=(timestamp: Int) { _timestamp = timestamp }


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
    }

    /** Set a character */
    def setChar(pos: Int, ch: Char) {
        noteDamage(false)
        text.set(pos, ch)
    }

    /** Delete a character */
    def deleteChar(pos: Int) {
        val ch = text.charAt(pos)
        noteDamage(ch == '\n' || getRow(pos) != getRow(point))
        // Shift the mark
        if (pos <= mark) mark -= 1
        text.deleteChar(pos)
    }

    /** Delete a range of characters. */
    def deleteRange(pos: Int, len: Int) {
        noteDamage(true)
        // Shift the mark - note that we may delete the mark in the process
        if (pos <= mark) {
            mark -= len
        }
        text.deleteRange(pos, len)
    }
    
    /** Insert a character */
    def insert(pos: Int, ch: Char) {
        noteDamage(ch == '\n' || getRow(pos) != getRow(point))
        // Shift the mark
        if (pos <= mark) mark += 1
        text.insert(pos, ch)
    }
    
    /** Insert a string */
    def insert(pos: Int, s: String) {
        noteDamage(true)
        // Shift the mark
        if (pos <= mark) mark += s.length
        text.insert(pos, s)
    }
    
    /** Insert an immutable text. */
    def insert(pos: Int, s: Text.Immutable) {
        noteDamage(true)
        // Shift the mark
        if (pos <= mark) mark += s.length
        text.insert(pos, s)
    }
    
    /** Insert a Text. */
    def insert(pos: Int, t: Text) {
        noteDamage(true)
        // Shift the mark
        if (pos <= mark) mark += t.length
        text.insert(pos, t)
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

        // Reset the mark (Task 7)
        mark = 0

        // Reset the file and buffer timestamp (Task 9)
        file_timestamp = 0
        buffer_timestamp = 0

        noteDamage(true)
    }
    
    /** Save contents on a file */
    def saveFile(name: String) {
        filename = name
    
        try {
            val out = new FileWriter(name)
            text.writeFile(out)
            out.close()
        } catch {
            case e: IOException =>
                MiniBuffer.message(display, "Couldn't write '%s'", name)
        }

        // Task 9 - Set the file timestamp to be the same as the buffer timestamp.
        file_timestamp = buffer_timestamp
    }


    /** Make a Memento that records the current editing state */
    def getState() = new Memento()
    
    /** An immutable record of the editor state at some time.  The state that
     * is recorded consists of just the current point. */
    class Memento {
        private val pt = point
        // Store the mark information as well (Task 7)
        private val mk = mark
        // Store the current buffer timestamp (Task 9)
        // The buffer timestamp is part of the editing state whilst the file timestamp is only modified when a file
        // is saved or loaded and the principal timestamp is a time-keeping device.
        private val bf_ts = buffer_timestamp
        
        /** Restore the state when the memento was created */
        def restore() { point = pt ; mark = mk; buffer_timestamp = bf_ts }
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
