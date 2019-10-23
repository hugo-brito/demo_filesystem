package com.rtjvm.scala.oop.files

import com.rtjvm.scala.oop.filesystem.FilesystemException

import scala.annotation.tailrec

class Directory(override val parentPath: String, override val name: String, val contents: List[DirEntry]) extends DirEntry(parentPath, name) {

	def hasEntry(name: String): Boolean = findEntry(name) != null

	def getAllFoldersInPath: List[String] =
		path.substring(1).split(Directory.SEPARATOR).toList.filter(e => !e.isEmpty)
		//						^^^
		//  starts from index 1, eliminating the first "/"
		// /a/b/c/d => List["a","b","c","d"]

	def findDescendent(path: List[String]): Directory = {
		if (path.isEmpty) this
		else findEntry(path.head).asDirectory.findDescendent(path.tail)
	}

	def addEntry(newEntry: DirEntry): Directory = {
		new Directory(parentPath, name, contents :+ newEntry)
	}

	def findEntry(entryName: String): DirEntry = {
		@tailrec
		def findEntryHelper(name: String, contentList: List[DirEntry]): DirEntry = {
			if (contentList.isEmpty) null
			else if (contentList.head.name.equals(name)) contentList.head
			else findEntryHelper(name, contentList.tail)
		}

		findEntryHelper(entryName, contents)
	}

	def replaceEntry(entryName: String, newEntry: DirEntry): Directory = {
		new Directory(parentPath, name, contents.filter(e => !e.name.equals(entryName)) :+ newEntry)
	}

	def isRoot: Boolean = parentPath.isEmpty
	def asDirectory: Directory = this
	def asFile: File = throw new FilesystemException("Directory cannot be converted to File!")
	def getType: String = "Directory"
	def isDirectory: Boolean = true
	def isFile: Boolean = false

}

object Directory {
	val SEPARATOR = "/"
	val ROOT_PATH = "/"

	def ROOT: Directory = Directory.empty("","")

	def empty(parentPath: String, name: String): Directory = new Directory(parentPath, name, List())
}