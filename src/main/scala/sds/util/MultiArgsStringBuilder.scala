package sds.util

import java.lang.StringBuilder

class MultiArgsStringBuilder {
	val build: StringBuilder = new StringBuilder();
	
	def this(init: String) {
		this()
		build.append(init)
	}

	def append(elements: Any*): Unit = elements.foreach(build.append(_))
}