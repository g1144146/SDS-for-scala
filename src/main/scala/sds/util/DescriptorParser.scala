package sds.util

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object DescriptorParser {
    private val obj: String = """L[a-z\.]*[0-9a-zA-Z_\$\.]+"""
    private val lang: String = "java.lang."
    private val langPackage: Array[String] = Array(
        "java.lang.annotation", "java.lang.instrument", "java.lang.invoke",
        "java.lang.management", "java.lang.reflect"   , "java.lang.ref"
    )

    def parse(desc: String): String = parse(desc, false)

    def parse(desc: String, hasAttribute: Boolean): String = {
        val objPattern: String = "(" + obj + """|\[+""" + obj + ")"
        val prmPattern: String = """(B|\[+B|C|\[+C|D|\[+D|F|\[+F|V|I|\[+I|J|\[+J|S|\[+S|Z|\[+Z)"""
        val parenthesis: String = """(\(|\))"""
        val gen: String = "T[A-Z]"
        val genPattern: String = "(" + gen + """|\[+""" + gen + ")"
        val colon: String = "(;:|::|:)"
        val wildCard: String = """(\+|\*)"""
        val diamond: String = "(<|>)"
        val pattern = Array(objPattern, prmPattern, parenthesis, genPattern,
                            colon, wildCard, diamond, "([A-Z])", "(;)")
        val replaced: String = desc.replace("/", ".").replace(";>", ">").replace(";)", ")")
        val regex: Regex = pattern.mkString("|").r()
        var beforeEndParen = true

        lazy val getMatched: (Match => String) = (m: Match) => {
            val matched: String = m.matched
            val len: Int  = matched.length
            if(matched.startsWith("[")) {
                val last: Int = matched.lastIndexOf("[") + 1
                var _type: String = ""
                if(matched.matches(prmPattern)) {
                    _type = parsePrim(matched.substring(len - 1))
                    _type = _type.substring(0, _type.length - 1)
                } else {
                    _type = matched.substring(last + 1, len)
                    _type = removeLangPrefix(_type)
                }
                _type += (0 until last).map((_: Int) => "[]").toArray.mkString
                _type
            } else if(matched.startsWith("L") || matched.matches("T[A-Z]+")) {
                removeLangPrefix(matched.substring(1, len))
            } else if(matched.matches("""\(|\)|<|>""")) {
                beforeEndParen = if(matched.equals(")")) false else true
                matched
            } else if(matched.equals(";:")) {
                " & "
            } else if(matched.matches("::|:")) {
                " extends "
            } else if(matched.equals("*")) {
                " ? "
            } else if(matched.equals("+")) {
                "? extends "
            } else if(matched.equals(";")) {
                ","
            } else if(hasAttribute && matched.matches("[A-Z]") && beforeEndParen) {
                matched
            } else if(parsePrim(matched).length > 0) {
                parsePrim(matched)
            } else {
                ""
            }
        }
        var parsed: String = regex.findAllMatchIn(replaced).map(getMatched(_)).toArray.mkString
        Array(parsed)
            .map((s: String) => if(s.endsWith(","))  s.substring(0, s.length - 1) else s)
            .map((s: String) => if(s.contains(",)")) s.replace(",)", ")")         else s)
            .head
    }

    def removeLangPrefix(target: String): String = {
        if(! target.startsWith(lang) || langPackage.exists(target.startsWith(_))) {
            return target
        }
        target.replace(lang, "")
    }

    private def parsePrim(target: String): String = target match {
        case "B" => "byte,"
        case "C" => "char,"
        case "D" => "double,"
        case "F" => "float,"
        case "I" => "int,"
        case "J" => "long,"
        case "S" => "short,"
        case "Z" => "boolean,"
        case "V" => "void"
        case _   => ""
    }
}