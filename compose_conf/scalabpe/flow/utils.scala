package scalabpe.flow

import java.text.SimpleDateFormat
import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter

import scalabpe.core._
import java.util.{Date, Random}

import scala.math.BigDecimal

object Global {
    def init() {
        println("init called")
    }
    def close() {
        println("close called")
    }
}

object FlowHelper { 

    val random = new Random()
	
    val jobStatusCache = new java.util.concurrent.ConcurrentHashMap[String,String]()

    def getConfig(s:String,defaultValue:String="") = Flow.router.getConfig(s,defaultValue)

    def isEmpty(req:Request,name:String):Boolean={
        if( name.indexOf(",") >= 0 ) return isEmptyForAny(req,name)
        return isEmpty(req.s(name))
    }

    private def isEmptyForAny(req:Request,names:String):Boolean={
        val ss = names.split(",")
        var i = 0
        while( i < ss.length ) {
            if( isEmpty(req.s(ss(i)) )) return true
            i += 1
        }
        false
    }

    def parseDate(obj:Any,defaultValue:Date = new Date(),format: String = "yyyy-MM-dd HH:mm:ss"):Date ={
        if (obj == null){
            defaultValue
        } else {
            obj match {
                case d: Date =>
                    obj.asInstanceOf[Date]
                case _ =>
                    val sdf: SimpleDateFormat = new SimpleDateFormat(format)
                    try {
                        sdf.parse(obj.asInstanceOf[String])
                    } catch {
                        case e: Exception => defaultValue
                    }
            }
        }
    }

    def parseLocalDateTime(obj:Any,defaultValue:LocalDateTime =LocalDateTime.now(),format: String = "yyyy-MM-dd HH:mm:ss"): LocalDateTime ={
        if (obj == null){
            defaultValue
        }else {
            obj match {
                case ldt : LocalDateTime =>
                    obj.asInstanceOf[LocalDateTime]
                case ld : LocalDate =>
                    obj.asInstanceOf[LocalDate].atTime(defaultValue.toLocalTime)
                case _ =>
                    val  formatter = DateTimeFormatter.ofPattern(format)
                    try{
                        LocalDateTime.parse(obj.asInstanceOf[String],formatter)
                    } catch {
                        case e: Exception => defaultValue
                    }
            }
        }
    }

    def parseInt(obj: Any, defaultValue: Int = 0): Int = {
        if (obj == null) {
            defaultValue
        } else {
            obj match {
                case i: Int =>
                    obj.asInstanceOf[Int]
                case _ =>
                    try {
                        obj.toString.toInt
                    } catch {
                        case e: NumberFormatException => defaultValue
                    }
            }
        }
    }

    def parseDouble(obj: Any, defaultValue: Double = 0): Double = {
        if (obj == null) {
            defaultValue
        } else {
            obj match {
                case d: Double =>
                    obj.asInstanceOf[Double]
                case _ =>
                    try {
                        obj.toString.toDouble
                    } catch {
                        case e: NumberFormatException => defaultValue
                    }
            }
        }
    }

    def parseBigDecimal(obj: Any, defaultValue: BigDecimal = BigDecimal.apply(0)): BigDecimal = {
        if (obj == null) {
            defaultValue
        } else {
            obj match {
                case bd: BigDecimal =>
                    obj.asInstanceOf[BigDecimal]
                case _ =>
                    try {
                        BigDecimal.apply(obj.toString)
                    } catch {
                        case e: NumberFormatException => defaultValue
                    }
            }
        }
    }

    def isInt(req:Request,name:String):Boolean={
        if( name.indexOf(",") >= 0 ) return isIntForAny(req,name)
        return isInt(req.s(name))
    }    
	
    private def isIntForAny(req:Request,names:String):Boolean={
        val ss = names.split(",")
        var i = 0
        while( i < ss.length ) {
            if( isInt(req.s(ss(i)) )) return true
            i += 1
        }
        false
    }

    def isEmpty(str:String):Boolean={
        return str == null || str.length() == 0
    }

    def isInt(n:String):Boolean={
        try {
            Integer.parseInt(n)
            return true
        } catch {
            case e: Throwable =>
             return false
        }
    }

    def checkInclude(ss:String,s:String,t:String=","):Boolean={
        if( ss == null || ss == "" ) return false
        if( s == null || s == "" ) return true
        return (t+ss+t).indexOf(t+s+t) >= 0 
    }

    def uuid(): String = {
        return java.util.UUID.randomUUID().toString().replaceAll("-", "")
    }

    def generateSeed():String = {
        "%08d".format(Math.abs(random.nextInt())%100000000)
    }
 
    def contact(a:String,b:String):String = a + b
}



