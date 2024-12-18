import scala.collection.mutable.ListBuffer

class SolutionDay04 {
    def rotateBuffer(contentRows: Array[String]) : Array[String]={
    Array(contentRows.map(row => row.split("").toArray).transpose.toArray).flatten.map(row => row.mkString(""))
}

    def countStringHits(targetContent: String, targetString : String) : Int = {
        targetContent.split("\n").map(row => targetString.r.findAllIn(row).toArray).flatten.length
    }

    def countDiagonalHitsLeft(targetRows : Array[String], targetString: String, targetOffset : Int = 0) : ListBuffer[List[Int]] = {

        var hits = ListBuffer[List[Int]]()

        var rowLength = targetRows.length -1 
        for (i <- 0 to rowLength)
            for (j <- 0 to rowLength)
                var tempString = ""
                for (k <- 0 to targetString.length-1)
                    if (i+k <= rowLength & j+k <=rowLength){
                        tempString += targetRows(i+k)(j+k)
                    }
                    else {
                        tempString += " "
                    }
                    
                if ( tempString == targetString ){
                    // the center for this is 1 to the right, down 1
                    hits += List(i+targetOffset,j+targetOffset)
                }
        hits
    }

    def countDiagonalHitsRight(targetRows : Array[String], targetString: String,targetOffset : Int = 0) : ListBuffer[List[Int]] = {

        var hits = ListBuffer[List[Int]]()

        var rowLength = targetRows.length -1 
        for (i <- 0 to rowLength)
            for (j <- rowLength to 0 by -1)
                var tempString = ""
                for (k <- 0 to targetString.length-1)
                    if (i+k <=rowLength & j-k >=0){
                        tempString += targetRows(i+k)(j-k)
                    }
                    else {
                        tempString += " "
                    }
                    
                if ( tempString == targetString ){
                    //the center for this would be 1 i value less AND 1 j value less
                    hits += List(i+targetOffset,j-targetOffset)
                }
        hits
    }
}

class Part01 extends SolutionDay04{

    var targetString = "XMAS"
    var targetStringRev = targetString.reverse

    def getSolution(path : String) : Int ={
        var textBlock = scala.io.Source.fromFile(path).mkString
        var textLines = textBlock.split("\n").toArray
        var textLinesRot = rotateBuffer(textLines)
        var textBlockRot = textLinesRot.mkString("\n")

        // search horiz for both XMAS variants
        var horizMatches = countStringHits(textBlock,targetString)
        var horizMatchesRev = countStringHits(textBlock,targetStringRev)

        // search vert for both XMAS variants
        var vertMatches = countStringHits(textBlockRot,targetString)
        var vertMatchesRev = countStringHits(textBlockRot,targetStringRev)

        //now do diagonal operations

        var topRightMatches = countDiagonalHitsRight(textLines,targetString).length
        var topRightMatchesRev = countDiagonalHitsRight(textLines,targetStringRev).length

        var topLeftMatches = countDiagonalHitsLeft(textLinesRot,targetString).length
        var topLeftMatchesRev = countDiagonalHitsLeft(textLinesRot,targetStringRev).length

        var diagonalHitCounts = Array(topRightMatches,topRightMatchesRev,topLeftMatches,topLeftMatchesRev).sum
        var standardHitCounts = Array(horizMatches,horizMatchesRev,vertMatches,vertMatchesRev).sum

        standardHitCounts + diagonalHitCounts
    }
}

class Part02 extends SolutionDay04 {

    var targetString = "MAS"
    var targetStringRev = targetString.reverse

    def getSolution(path : String) : Int ={

        var textBlock = scala.io.Source.fromFile(path).mkString
        var textLines = textBlock.split("\n").toArray
        var textLinesRot = rotateBuffer(textLines)

        var topRightMatches = countDiagonalHitsRight(textLines,targetString,1)
        var topRightMatchesRev = countDiagonalHitsRight(textLines,targetStringRev,1)

        var topLeftMatches = countDiagonalHitsLeft(textLines,targetString,1)
        var topLeftMatchesRev = countDiagonalHitsLeft(textLines,targetStringRev,1)

        var leftMatches = (topLeftMatches ++ topLeftMatchesRev).distinct.map(record => record.mkString("."))
        var rightMatches = (topRightMatches ++ topRightMatchesRev).distinct.map(record => record.mkString("."))

        var setMatch = leftMatches.toSet.intersect(rightMatches.toSet).toList.length
        setMatch
    }
}