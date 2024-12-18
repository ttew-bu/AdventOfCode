import scala.collection.MapView

class SolutionDay05 {
    var path = "inputs/day_five.txt"
    var textBlock = scala.io.Source.fromFile(path).mkString
    var textLines = textBlock.split("\n").toArray
    var ruleStrings = textLines.filter(_.contains("|"))
    var sequenceStrings = textLines.filter(_.contains(","))
    var ruleMap = ruleStrings.map(rule => rule.split("""\|""")).map(rule => (rule(0),(rule(1)))).groupBy(_._2).mapValues(_.map(_._1))

}

class Part01 extends SolutionDay05{

def checkLogicFailures(fullSequence : Array[String], ruleMap : MapView[String,Array[String]]) : Int = {
    //if valid, return middle value

    var counter = 0

    var seqLength = fullSequence.length

    for (value <- 0 to seqLength-1)
        var bannedFollowingValues = ruleMap.getOrElse(fullSequence(value),Array[String]())
        var sliceFailures = fullSequence.slice(value, seqLength).toSet.intersect(bannedFollowingValues.toSet).toList.length
        if (sliceFailures>0){
            counter += 1
        }

    if (counter==0){
        var middleIndex = (seqLength - 1)/2
        fullSequence(middleIndex).toInt
    }

    else{
        0
    }

}

def getSolution() : Int = {

    sequenceStrings.map(sequence => checkLogicFailures(sequence.split(","), ruleMap)).sum

}

}


class Part02 extends SolutionDay05{
    def correctLogicFailures(fullSequence : Array[String], ruleMap : MapView[String,Array[String]]) : Int = {

        var positionCursor = 0
        var fixesApplied = 0

        var fullSequenceCopy = fullSequence

        var seqLength = fullSequence.length

        while (positionCursor < seqLength){

            var bannedFollowingValues = ruleMap.getOrElse(fullSequenceCopy(positionCursor),Array[String]())
            var sliceFailures = fullSequenceCopy.slice(positionCursor, seqLength).toSet.intersect(bannedFollowingValues.toSet).toArray

            if (sliceFailures.length==0){
                positionCursor +=1
            }

            else{

                //Remove all failures, move them to the front
                fullSequenceCopy = fullSequenceCopy.filter(!sliceFailures.contains(_))
                fullSequenceCopy = sliceFailures ++ fullSequenceCopy
                fixesApplied +=1
                //reset cursor if we make a change
                positionCursor = 0
            }
        }

        //only count things we had to edit
        if ( fixesApplied >0 ){
        
            var middleIndex = (seqLength - 1)/2

            fullSequenceCopy(middleIndex).toInt
            
        }
        
        else{
            0
        }
    }
    
    def getSolution() : Int = {

    sequenceStrings.map(sequence => correctLogicFailures(sequence.split(","), ruleMap)).sum

}
}

    
    
    