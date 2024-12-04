class SolutionDay01{
    def inputToPairs(path : String) : List[Array[String]]  = {
            var fileContentGenerator = scala.io.Source.fromFile(path).getLines
            var fileContentPairs = 
                fileContentGenerator.map(row => {row.split(" ").filter(_ != "")}).toList

            // here we return a list which contains our pairs of strings
            fileContentPairs
        }
}

class Part01 extends SolutionDay01{

    
    def processPairs(pairs : List[Array[String]]) : Int = {
        var leftList = pairs.map(location_str => {location_str(0).toInt}).sorted()
        var rightList = pairs.map(location_str => {location_str(1).toInt}).sorted()

        val diffValues = (leftList,rightList).zipped.map(_ - _).map(value => value.abs).sum()
        diffValues
    }

    def getSolution(path : String) : Unit = {
        val pairs = inputToPairs(path)
        val solutionVal = processPairs(pairs)
        println(solutionVal)
    }
}

class Part02 extends SolutionDay01{

    def processPairs(pairs : List[Array[String]]) : Int = {
        var leftList = pairs.map(location_str => {location_str(0).toInt}).sorted()
        var rightList = pairs.map(location_str => {location_str(1).toInt}).sorted()
        val relevantValues = leftList intersect rightList

        var relevantValueCounts = rightList.filter(relevantValues.contains(_)).groupBy(identity).mapValues(_.size).toList
        var processedPairVal = relevantValueCounts.map{ (listValue, listValueFrequency) => listValue * listValueFrequency }.sum
        processedPairVal
    }

    def getSolution(path : String) : Unit = {
        val pairs = inputToPairs(path)
        val solutionVal = processPairs(pairs)
        println(solutionVal)
    }
}