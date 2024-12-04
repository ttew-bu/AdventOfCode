class SolutionDay02 {

    def readRows(path : String): List[Array[String]] = {
        var fileContentGenerator = scala.io.Source.fromFile(path).getLines
        var parsedRecords = fileContentGenerator.map(record => record.split(' ')).toList
        parsedRecords

    }

    def checkTrend(inputList: List[Int]) : Int = {
        var nIncreasing = inputList.map(num => num.toInt).filter(_>0).length.toInt
        var nDecreasing = inputList.map(num => num.toInt).filter(_<0).length.toInt

        if ( nIncreasing == inputList.length.toInt ){
            1
        } else if ( nDecreasing == inputList.length.toInt ){
            1
        }
        else{
            0
        }
    }

    def checkAbsolute(inputList: List[Int]) : Int = {
        var nValid = inputList.map(num => num.toInt.abs).filter(_>0).filter(_<4).length.toInt
        if ( nValid  == inputList.length.toInt ){
            1
        }
        else{
            0
        }
    }

    class Part01 extends SolutionDay02:
        def diffPairs(singleRecord: List[Array[String]]) : List[Int] = {
    
            var recordOutput = singleRecord.map(pair => pair(1).toInt-pair(0).toInt).toList
            recordOutput

        }
        def getSolution(path : String): Int = {

            var parsedRecords = readRows(path)
            var recordWindows = parsedRecords.map(record => record.sliding(2).toList).toList
            var recordValues = recordWindows.map(record => diffPairs(record)).toList

            var absoluteChecks = recordValues.map(record => checkAbsolute(record))
            var trendChecks = recordValues.map(record => checkTrend(record))

            var result = absoluteChecks.zip(trendChecks).map(record => List(record(0),record(1)).min).sum
            result
            }

    class Part02 extends SolutionDay02:
        def diffPairs(singleRecord: List[List[Int]]) : List[Int] = {
    
            var recordOutput = singleRecord.map(pair => pair(0)-pair(1)).toList
            recordOutput

        }
        def combinationsWithDuplicates(inputList : Array[String]) : List[List[Int]] = {
            var combinationList = inputList.toList.zipWithIndex.map{ case (e, i) => inputList.patch(i, Nil, 1).map(item => item.toInt).toList}
            combinationList
        }

        def processCombinationList(combinationListRecord : List[List[Int]]) :  List[List[Int]] = {
                  var combinationProcessedRecord = combinationListRecord.map(record => record.sliding(2).toList).map(record => diffPairs(record)).toList
                  combinationProcessedRecord
              }

        def generatePermutations(inputList:Array[String]): List[List[Int]]={

            //Note that we need to convert to seq as list -> combinations does not conserve order and our problem is one where order matters
            var permutationList = combinationsWithDuplicates(inputList).toList
            permutationList
        }

        def getSolution(path : String): Int = {
            var parsedRecords = readRows(path)
            var permutationList = parsedRecords.map(record => generatePermutations(record)).toList
            var processedPermutationList = permutationList.map(record => processCombinationList(record))

            var varAbs = processedPermutationList.map(item => item.map(subrecord => checkAbsolute(subrecord)))
            var varTrends = processedPermutationList.map(item => item.map(subrecord => checkTrend(subrecord)))
            var result = varAbs.zipWithIndex.map{ case (e, i) => e.zip(varTrends(i))}.map(record => record.map(subrecord => 
                List(subrecord(0),subrecord(1)).min)
                ).map(record => record.max).sum
            result
    }}