class SolutionDay03 {

    val mulRegexPattern = """(mul\()(\d[\d,.]*)(?=\))""".r

    def cleanRecord(string : String) : String ={
            var cleanedVal = string.replace("mul(","").replace(")","")
            cleanedVal
        }

        def multiplyStringPair(pair: String) : Int = {
            var outputValue = pair.split(",")(0).toInt*pair.split(",")(1).toInt
            outputValue
        }

    def cleanAndMultiply(validStringPortions : List[List[String]]) : Int = {
        validStringPortions.map(record => record.map(subrecord => cleanRecord(subrecord)
           
      
                   )).flatten.map(pair => multiplyStringPair(pair)).sum
    }

    class Part01 extends SolutionDay03:

        def getSolution(path : String) : Int ={
            var fileContentGenerator = scala.io.Source.fromFile(path).mkString
            var validStringPortions = fileContentGenerator.map(line => mulRegexPattern.findAllIn(line).toList)

            cleanAndMultiply(validStringPortions)
        }
        
    class Part02 extends SolutionDay03:
        def getSolution(path: String) : List[String] = {
            var switchRegexPattern = """(do\(|^)(.*?)(?=don\'t\(|$)""".r

            //add do() as instructions state the beginning of the file has multiplication enabled
            //replace newlines with empty strings so our matching does not break on it
            var fileContentString = scala.io.Source.fromFile(path).mkString.replace("\n","")
            var validDoSubstrings = switchRegexPattern.findAllIn(fileContentString).toList
            var validStringPortions = validDoSubstrings.map(line => mulRegexPattern.findAllIn(line).toList)

            cleanAndMultiply(validStringPortions)
            
        }
    }
