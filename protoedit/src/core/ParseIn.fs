module ParseIn

let readBinaryFile (filePath:string) = System.IO.File.ReadAllBytes(filePath)

let readFileLines (filePath:string) = System.IO.File.ReadLines(filePath)