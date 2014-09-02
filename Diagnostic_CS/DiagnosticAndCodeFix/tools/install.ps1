param($installPath, $toolsPath, $package, $project)

$analyzerPath = join-path $toolsPath "analyzers"
$analyzerFilePath = join-path $analyzerPath "String Format Diagnostic (CS).dll"

$project.Object.AnalyzerReferences.Add("$analyzerFilePath")