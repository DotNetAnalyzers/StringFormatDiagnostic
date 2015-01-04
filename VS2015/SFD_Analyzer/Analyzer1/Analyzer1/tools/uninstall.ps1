param($installPath, $toolsPath, $package, $project)

$analyzerPath = join-path $toolsPath "analyzers"
$analyzerFilePath = join-path $analyzerPath "Analyzer1.dll"

$project.Object.AnalyzerReferences.Remove("$analyzerFilePath")