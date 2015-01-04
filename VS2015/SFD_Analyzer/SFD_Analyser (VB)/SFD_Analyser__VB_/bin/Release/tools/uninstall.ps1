param($installPath, $toolsPath, $package, $project)

$analyzerPath = join-path $toolsPath "analyzers"
$analyzerFilePath = join-path $analyzerPath "SFD_Analyser__VB_.dll"

$project.Object.AnalyzerReferences.Remove("$analyzerFilePath")