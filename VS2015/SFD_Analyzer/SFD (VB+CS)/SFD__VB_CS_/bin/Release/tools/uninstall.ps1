param($installPath, $toolsPath, $package, $project)

$analyzerPath = join-path $toolsPath "analyzers"
$analyzerFilePath = join-path $analyzerPath "SFD__VB_CS_.dll"

$project.Object.AnalyzerReferences.Remove("$analyzerFilePath")