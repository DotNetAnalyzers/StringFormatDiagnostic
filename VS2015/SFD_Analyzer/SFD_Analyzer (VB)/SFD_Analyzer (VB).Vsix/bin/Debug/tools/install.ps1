param($installPath, $toolsPath, $package, $project)

$analyzerPath = join-path $toolsPath "analyzers"
$analyzerFilePath = join-path $analyzerPath "SFD_Analyzer.dll"

$project.Object.AnalyzerReferences.Add("$analyzerFilePath")