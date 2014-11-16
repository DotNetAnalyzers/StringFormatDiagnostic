﻿param($installPath, $toolsPath, $package, $project)

$analyzerPath = join-path $toolsPath "analyzers"
$analyzerFilePath = join-path $analyzerPath "VB_SFD.dll"

$project.Object.AnalyzerReferences.Remove("$analyzerFilePath")