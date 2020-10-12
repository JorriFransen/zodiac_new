
$sdir = "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\Common7\Tools"

pushd $sdir

cmd /c "VsDevCmd.bat -arch=amd64&set" |
foreach {
  if ($_ -match "=") {
    $v = $_.split("="); set-item -force -path "ENV:\$($v[0])"  -value "$($v[1])"
  }
}
popd
Write-Host "`nVisual Studio Command Prompt variables set." -ForegroundColor Yellow
