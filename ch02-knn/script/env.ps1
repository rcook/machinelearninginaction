$lapackPathFile = Join-Path $PSScriptRoot ..\.stack-work\lapack-path.txt

if (Test-Path $lapackPathFile) {
  $lapackPath = (Get-Content -Raw $lapackPathFile).Trim()
}
else {
  pushd $PSScriptRoot
  stack exec -- sh get-lapack-path.sh | Add-Content $lapackPathFile
  popd
}

if (-not ($env:PATH -match [Regex]::Escape($lapackPath))) {
  $env:PATH = $lapackPath + ";" + $env:PATH
}
