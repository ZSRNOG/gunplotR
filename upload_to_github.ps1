$ErrorActionPreference = "Stop"

$repoDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$git = "C:\Program Files\Git\cmd\git.exe"
$gh = "C:\Program Files\GitHub CLI\gh.exe"

if (-not (Test-Path $git)) {
  throw "Cannot find git.exe. Install Git first: winget install -e --id Git.Git"
}
if (-not (Test-Path $gh)) {
  throw "Cannot find gh.exe. Install GitHub CLI first: winget install -e --id GitHub.cli"
}

Set-Location $repoDir

& $gh auth status
if ($LASTEXITCODE -ne 0) {
  Write-Host "Please log in to GitHub. A browser/device-code flow will start."
  & $gh auth login --hostname github.com --git-protocol https --web
}

& $git status --short

$remote = (& $git remote get-url origin 2>$null)
if (-not $remote) {
  & $gh repo create zsrnog/gunplotR --public --source . --remote origin --push
} else {
  & $git push -u origin main
}

Write-Host "Published: https://github.com/zsrnog/gunplotR"
