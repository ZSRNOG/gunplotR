# Publish gunplotR to GitHub

This repository is prepared for publishing as `zsrnog/gunplotR`.

## Important Security Note

Do not use a GitHub account password in Git commands. GitHub no longer accepts
password authentication for Git pushes. Use one of these instead:

- GitHub CLI login: `gh auth login`
- HTTPS with a Personal Access Token
- SSH key authentication

If a password was pasted into a chat or terminal, rotate it in GitHub account
settings before publishing.

## Option 1: GitHub CLI

Install Git and GitHub CLI:

```powershell
winget install -e --id Git.Git
winget install -e --id GitHub.cli
```

Restart PowerShell, then:

```powershell
cd C:\Users\zsr\Documents\gunplotR
git init
git branch -M main
git add .
git commit -m "Initial release of gunplotR"

gh auth login
gh repo create zsrnog/gunplotR --public --source . --remote origin --push
```

## Option 2: HTTPS + Personal Access Token

Create a new GitHub repository named `gunplotR`, then run:

```powershell
cd C:\Users\zsr\Documents\gunplotR
git init
git branch -M main
git add .
git commit -m "Initial release of gunplotR"
git remote add origin https://github.com/zsrnog/gunplotR.git
git push -u origin main
```

When prompted for a password, paste a Personal Access Token, not the account
password.

## Option 3: SSH

```powershell
cd C:\Users\zsr\Documents\gunplotR
git init
git branch -M main
git add .
git commit -m "Initial release of gunplotR"
git remote add origin git@github.com:zsrnog/gunplotR.git
git push -u origin main
```

## After Publishing

The README badge will become active after GitHub Actions runs:

```text
https://github.com/zsrnog/gunplotR/actions
```

Users can then install the package with:

```r
install.packages("remotes")
remotes::install_github("zsrnog/gunplotR")
```
