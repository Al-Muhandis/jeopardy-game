#!/usr/bin/env pwsh
##############################################################################################################

Function PrivClipper {
    Write-Output "
Usage: pwsh -File $($PSCommandPath) [OPTIONS]
Options:
    build   Build program
"
}

Function PrivMsiexec {
    Foreach ($REPLY in $args) {
        $params = @{
            Uri = $REPLY
            OutFile = (Split-Path -Path $REPLY -Leaf).Split('?')[0]
        }
        Write-Output "Invoke-WebRequest $($params.Uri)"
        $ProgressPreference = 'SilentlyContinue'
        Invoke-WebRequest @params
        $ProgressPreference = 'Continue'
        Switch ((Split-Path -Path $params.OutFile -Leaf).Split('.')[-1]) {
            'msi' {
                If (Get-Command 'msiexec' | Out-Null) {
                    Start-Process -Wait -FilePath 'msiexec' -ArgumentList '/passive', '/package', $params.OutFile
                }
            }
            Default {
                If ($Env:OS == 'Windows_NT') {
                    Start-Process -Wait $params.OutFile -ArgumentList '/silent', '/norestart', "/dir=$($Env:HOME)/$REPLY"
                }
            }
        }
        Write-Output "Remove-Item $($params.OutFile)"
        Remove-Item $params.OutFile
    }
}

Function PrivPrepare {
    $VAR = @{
        git  = 'https://github.com/git-for-windows/git/releases/download/v2.47.0.windows.2/Git-2.47.0.2-64-bit.exe'
        lazbuild = 'https://netix.dl.sourceforge.net/project/lazarus/Lazarus%20Windows%2064%20bits/Lazarus%203.6/lazarus-3.6-fpc-3.2.2-win64.exe?viasf=1'
    }
    ForEach ($REPLY in $VAR.Keys) {
        Write-Output "Check $REPLY"
        If (-not (Get-Command $REPLY | Out-Null)) {
            Write-Output "Install $REPLY"
            PrivMsiexec $VAR[$REPLY]
            Get-ChildItem -Filter $REPLY -Recurse -File –Path $Env:HOME
        }
    }
}

Function PrivPkgsearch {
    ForEach ($REPLY in $args) {
        If (-not (Start-Process -Wait -FilePath 'lazbuild' -ArgumentList '--verbose-pkgsearch', $REPLY)) {
            Start-Process -Wait -FilePath 'lazbuild' -ArgumentList '--add-package', $REPLY
        }
    }
}

Function PrivPackages {
    If ( Test-Path -Path 'use' ) {
        Start-Process -Wait -FilePath 'git' -ArgumentList 'submodule', 'update', '--recursive', '--init'
        Start-Process -Wait -FilePath 'git' -ArgumentList 'submodule', 'update', '--recursive', '--remote'
    } Else {
        New-Item -ItemType Directory -Name 'use'
    }
    If ($args.count -gt 0) {
        ForEach ($REPLY in $args) {
            $params = @{
                Uri = "https://packages.lazarus-ide.org/$($REPLY).zip"
                OutFile = "$($REPLY).zip"
            }
            $ProgressPreference = 'SilentlyContinue'
            Invoke-WebRequest @params
            $ProgressPreference = 'Continue'
            Expand-Archive -Path $params.OutFile -DestinationPath $REPLY -Force
            Remove-Item $params.OutFile
        }
    }
    Get-ChildItem -Filter '*.lpk' -Recurse -File –Path 'use' | ForEach-Object {
        Start-Process -Wait -FilePath 'lazbuild' -ArgumentList '--add-package-link', $_.Name
    }
}

Function PrivMain {
    Invoke-ScriptAnalyzer -EnableExit -Path $PSCommandPath
    Set-PSDebug -Strict
    If ($args.count -gt 0) {
        PrivPrepare
        Switch ($args[0]) {
            'build' {
                PrivPkgsearch
                PrivPackages 'Rx' 'ZeosDBO'
                Get-ChildItem -Filter '*.lpi' -Recurse -File –Path 'src' | ForEach-Object {
                    Start-Process -Wait -FilePath 'lazbuild' -ArgumentList '--no-write-project', '--recursive', '--no-write-project', '--build-mode=release', $_.Name
                }
            }
            Default {
                PrivClipper
            }
        }
    } Else {
        PrivClipper
    }
}

##############################################################################################################
PrivMain @args
