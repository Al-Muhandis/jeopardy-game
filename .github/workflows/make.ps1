#!/usr/bin/env pwsh
#-------------------------------------------------------------------------------
# FILTERS AND FUNCTIONS
#-------------------------------------------------------------------------------

Filter Get-Url {
    $pattern = $_
    $baseUrl = 'https://fossies.org/windows/misc/'
    $baseUrl + (
        (Invoke-WebRequest -Uri $baseUrl -UseBasicParsing).Links.href |
            Where-Object { $_ -match '^{0}.*(exe|msi)$' -f $pattern }
    )[-1]
}

Filter Get-Package {
    $OutFile = (New-TemporaryFile).FullName + '.exe'
    Invoke-WebRequest -OutFile $OutFile -Uri $_
    $OutFile
}

Function Install-Package {
    $Input | ForEach-Object {
        $arguments = @(
            '/SP-',
            '/VERYSILENT',
            '/NORESTART',
            '/SUPPRESSMSGBOXES'
        )
        Start-Process -FilePath $_ -ArgumentList $arguments -Wait -NoNewWindow
        Remove-Item $_
    }
}

#-------------------------------------------------------------------------------
# MAIN ENDPOINT
#-------------------------------------------------------------------------------

$ErrorActionPreference = 'stop'
Set-PSDebug -Strict
@(
    'https://download.lazarus-ide.org/Lazarus%20Windows%2064%20bits/Lazarus%204.6/lazarus-4.6-fpc-3.2.2-win64.exe'
) | Get-Package | Install-Package
$env:PATH+=';C:\Lazarus'
(Get-Command 'lazbuild').Source | Out-Host
$env:PATH+=';C:\Lazarus\fpc\3.2.2\bin\x86_64-win64'
(Get-Command 'instantfpc').Source | Out-Host
$env:INSTANTFPCOPTIONS='-FuC:\Lazarus\components\lazutils'
& instantfpc '.github/workflows/make.pas' build | Out-Host
Exit $LastExitCode