#!/usr/bin/env pwsh
##############################################################################################################

function priv_clipper {
    $ENV = @{
        vagrant  = 'it-gro/win10-ltsc-eval'
        download = 'https://microsoft.com/en-us/evalcenter'
        package  = 'https://learn.microsoft.com/en-us/mem/configmgr/develop/apps/how-to-create-the-windows-installer-file-msi'
        shell    = 'https://learn.microsoft.com/ru-ru/powershell'
        habr     = 'https://habr.com/ru/companies/ruvds/articles/487876/'
    }
    Write-Output $ENV
}

function priv_prepare {
    $params = @{
        Uri = 'https://aka.ms/getwinget'
        OutFile = 'Microsoft.DesktopAppInstaller_8wekyb3d8bbwe.msixbundle'
    }
    $ProgressPreference = 'SilentlyContinue'
    Invoke-WebRequest @params
    $ProgressPreference = 'Continue'
    Add-AppxPackage Microsoft.DesktopAppInstaller_8wekyb3d8bbwe.msixbundle
    winget install --disable-interactivity lazarus git
}

function priv_packages {

}

function priv_main {
    if ($args.count > 0) {
        pub_prepare
        switch ($args[1]) {
            'build' {Invoke-ScriptAnalyzer -EnableExit -Recurse -Path scripts}
            default {Write-Output $args}
        }
    } else {
        Write-Output $args
    }
}

##############################################################################################################
priv_main @args
