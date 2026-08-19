# cmdpal-settings-import.ps1 - dotfiles의 Command Palette 설정을 시스템에 반영
# 사용법: .\cmdpal-settings-import.ps1 [-Install] [-DryRun]
#
#   -Install : extensions.txt의 winget 패키지를 먼저 설치
#   -DryRun  : 실제로 바꾸지 않고 무엇을 할지만 출력
#
# CmdPal은 종료할 때 settings.json을 다시 쓰기 때문에,
# 설정을 덮어쓰기 전에 반드시 프로세스를 종료해야 합니다.

param(
    [switch]$Install,
    [switch]$DryRun
)

$Repo = Split-Path $PSScriptRoot -Parent
$Src  = Join-Path $Repo 'windows\cmdpal'

$PackagesDir = "$env:LOCALAPPDATA\Packages"
$CmdPalState = Join-Path $PackagesDir 'Microsoft.CommandPalette_8wekyb3d8bbwe\LocalState'

$Extensions = @{
    'everything'    = 'VictorLin.EverythingCP_yazqh14evg2ve'
    'processkiller' = '8LWXpg.ProcessKillerforCommandPalette_a6kn9e4pg75pr'
}

function Write-Action($action, $msg, $color = 'Cyan') {
    Write-Host "[$action] " -ForegroundColor $color -NoNewline
    Write-Host $msg
}

# ---------------------------------------------------------------------------
# 1. 확장 설치
# ---------------------------------------------------------------------------
if ($Install) {
    $listFile = Join-Path $Src 'extensions.txt'
    if (-not (Test-Path $listFile)) {
        Write-Action 'ERROR' "$listFile 없음" 'Red'
        exit 1
    }

    Get-Content $listFile | ForEach-Object {
        $id = $_.Trim()
        if (-not $id -or $id.StartsWith('#')) { return }

        if ($DryRun) {
            Write-Action 'INSTALL' "$id (dry-run)" 'Green'
            return
        }

        Write-Action 'INSTALL' $id 'Green'
        winget install --id $id --accept-package-agreements --accept-source-agreements
    }
    Write-Host ""
}

# ---------------------------------------------------------------------------
# 2. CmdPal 종료 (안 하면 종료할 때 설정을 되돌려 씀)
# ---------------------------------------------------------------------------
$proc = Get-Process -Name 'Microsoft.CmdPal.UI' -ErrorAction SilentlyContinue
if ($proc) {
    if ($DryRun) {
        Write-Action 'STOP' 'Microsoft.CmdPal.UI (dry-run)' 'Yellow'
    } else {
        Write-Action 'STOP' 'Microsoft.CmdPal.UI' 'Yellow'
        $proc | Stop-Process -Force
        Start-Sleep -Milliseconds 500
    }
}

# ---------------------------------------------------------------------------
# 3. 설정 복사
# ---------------------------------------------------------------------------
function Restore-Config($src, $dstDir, $name) {
    if (-not (Test-Path $src)) {
        Write-Action 'SKIP' "$name (repo에 없음)" 'DarkGray'
        return
    }
    if (-not (Test-Path $dstDir)) {
        Write-Action 'SKIP' "$name (대상 패키지 미설치)" 'DarkYellow'
        return
    }

    $dst = Join-Path $dstDir (Split-Path $src -Leaf)

    # 기존 설정 백업 (한 번만, .bak 이 없을 때)
    if ((Test-Path $dst) -and -not (Test-Path "$dst.bak")) {
        if (-not $DryRun) { Copy-Item $dst "$dst.bak" -Force }
        Write-Action 'BACKUP' "$dst.bak" 'DarkGray'
    }

    if (-not $DryRun) { Copy-Item $src $dst -Force }
    Write-Action 'RESTORE' "$name -> $dst" 'Green'
}

Restore-Config (Join-Path $Src 'settings.json') $CmdPalState 'settings.json'

foreach ($name in $Extensions.Keys) {
    $dir = Join-Path $Src "extensions\$name"
    if (-not (Test-Path $dir)) { continue }
    $state = Join-Path $PackagesDir "$($Extensions[$name])\LocalState"
    foreach ($f in Get-ChildItem $dir -File) {
        Restore-Config $f.FullName $state "$name/$($f.Name)"
    }
}

Write-Host ""
Write-Host "완료. PowerToys 설정에서 Command Palette를 다시 켜거나, 재실행하면 반영됩니다." -ForegroundColor Cyan
