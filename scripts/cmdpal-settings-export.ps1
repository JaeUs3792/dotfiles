# cmdpal-settings-export.ps1 - PowerToys Command Palette 설정을 dotfiles로 내보내기
# 사용법: .\cmdpal-settings-export.ps1 [-DryRun]
#
# CmdPal은 MSIX 패키지라 설정이 LocalState 아래에 있고, 종료할 때 파일을 다시 씁니다.
# 그래서 심볼릭 링크(stow) 대신 복사 방식으로 관리합니다.
# 정확한 스냅샷을 뜨려면 CmdPal을 먼저 종료하세요.

param(
    [switch]$DryRun
)

$Repo = Split-Path $PSScriptRoot -Parent
$Dest = Join-Path $Repo 'windows\cmdpal'

$PackagesDir = "$env:LOCALAPPDATA\Packages"
$CmdPalState = Join-Path $PackagesDir 'Microsoft.CommandPalette_8wekyb3d8bbwe\LocalState'

# 확장 패키지 (패키지 패밀리 이름 -> 저장 디렉토리 이름)
$Extensions = @{
    'VictorLin.EverythingCP_yazqh14evg2ve'                  = 'everything'
    '8LWXpg.ProcessKillerforCommandPalette_a6kn9e4pg75pr'   = 'processkiller'
}

function Write-Action($action, $msg, $color = 'Cyan') {
    Write-Host "[$action] " -ForegroundColor $color -NoNewline
    Write-Host $msg
}

function Copy-Config($src, $dst) {
    if (-not (Test-Path $src)) {
        Write-Action 'SKIP' "$src (없음)" 'DarkGray'
        return
    }
    $dstDir = Split-Path $dst -Parent
    if (-not (Test-Path $dstDir)) {
        if (-not $DryRun) { New-Item -ItemType Directory -Path $dstDir -Force | Out-Null }
    }
    if (-not $DryRun) { Copy-Item $src $dst -Force }
    Write-Action 'COPY' "$dst" 'Green'
}

# CmdPal이 실행 중이면 경고 (종료 시 settings.json을 덮어씀)
if (Get-Process -Name 'Microsoft.CmdPal.UI' -ErrorAction SilentlyContinue) {
    Write-Host "[WARN] CmdPal이 실행 중입니다. 최근 변경이 아직 파일에 안 써졌을 수 있습니다." -ForegroundColor Yellow
    Write-Host "       정확한 스냅샷을 원하면 CmdPal을 종료한 뒤 다시 실행하세요." -ForegroundColor Yellow
}

# 본체 설정
# state.json(실행 기록), commandProviderCache.json(캐시)은 재생성되는 파일이라 제외
Copy-Config (Join-Path $CmdPalState 'settings.json') (Join-Path $Dest 'settings.json')

# 확장별 설정 (설정을 한 번이라도 바꿔야 파일이 생김)
foreach ($pfn in $Extensions.Keys) {
    $state = Join-Path $PackagesDir "$pfn\LocalState"
    if (-not (Test-Path $state)) {
        Write-Action 'SKIP' "$($Extensions[$pfn]) (패키지 없음)" 'DarkGray'
        continue
    }
    $files = Get-ChildItem $state -File -ErrorAction SilentlyContinue
    if (-not $files) {
        Write-Action 'SKIP' "$($Extensions[$pfn]) (설정 파일 없음 - 기본값 사용 중)" 'DarkGray'
        continue
    }
    foreach ($f in $files) {
        Copy-Config $f.FullName (Join-Path $Dest "extensions\$($Extensions[$pfn])\$($f.Name)")
    }
}

Write-Host ""
Write-Host "확장 설치 목록은 windows/cmdpal/extensions.txt 에서 직접 관리합니다." -ForegroundColor DarkGray
Write-Host "완료." -ForegroundColor Cyan
