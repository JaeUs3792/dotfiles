# stow.ps1 - GNU Stow 흉내내기 (Windows PowerShell)
# 사용법: .\stow.ps1 [-Unstow] [-DryRun] [-Verbose]
#
# ~/dotfiles 에서 실행하면 하위 파일들을 $HOME 에 심볼릭 링크로 연결합니다.
# 예: dotfiles/.config/nvim/init.lua -> $HOME/.config/nvim/init.lua

param(
    [switch]$Unstow,   # 심볼릭 링크 제거
    [switch]$DryRun,   # 실제 적용 없이 미리 보기
    [switch]$Verbose   # 상세 출력
)

$DotfilesDir = $PSScriptRoot | Split-Path -Parent
$TargetDir   = $HOME

# 무시할 항목 (.gitignore 처럼)
$IgnoreList = @(
    '.git',
    '.gitignore',
    'scripts',
    'README.md',
    '*.ps1'
)

function Should-Ignore($relativePath) {
    $name = Split-Path $relativePath -Leaf
    foreach ($pattern in $IgnoreList) {
        if ($name -like $pattern) { return $true }
        if ($relativePath -like "*\$pattern\*") { return $true }
    }
    return $false
}

function Write-Action($action, $msg, $color = 'Cyan') {
    Write-Host "[$action] " -ForegroundColor $color -NoNewline
    Write-Host $msg
}

$created = 0
$skipped = 0
$removed = 0
$errors  = 0

# dotfiles 아래 모든 파일 순회
Get-ChildItem -Path $DotfilesDir -Recurse -File | ForEach-Object {
    $file        = $_
    $relative    = $file.FullName.Substring($DotfilesDir.Length + 1)  # dotfiles\ 이후 경로

    if (Should-Ignore $relative) {
        if ($Verbose) { Write-Action 'SKIP' $relative 'DarkGray' }
        return
    }

    $linkPath    = Join-Path $TargetDir $relative
    $targetPath  = $file.FullName

    if ($Unstow) {
        # --- 링크 제거 ---
        if (Test-Path $linkPath) {
            $item = Get-Item $linkPath -Force
            if ($item.LinkType -eq 'SymbolicLink') {
                if (-not $DryRun) { Remove-Item $linkPath -Force }
                Write-Action 'REMOVE' $linkPath 'Yellow'
                $removed++
            } else {
                Write-Action 'SKIP' "$linkPath (심볼릭 링크 아님, 건드리지 않음)" 'DarkYellow'
                $skipped++
            }
        }
    } else {
        # --- 링크 생성 ---
        $linkDir = Split-Path $linkPath -Parent

        # 부모 디렉토리 생성
        if (-not (Test-Path $linkDir)) {
            if (-not $DryRun) { New-Item -ItemType Directory -Path $linkDir -Force | Out-Null }
            if ($Verbose) { Write-Action 'MKDIR' $linkDir 'DarkCyan' }
        }

        if (Test-Path $linkPath -PathType Leaf) {
            $existing = Get-Item $linkPath -Force
            if ($existing.LinkType -eq 'SymbolicLink') {
                # 이미 올바른 링크인지 확인
                $resolvedTarget = $existing.Target
                if ($resolvedTarget -eq $targetPath) {
                    if ($Verbose) { Write-Action 'EXISTS' $linkPath 'DarkGray' }
                    $skipped++
                    return
                } else {
                    Write-Action 'CONFLICT' "$linkPath -> $resolvedTarget (예상: $targetPath)" 'Red'
                    $errors++
                    return
                }
            } else {
                Write-Action 'CONFLICT' "$linkPath (일반 파일이 이미 존재)" 'Red'
                $errors++
                return
            }
        }

        if (-not $DryRun) {
            try {
                New-Item -ItemType SymbolicLink -Path $linkPath -Target $targetPath -Force | Out-Null
                Write-Action 'LINK' "$linkPath -> $targetPath" 'Green'
                $created++
            } catch {
                Write-Action 'ERROR' "$linkPath : $_" 'Red'
                $errors++
            }
        } else {
            Write-Action 'LINK' "$linkPath -> $targetPath (dry-run)" 'Green'
            $created++
        }
    }
}

# 요약
Write-Host ""
if ($Unstow) {
    Write-Host "완료: 제거 $removed, 건너뜀 $skipped, 오류 $errors" -ForegroundColor Cyan
} else {
    Write-Host "완료: 생성 $created, 건너뜀 $skipped, 오류 $errors" -ForegroundColor Cyan
}

if ($errors -gt 0) {
    Write-Host "충돌 파일은 수동으로 처리해주세요." -ForegroundColor Yellow
}
