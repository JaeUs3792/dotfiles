# init_script_windows.ps1
# Installs fonts equivalent to the CachyOS init script:
#   ttf-firacode-nerd, ttf-mononoki-nerd, otf-comicshanns-nerd,
#   ttf-nanum, noto-fonts-emoji, noto-fonts-cjk, ttf-symbola
#
# Times New Roman is already bundled with Windows, so it is skipped.
#
# Usage:
#   .\init_script_windows.ps1              # install to user fonts (no admin required)
#   .\init_script_windows.ps1 -SystemWide  # install to C:\Windows\Fonts  (requires admin)

param(
    [switch]$SystemWide
)

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
$TempDir = Join-Path $env:TEMP "font_install"
$UserFontDir = "$env:LOCALAPPDATA\Microsoft\Windows\Fonts"

if ($SystemWide) {
    $FontDir = "C:\Windows\Fonts"
} else {
    $FontDir = $UserFontDir
    New-Item -ItemType Directory -Path $FontDir -Force | Out-Null
}

New-Item -ItemType Directory -Path $TempDir -Force | Out-Null

# Pre-load ZIP assembly to suppress the GAC output that Expand-Archive prints on first use
Add-Type -AssemblyName System.IO.Compression.FileSystem | Out-Null

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
function Write-Step($msg) {
    Write-Host "`n==> $msg" -ForegroundColor Cyan
}

function Write-Ok($msg) {
    Write-Host "  [OK] $msg" -ForegroundColor Green
}

function Write-Err($msg) {
    Write-Host "  [ERR] $msg" -ForegroundColor Red
}

function Download-File($url, $dest) {
    try {
        Invoke-WebRequest -Uri $url -OutFile $dest -UseBasicParsing -MaximumRedirection 10 `
            -Headers @{ "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)" }
    } catch {
        Write-Err "Failed to download: $url"
        Write-Err $_.Exception.Message
        return $false
    }
    return $true
}

function Expand-Zip($zip, $dest) {
    # Use ZipFile directly instead of Expand-Archive to avoid WindowsBase.dll GAC output
    [System.IO.Compression.ZipFile]::ExtractToDirectory($zip, $dest, $true)
}

function Install-FontFile($path) {
    $name = Split-Path $path -Leaf
    $dest = Join-Path $FontDir $name

    # Skip if already installed
    if (Test-Path $dest) {
        Write-Host "  [SKIP] $name (already exists)" -ForegroundColor DarkGray
        return
    }

    Copy-Item $path $dest -Force

    # Register in registry for per-user install
    if (-not $SystemWide) {
        $regPath = "HKCU:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Fonts"
        $ext = [System.IO.Path]::GetExtension($name).ToLower()
        $type = if ($ext -eq ".otf") { "OpenType" } else { "TrueType" }
        $regName = "$([System.IO.Path]::GetFileNameWithoutExtension($name)) ($type)"
        Set-ItemProperty -Path $regPath -Name $regName -Value $dest -Force
    }

    Write-Ok $name
}

function Install-FontsFromDir($dir) {
    Get-ChildItem -Path $dir -Include "*.ttf","*.otf" -Recurse | ForEach-Object {
        Install-FontFile $_.FullName
    }
}

function Install-NerdFont($fontName) {
    Write-Step "Nerd Font: $fontName"

    # Skip download if font files are already installed
    $existing = Get-ChildItem -Path $FontDir -Filter "*$fontName*" -ErrorAction SilentlyContinue
    if ($existing.Count -gt 0) {
        Write-Host "  [SKIP] $fontName already installed ($($existing.Count) files)" -ForegroundColor DarkGray
        return
    }

    # Get latest release download URL via GitHub API
    $apiUrl  = "https://api.github.com/repos/ryanoasis/nerd-fonts/releases/latest"
    try {
        $release = Invoke-RestMethod -Uri $apiUrl -UseBasicParsing
    } catch {
        Write-Err "Failed to fetch nerd-fonts release info"
        return
    }

    $asset = $release.assets | Where-Object { $_.name -eq "$fontName.zip" } | Select-Object -First 1
    if (-not $asset) {
        Write-Err "$fontName.zip not found in latest release"
        return
    }

    $zip     = Join-Path $TempDir "$fontName.zip"
    $unzipDir = Join-Path $TempDir $fontName

    if (Download-File $asset.browser_download_url $zip) {
        New-Item -ItemType Directory -Path $unzipDir -Force | Out-Null
        Expand-Zip $zip $unzipDir
        Install-FontsFromDir $unzipDir
    }
}

function Install-GithubFont($repo, $dirPath, $name) {
    Write-Step "Font: $name"

    # Use GitHub Contents API to list font files — avoids downloading the whole repo archive
    $apiUrl = "https://api.github.com/repos/$repo/contents/$dirPath"
    try {
        $entries = Invoke-RestMethod -Uri $apiUrl -UseBasicParsing
    } catch {
        Write-Err "Failed to list font files from $repo/$dirPath"
        Write-Err $_.Exception.Message
        return
    }

    $fontEntries = $entries | Where-Object { $_.name -match '\.(ttf|otf)$' }
    if (-not $fontEntries) {
        Write-Err "No .ttf/.otf files found in $repo/$dirPath"
        return
    }

    # Check if all files are already installed before downloading anything
    $missing = $fontEntries | Where-Object { -not (Test-Path (Join-Path $FontDir $_.name)) }
    if (-not $missing) {
        Write-Host "  [SKIP] $name already installed" -ForegroundColor DarkGray
        return
    }

    foreach ($entry in $missing) {
        $tmpFile = Join-Path $TempDir $entry.name
        if (Download-File $entry.download_url $tmpFile) {
            Install-FontFile $tmpFile
        }
    }
}

# ---------------------------------------------------------------------------
# Font installations
# ---------------------------------------------------------------------------

# Nerd Fonts (FiraCode, Mononoki, ComicShannsMono)
Install-NerdFont "FiraCode"
Install-NerdFont "Mononoki"
Install-NerdFont "ComicShannsMono"

# Nanum (Korean font family) — from google/fonts GitHub repo
Install-GithubFont "google/fonts" "ofl/nanumgothic"       "Nanum Gothic"
Install-GithubFont "google/fonts" "ofl/nanummyeongjo"     "Nanum Myeongjo"
Install-GithubFont "google/fonts" "ofl/nanumgothiccoding" "Nanum Gothic Coding"

# Noto Emoji — from google/fonts GitHub repo
Install-GithubFont "google/fonts" "ofl/notoemoji" "Noto Emoji"

# Noto Sans CJK — from google/fonts GitHub repo
Install-GithubFont "google/fonts" "ofl/notosanskr" "Noto Sans KR"
Install-GithubFont "google/fonts" "ofl/notosansjp" "Noto Sans JP"
Install-GithubFont "google/fonts" "ofl/notosanssc" "Noto Sans SC"

# Symbola — available for free via Font Library (freeware by George Douros)
Write-Step "Symbola"
if (Test-Path (Join-Path $FontDir "Symbola.ttf")) {
    Write-Host "  [SKIP] Symbola already installed" -ForegroundColor DarkGray
} else {
    $symbolaZip = Join-Path $TempDir "Symbola.zip"
    $symbolaDir = Join-Path $TempDir "Symbola"
    if (Download-File "https://fontlibrary.org/assets/downloads/symbola/cf81aeb303c13ce765877d31571dc5c7/symbola.zip" $symbolaZip) {
        New-Item -ItemType Directory -Path $symbolaDir -Force | Out-Null
        Expand-Zip $symbolaZip $symbolaDir
        Install-FontsFromDir $symbolaDir
    }
}

# ---------------------------------------------------------------------------
# RTC: Set hardware clock to UTC (for dual-boot with Linux)
# ---------------------------------------------------------------------------
Write-Step "RTC: Setting hardware clock to UTC"

$isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole(
    [Security.Principal.WindowsBuiltInRole]::Administrator
)

if (-not $isAdmin) {
    Write-Host "  [SKIP] Requires administrator privileges (re-run as admin)" -ForegroundColor Yellow
} else {
    $rtcKey = "HKLM:\SYSTEM\CurrentControlSet\Control\TimeZoneInformation"
    $current = (Get-ItemProperty -Path $rtcKey -Name "RealTimeIsUniversal" -ErrorAction SilentlyContinue).RealTimeIsUniversal
    if ($current -eq 1) {
        Write-Host "  [SKIP] Already set to UTC" -ForegroundColor DarkGray
    } else {
        Set-ItemProperty -Path $rtcKey -Name "RealTimeIsUniversal" -Value 1 -Type DWord -Force
        Write-Ok "Hardware clock set to UTC (takes effect after reboot)"
    }
}

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------
Write-Step "Cleaning up temp files"
Remove-Item -Path $TempDir -Recurse -Force
Write-Ok "Done. You may need to restart applications to see the new fonts."
