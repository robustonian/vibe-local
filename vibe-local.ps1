# vibe-local.ps1
# Windows launcher for vibe-local
# Uses vibe-coder.py directly — no proxy, no Node.js, no Claude Code needed
#
# NOTE: This project is NOT affiliated with, endorsed by, or associated with Anthropic.
#
# Usage:
#   vibe-local                       # Interactive mode
#   vibe-local -p "question"         # One-shot
#   vibe-local --auto                # Auto-detect network
#   vibe-local --model qwen3:8b      # Manual model
#   vibe-local -y                    # Skip permission check
#   vibe-local -DebugMode             # Debug mode

param(
    [switch]$Auto,
    [switch]$Yes,
    [switch]$DebugMode,
    [string]$Model,
    [Parameter(ValueFromRemainingArguments)]
    [string[]]$ExtraArgs
)

$ErrorActionPreference = "Continue"

# --- UTF-8 encoding fix (PowerShell 文字化け対策) ---
try {
    [Console]::OutputEncoding = [System.Text.Encoding]::UTF8
    [Console]::InputEncoding = [System.Text.Encoding]::UTF8
    $OutputEncoding = [System.Text.Encoding]::UTF8
    $null = & cmd /c "chcp 65001 >nul 2>&1"
} catch {}

# --- Directory init ---
$StateDir = Join-Path $env:LOCALAPPDATA "vibe-local"
if (-not (Test-Path $StateDir)) { New-Item -ItemType Directory -Path $StateDir -Force | Out-Null }

# --- Config loading ---
$ConfigDir = Join-Path $env:USERPROFILE ".config\vibe-local"
$ConfigFile = Join-Path $ConfigDir "config"
$LibDir = Join-Path $env:USERPROFILE ".local\lib\vibe-local"
$VibeCoderScript = Join-Path $LibDir "vibe-coder.py"

# Defaults
$CfgModel = ""
$SidecarModel = ""
$OllamaHost = "http://localhost:11434"
$VibeLocalDebug = 0
$ApiKey = ""

# Parse config file (safe grep-style, no dot-sourcing)
if (Test-Path $ConfigFile) {
    $configLines = Get-Content $ConfigFile -ErrorAction SilentlyContinue
    foreach ($line in $configLines) {
        if ($line -match '^\s*#') { continue }
        if ($line -match '^\s*MODEL\s*=\s*"?([^"]*)"?\s*$') { $CfgModel = $Matches[1].Trim() }
        if ($line -match '^\s*SIDECAR_MODEL\s*=\s*"?([^"]*)"?\s*$') { $SidecarModel = $Matches[1].Trim() }
        if ($line -match '^\s*OLLAMA_HOST\s*=\s*"?([^"]*)"?\s*$') { $OllamaHost = $Matches[1].Trim() }
        if ($line -match '^\s*VIBE_LOCAL_DEBUG\s*=\s*"?([01])"?\s*$') { $VibeLocalDebug = [int]$Matches[1] }
        if ($line -match '^\s*API_KEY\s*=\s*"?([^"]*)"?\s*$') { $ApiKey = $Matches[1].Trim() }
    }
}

# .env from CWD (project-level config — higher priority than global config)
$DotEnvFile = Join-Path (Get-Location) ".env"
if ((Test-Path $DotEnvFile) -and (-not (Get-Item $DotEnvFile).LinkType)) {
    $dotEnvLines = Get-Content $DotEnvFile -ErrorAction SilentlyContinue
    foreach ($line in $dotEnvLines) {
        if ($line -match '^\s*#') { continue }
        if ($line -match '^\s*OLLAMA_HOST\s*=\s*"?([^"]*)"?\s*$') { $OllamaHost = $Matches[1].Trim() }
        if ($line -match '^\s*MODEL\s*=\s*"?([^"]*)"?\s*$') { $CfgModel = $Matches[1].Trim() }
        if ($line -match '^\s*SIDECAR_MODEL\s*=\s*"?([^"]*)"?\s*$') { $SidecarModel = $Matches[1].Trim() }
        if ($line -match '^\s*API_KEY\s*=\s*"?([^"]*)"?\s*$') { $ApiKey = $Matches[1].Trim() }
        if ($line -match '^\s*VIBE_LOCAL_DEBUG\s*=\s*"?([01])"?\s*$') { $VibeLocalDebug = [int]$Matches[1] }
    }
}

# Environment variables override config and .env (CLI flags still win later)
if ($env:OLLAMA_HOST) { $OllamaHost = $env:OLLAMA_HOST }
if ($env:VIBE_CODER_MODEL) { $CfgModel = $env:VIBE_CODER_MODEL }
if ($env:VIBE_LOCAL_MODEL) { $CfgModel = $env:VIBE_LOCAL_MODEL }
if ($env:VIBE_CODER_SIDECAR) { $SidecarModel = $env:VIBE_CODER_SIDECAR }
if ($env:VIBE_LOCAL_SIDECAR_MODEL) { $SidecarModel = $env:VIBE_LOCAL_SIDECAR_MODEL }
if ($env:VIBE_CODER_DEBUG -eq "1" -or $env:VIBE_LOCAL_DEBUG -eq "1") { $VibeLocalDebug = 1 }
if ($env:API_KEY) { $ApiKey = $env:API_KEY }
if ($env:VIBE_LOCAL_API_KEY) { $ApiKey = $env:VIBE_LOCAL_API_KEY }

# Command line overrides
if ($Model) { $CfgModel = $Model }
if ($DebugMode) { $VibeLocalDebug = 1 }

# [SEC] Validate OLLAMA_HOST - require http/https scheme, reject credential injection
try {
    $ollamaUri = [System.Uri]::new($OllamaHost)
    if ($ollamaUri.Scheme -notin @("http", "https") -or $OllamaHost -match "@") {
        throw "invalid scheme or credential injection"
    }
} catch {
    Write-Host "Warning: OLLAMA_HOST '$OllamaHost' is not a valid URL. Resetting to default." -ForegroundColor Yellow
    $OllamaHost = "http://localhost:11434"
}

# --- Find vibe-coder.py ---
if (-not (Test-Path $VibeCoderScript)) {
    $ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
    $DevScript = Join-Path $ScriptDir "vibe-coder.py"
    if (Test-Path $DevScript) {
        $VibeCoderScript = $DevScript
    } else {
        Write-Host "Error: vibe-coder.py not found" -ForegroundColor Red
        Write-Host "  Run install.ps1 or place vibe-coder.py in the same directory"
        exit 1
    }
}

# --- Find Python command ---
function Find-Python {
    try {
        $ver = & py -3 --version 2>&1
        if ($LASTEXITCODE -eq 0 -and "$ver" -match "Python 3") { return "py -3" }
    } catch {}
    try {
        $ver = & python3 --version 2>&1
        if ($LASTEXITCODE -eq 0 -and "$ver" -match "Python 3") { return "python3" }
    } catch {}
    try {
        $ver = & python --version 2>&1
        if ($LASTEXITCODE -eq 0 -and "$ver" -match "Python 3") { return "python" }
    } catch {}
    return $null
}

$PythonCmd = Find-Python
if (-not $PythonCmd) {
    Write-Host "Error: Python not found. Install Python 3: winget install Python.Python.3.12" -ForegroundColor Red
    exit 1
}

# --- Network check ---
function Test-Network {
    try {
        $null = Invoke-RestMethod -Uri "https://api.anthropic.com/" -TimeoutSec 3 -ErrorAction Stop
        return $true
    } catch {
        return $false
    }
}

# --- Auto mode ---
if ($Auto) {
    if (Test-Network) {
        $hasClaude = Get-Command claude -ErrorAction SilentlyContinue
        if ($hasClaude) {
            Write-Host "Network available + Claude Code found -> launching Claude Code" -ForegroundColor Cyan
            claude @ExtraArgs
            exit $LASTEXITCODE
        }
        Write-Host "Network available (no Claude Code) -> local mode" -ForegroundColor Yellow
    } else {
        Write-Host "No network -> local mode" -ForegroundColor Yellow
    }
}

# --- Local mode startup ---
try {
    # Connection and model availability checks are centralized in vibe-coder.py so
    # OpenAI-compatible endpoints can start without being treated as Ollama.

    # --- Permission check ---
    $PermArgs = @()

    if ($Yes) {
        $PermArgs += "-y"
    } else {
        Write-Host ""
        Write-Host "============================================"
        Write-Host " Warning: Permission Check" -ForegroundColor Yellow
        Write-Host "============================================"
        Write-Host ""
        Write-Host " vibe-local can run in auto-approve mode (-y)."
        Write-Host ""
        Write-Host " This means the AI can execute commands, read/write"
        Write-Host " files, and modify your system WITHOUT asking."
        Write-Host ""
        Write-Host " Local LLMs are less accurate than cloud AI."
        Write-Host " Unintended actions may occur."
        Write-Host ""
        Write-Host "--------------------------------------------"
        Write-Host " [y] Auto-approve mode"
        Write-Host " [N] Normal mode (ask before each tool use)"
        Write-Host "--------------------------------------------"
        Write-Host ""
        $reply = Read-Host " Continue? [y/N]"

        if ($reply -match '^[yY]') {
            $PermArgs += "-y"
            Write-Host " -> Auto-approve mode" -ForegroundColor Yellow
        } else {
            Write-Host " -> Normal mode (ask each time)" -ForegroundColor Green
        }
    }

    $DebugArgs = @()
    if ($VibeLocalDebug -eq 1) { $DebugArgs += "--debug" }

    $ModelArgs = @()
    if ($CfgModel) { $ModelArgs += @("--model", $CfgModel) }

    Write-Host ""
    Write-Host "============================================"
    Write-Host " vibe-local (vibe-coder)"
    if ($CfgModel) {
        Write-Host " Model: $CfgModel"
    } else {
        Write-Host " Model: (auto-detect)"
    }
    Write-Host " Endpoint: $OllamaHost"
    Write-Host " Engine: vibe-coder.py (direct, no proxy)"
    Write-Host "============================================"
    Write-Host ""

    $env:OLLAMA_HOST = $OllamaHost
    $env:VIBE_LOCAL_API_KEY = $ApiKey
    $env:VIBE_LOCAL_MODEL = $CfgModel
    $env:VIBE_LOCAL_SIDECAR_MODEL = if ($SidecarModel) { $SidecarModel } else { "" }
    $env:VIBE_LOCAL_DEBUG = "$VibeLocalDebug"
    $env:PYTHONIOENCODING = "utf-8"
    $env:PYTHONUTF8 = "1"

    $allArgs = $ModelArgs + $PermArgs + $DebugArgs + $ExtraArgs

    $pyParts = $PythonCmd -split ' '
    if ($pyParts.Count -eq 2) {
        & $pyParts[0] $pyParts[1] "$VibeCoderScript" @allArgs
    } else {
        & $pyParts[0] "$VibeCoderScript" @allArgs
    }
}
finally {
    # [SEC] Clean up environment variables set during this session
    Remove-Item Env:OLLAMA_HOST -ErrorAction SilentlyContinue
    Remove-Item Env:VIBE_LOCAL_API_KEY -ErrorAction SilentlyContinue
    Remove-Item Env:VIBE_LOCAL_MODEL -ErrorAction SilentlyContinue
    Remove-Item Env:VIBE_LOCAL_SIDECAR_MODEL -ErrorAction SilentlyContinue
    Remove-Item Env:VIBE_LOCAL_DEBUG -ErrorAction SilentlyContinue
    Remove-Item Env:PYTHONIOENCODING -ErrorAction SilentlyContinue
    Remove-Item Env:PYTHONUTF8 -ErrorAction SilentlyContinue
}
