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
#   vibe-local --debug               # Debug mode

param(
    [switch]$Auto,
    [switch]$Yes,
    [switch]$Debug,
    [string]$Model,
    [Parameter(ValueFromRemainingArguments)]
    [string[]]$ExtraArgs
)

$ErrorActionPreference = "Continue"

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

# Parse config file (safe grep-style, no dot-sourcing)
if (Test-Path $ConfigFile) {
    $configLines = Get-Content $ConfigFile -ErrorAction SilentlyContinue
    foreach ($line in $configLines) {
        if ($line -match '^\s*#') { continue }
        if ($line -match '^\s*MODEL\s*=\s*"?([^"]*)"?\s*$') { $CfgModel = $Matches[1].Trim() }
        if ($line -match '^\s*SIDECAR_MODEL\s*=\s*"?([^"]*)"?\s*$') { $SidecarModel = $Matches[1].Trim() }
        if ($line -match '^\s*OLLAMA_HOST\s*=\s*"?([^"]*)"?\s*$') { $OllamaHost = $Matches[1].Trim() }
        if ($line -match '^\s*VIBE_LOCAL_DEBUG\s*=\s*"?([01])"?\s*$') { $VibeLocalDebug = [int]$Matches[1] }
    }
}

# Command line overrides
if ($Model) { $CfgModel = $Model }
if ($Debug) { $VibeLocalDebug = 1 }

# [SEC] Validate OLLAMA_HOST - only allow localhost (SSRF prevention)
$ollamaUri = [System.Uri]::new($OllamaHost)
if ($ollamaUri.Host -notin @("localhost", "127.0.0.1", "::1", "[::1]")) {
    Write-Host "Warning: OLLAMA_HOST '$($ollamaUri.Host)' is not localhost. Resetting to localhost for security." -ForegroundColor Yellow
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

# --- Ensure Ollama is running ---
function Ensure-Ollama {
    try {
        $null = Invoke-RestMethod -Uri "$OllamaHost/api/tags" -TimeoutSec 2 -ErrorAction Stop
        return $true
    } catch {}

    Write-Host "Starting Ollama..." -ForegroundColor Cyan
    try {
        Start-Process ollama -ArgumentList "serve" -WindowStyle Hidden -ErrorAction Stop
    } catch {
        Write-Host "Error: Could not start Ollama. Make sure it's installed: winget install Ollama.Ollama" -ForegroundColor Red
        return $false
    }

    for ($i = 1; $i -le 15; $i++) {
        Write-Host "`r  Waiting for Ollama... ${i}s " -NoNewline
        Start-Sleep -Seconds 2
        try {
            $null = Invoke-RestMethod -Uri "$OllamaHost/api/tags" -TimeoutSec 2 -ErrorAction Stop
            Write-Host "`r                                    "
            Write-Host "Ollama started successfully" -ForegroundColor Green
            return $true
        } catch {}
    }
    Write-Host ""
    Write-Host "Error: Ollama failed to start within 30 seconds" -ForegroundColor Red
    return $false
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
    if (-not (Ensure-Ollama)) {
        Write-Host "Cannot start Ollama. Exiting." -ForegroundColor Red
        exit 1
    }

    # Check model is available (if specified)
    if ($CfgModel) {
        try {
            $tags = Invoke-RestMethod -Uri "$OllamaHost/api/tags" -TimeoutSec 5 -ErrorAction Stop
            $modelFound = $tags.models | Where-Object { $_.name -eq $CfgModel }
            if (-not $modelFound) {
                Write-Host "Error: Model '$CfgModel' not found" -ForegroundColor Red
                Write-Host "  Run: ollama pull `"$CfgModel`""
                Write-Host ""
                Write-Host "Available models:"
                foreach ($m in $tags.models) { Write-Host "  - $($m.name)" }
                exit 1
            }
        } catch {
            Write-Host "Warning: Could not verify model availability" -ForegroundColor Yellow
        }
    }

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
    Write-Host " Ollama: $OllamaHost"
    Write-Host " Engine: vibe-coder.py (direct, no proxy)"
    Write-Host "============================================"
    Write-Host ""

    $env:OLLAMA_HOST = $OllamaHost
    $env:VIBE_LOCAL_MODEL = $CfgModel
    $env:VIBE_LOCAL_SIDECAR_MODEL = if ($SidecarModel) { $SidecarModel } else { "" }
    $env:VIBE_LOCAL_DEBUG = "$VibeLocalDebug"

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
    Remove-Item Env:VIBE_LOCAL_MODEL -ErrorAction SilentlyContinue
    Remove-Item Env:VIBE_LOCAL_SIDECAR_MODEL -ErrorAction SilentlyContinue
    Remove-Item Env:VIBE_LOCAL_DEBUG -ErrorAction SilentlyContinue
}
