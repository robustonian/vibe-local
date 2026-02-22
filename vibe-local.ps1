# vibe-local.ps1
# Windows launcher for vibe-local
# Equivalent to vibe-local.sh for macOS/Linux
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
$ProxyLibDir = Join-Path $env:USERPROFILE ".local\lib\vibe-local"
$ProxyScript = Join-Path $ProxyLibDir "anthropic-ollama-proxy.py"

# Defaults
$CfgModel = ""
$SidecarModel = ""
$OllamaHost = "http://localhost:11434"
$ProxyPort = 8082
$VibeLocalDebug = 0

# Parse config file (safe grep-style, no dot-sourcing)
if (Test-Path $ConfigFile) {
    $configLines = Get-Content $ConfigFile -ErrorAction SilentlyContinue
    foreach ($line in $configLines) {
        if ($line -match '^\s*#') { continue }
        if ($line -match '^\s*MODEL\s*=\s*"?([^"]*)"?\s*$') { $CfgModel = $Matches[1].Trim() }
        if ($line -match '^\s*SIDECAR_MODEL\s*=\s*"?([^"]*)"?\s*$') { $SidecarModel = $Matches[1].Trim() }
        if ($line -match '^\s*PROXY_PORT\s*=\s*"?(\d+)"?\s*$') { $ProxyPort = [int]$Matches[1] }
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

# [SEC] Validate PROXY_PORT is numeric and in range
if ($ProxyPort -lt 1024 -or $ProxyPort -gt 65535) {
    Write-Host "Warning: PROXY_PORT $ProxyPort is out of range (1024-65535). Resetting to 8082." -ForegroundColor Yellow
    $ProxyPort = 8082
}

# --- RAM detection & model auto-select ---
try {
    $TotalMem = (Get-CimInstance Win32_ComputerSystem).TotalPhysicalMemory
    $RamGB = [math]::Floor($TotalMem / 1073741824)
} catch {
    $RamGB = 16  # fallback
    Write-Host "Warning: Could not detect RAM, assuming ${RamGB}GB" -ForegroundColor Yellow
}

if (-not $CfgModel) {
    if ($RamGB -ge 32) {
        $CfgModel = "qwen3-coder:30b"
    } elseif ($RamGB -ge 16) {
        $CfgModel = "qwen3:8b"
    } elseif ($RamGB -ge 8) {
        $CfgModel = "qwen3:1.7b"
    } else {
        Write-Host "Error: Not enough RAM (${RamGB}GB). Minimum 8GB required." -ForegroundColor Red
        exit 1
    }
}

if (-not $SidecarModel) {
    if ($RamGB -ge 32) {
        $SidecarModel = "qwen3:8b"
    } elseif ($RamGB -ge 16) {
        $SidecarModel = "qwen3:1.7b"
    }
}

$ProxyUrl = "http://127.0.0.1:${ProxyPort}"
$ProxyPidFile = Join-Path $StateDir "proxy.pid"
$ProxyLog = Join-Path $StateDir "proxy.log"

# --- Dev fallback: find proxy script ---
if (-not (Test-Path $ProxyScript)) {
    $ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
    $DevProxy = Join-Path $ScriptDir "anthropic-ollama-proxy.py"
    if (Test-Path $DevProxy) {
        $ProxyScript = $DevProxy
    } else {
        Write-Host "Error: Proxy script not found" -ForegroundColor Red
        Write-Host "  Run install.ps1 or place anthropic-ollama-proxy.py in the same directory"
        exit 1
    }
}

# --- Find Python command ---
function Find-Python {
    # Try py launcher first (Python Launcher for Windows)
    try {
        $null = & py -3 --version 2>&1
        if ($LASTEXITCODE -eq 0) { return "py -3" }
    } catch {}
    # Try python3
    try {
        $null = & python3 --version 2>&1
        if ($LASTEXITCODE -eq 0) { return "python3" }
    } catch {}
    # Try python
    try {
        $null = & python --version 2>&1
        if ($LASTEXITCODE -eq 0) { return "python" }
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

# --- Ensure proxy is running ---
function Ensure-Proxy {
    # Check existing proxy via PID file
    if (Test-Path $ProxyPidFile) {
        $oldPid = [int](Get-Content $ProxyPidFile -ErrorAction SilentlyContinue)
        $proc = Get-Process -Id $oldPid -ErrorAction SilentlyContinue
        if ($proc) {
            # Check if proxy script was updated
            $scriptMtime = (Get-Item $ProxyScript).LastWriteTime.Ticks
            $pidMtime = (Get-Item $ProxyPidFile).LastWriteTime.Ticks
            if ($scriptMtime -gt $pidMtime) {
                Write-Host "Proxy script updated. Restarting..." -ForegroundColor Yellow
                Stop-Process -Id $oldPid -Force -ErrorAction SilentlyContinue
                Remove-Item $ProxyPidFile -Force -ErrorAction SilentlyContinue
                Start-Sleep -Seconds 1
            } else {
                # Script unchanged, check if proxy responds
                try {
                    $null = Invoke-RestMethod -Uri "$ProxyUrl/" -TimeoutSec 1 -ErrorAction Stop
                    return $true
                } catch {}
            }
        } else {
            Remove-Item $ProxyPidFile -Force -ErrorAction SilentlyContinue
        }
    }

    # Check if proxy already running on port (no PID file)
    try {
        $null = Invoke-RestMethod -Uri "$ProxyUrl/" -TimeoutSec 1 -ErrorAction Stop
        return $true
    } catch {}

    # Find available port
    $port = $ProxyPort
    foreach ($tryPort in @($ProxyPort, 8083, 8084, 8085)) {
        $conn = Get-NetTCPConnection -LocalPort $tryPort -ErrorAction SilentlyContinue
        if (-not $conn) {
            $port = $tryPort
            break
        }
    }
    $script:ProxyPort = $port
    $script:ProxyUrl = "http://127.0.0.1:${port}"

    Write-Host "Starting Anthropic->Ollama proxy (port: $port)..." -ForegroundColor Cyan

    # Validate proxy script
    $pyParts = $PythonCmd -split ' '
    try {
        if ($pyParts.Count -eq 2) {
            & $pyParts[0] $pyParts[1] -c "import ast, sys; ast.parse(open(sys.argv[1]).read())" "$ProxyScript" 2>&1 | Out-Null
        } else {
            & $pyParts[0] -c "import ast, sys; ast.parse(open(sys.argv[1]).read())" "$ProxyScript" 2>&1 | Out-Null
        }
    } catch {
        Write-Host "Error: Proxy script is corrupted" -ForegroundColor Red
        return $false
    }

    # Launch proxy
    $env:OLLAMA_HOST = $OllamaHost
    $env:VIBE_LOCAL_MODEL = $CfgModel
    $env:VIBE_LOCAL_SIDECAR_MODEL = if ($SidecarModel) { $SidecarModel } else { $CfgModel }
    $env:VIBE_LOCAL_DEBUG = "$VibeLocalDebug"

    if ($pyParts.Count -eq 2) {
        $proxyProc = Start-Process -FilePath $pyParts[0] -ArgumentList "$($pyParts[1]) `"$ProxyScript`" $port" -WindowStyle Hidden -PassThru -RedirectStandardOutput "$ProxyLog.stdout" -RedirectStandardError "$ProxyLog"
    } else {
        $proxyProc = Start-Process -FilePath $pyParts[0] -ArgumentList "`"$ProxyScript`" $port" -WindowStyle Hidden -PassThru -RedirectStandardOutput "$ProxyLog.stdout" -RedirectStandardError "$ProxyLog"
    }
    $proxyProc.Id | Out-File -FilePath $ProxyPidFile -Encoding ascii -NoNewline

    # Wait for proxy to respond
    for ($i = 1; $i -le 15; $i++) {
        Write-Host "`r  Waiting for proxy... ${i}s " -NoNewline
        Start-Sleep -Seconds 1
        try {
            $null = Invoke-RestMethod -Uri "$script:ProxyUrl/" -TimeoutSec 1 -ErrorAction Stop
            Write-Host "`r                                    "
            Write-Host "Proxy started (PID: $($proxyProc.Id), port: $port)" -ForegroundColor Green
            return $true
        } catch {}
        # Check if process died
        if ($proxyProc.HasExited) {
            Write-Host ""
            Write-Host "Error: Proxy crashed on startup" -ForegroundColor Red
            if (Test-Path $ProxyLog) {
                Write-Host "--- Log ---"
                Get-Content $ProxyLog -Tail 20
                Write-Host "--- End ---"
            }
            return $false
        }
    }
    Write-Host ""
    Write-Host "Error: Proxy did not respond within 15 seconds" -ForegroundColor Red
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

# --- Cleanup ---
function Cleanup-Proxy {
    if (Test-Path $ProxyPidFile) {
        $pid = [int](Get-Content $ProxyPidFile -ErrorAction SilentlyContinue)
        Stop-Process -Id $pid -Force -ErrorAction SilentlyContinue
        Remove-Item $ProxyPidFile -Force -ErrorAction SilentlyContinue
    }
}

# --- Auto mode ---
if ($Auto) {
    if (Test-Network) {
        Write-Host "Network available -> launching normal Claude Code" -ForegroundColor Cyan
        claude @ExtraArgs
        exit $LASTEXITCODE
    } else {
        Write-Host "No network -> local mode ($CfgModel)" -ForegroundColor Yellow
    }
}

# --- Local mode startup ---
try {
    if (-not (Ensure-Ollama)) {
        Write-Host "Cannot start Ollama. Exiting." -ForegroundColor Red
        exit 1
    }

    # Check model is available
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

    # Start proxy
    if (-not (Ensure-Proxy)) {
        Write-Host "Cannot start proxy. Exiting." -ForegroundColor Red
        exit 1
    }

    # --- Permission check ---
    $PermArgs = @()

    if ($Yes) {
        $PermArgs += "--dangerously-skip-permissions"
    } else {
        Write-Host ""
        Write-Host "============================================"
        Write-Host " Warning: Permission Check" -ForegroundColor Yellow
        Write-Host "============================================"
        Write-Host ""
        Write-Host " vibe-local can run in auto-approve mode"
        Write-Host " (--dangerously-skip-permissions)."
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
            $PermArgs += "--dangerously-skip-permissions"
            Write-Host " -> Auto-approve mode" -ForegroundColor Yellow
        } else {
            Write-Host " -> Normal mode (ask each time)" -ForegroundColor Green
        }
    }

    $PermLabel = "Normal mode (ask each time)"
    if ($PermArgs.Count -gt 0) { $PermLabel = "Auto-approve" }

    $DebugLabel = "OFF"
    if ($VibeLocalDebug -eq 1) { $DebugLabel = "ON (full request/response logging)" }

    $SidecarLabel = if ($SidecarModel) { $SidecarModel } else { "none" }

    Write-Host ""
    Write-Host "============================================"
    Write-Host " vibe-local"
    Write-Host " Model: $CfgModel"
    Write-Host " Sidecar: $SidecarLabel"
    Write-Host " Proxy: $ProxyUrl -> $OllamaHost"
    Write-Host " Permissions: $PermLabel"
    Write-Host " Debug: $DebugLabel"
    Write-Host "============================================"
    Write-Host ""
    Write-Host " Launching Claude Code..."
    Write-Host ""

    $env:ANTHROPIC_BASE_URL = $ProxyUrl
    $env:ANTHROPIC_API_KEY = "local"
    $env:VIBE_LOCAL_DEBUG = "$VibeLocalDebug"

    $claudeArgs = @("--model", $CfgModel) + $PermArgs + $ExtraArgs
    & claude @claudeArgs
}
finally {
    Cleanup-Proxy
    # [SEC] Clean up environment variables set during this session
    Remove-Item Env:ANTHROPIC_BASE_URL -ErrorAction SilentlyContinue
    Remove-Item Env:ANTHROPIC_API_KEY -ErrorAction SilentlyContinue
    Remove-Item Env:VIBE_LOCAL_DEBUG -ErrorAction SilentlyContinue
    Remove-Item Env:OLLAMA_HOST -ErrorAction SilentlyContinue
    Remove-Item Env:VIBE_LOCAL_MODEL -ErrorAction SilentlyContinue
    Remove-Item Env:VIBE_LOCAL_SIDECAR_MODEL -ErrorAction SilentlyContinue
}
