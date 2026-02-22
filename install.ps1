# install.ps1
# vibe-local Windows installer
# Vaporwave aesthetic installer for Windows
#
# Usage:
#   .\install.ps1
#   .\install.ps1 -Model qwen3:8b
#   .\install.ps1 -Lang en

param(
    [string]$Model,
    [string]$Lang,
    [switch]$Help
)

$ErrorActionPreference = "Continue"
$ProgressPreference = "SilentlyContinue"  # Speed up Invoke-WebRequest

# ╔══════════════════════════════════════════════════════════════╗
# ║  🎨  Ｖ Ａ Ｐ Ｏ Ｒ Ｗ Ａ Ｖ Ｅ   Ｃ Ｏ Ｌ Ｏ Ｒ Ｓ    ║
# ╚══════════════════════════════════════════════════════════════╝

# ANSI escape support (Windows Terminal)
$ESC = [char]27
function C { param([int]$c) return "${ESC}[38;5;${c}m" }
function BG { param([int]$c) return "${ESC}[48;5;${c}m" }
$BOLD = "${ESC}[1m"
$DIM = "${ESC}[2m"
$NC = "${ESC}[0m"

$PINK = C 198; $HOT_PINK = C 206; $MAGENTA = C 165; $PURPLE = C 141
$CYAN = C 51; $AQUA = C 87; $MINT = C 121; $CORAL = C 210
$ORANGE = C 208; $YELLOW = C 226; $WHITE = C 255; $GRAY = C 245
$RED = C 196; $GREEN = C 46; $NEON_GREEN = C 118; $BLUE = C 33

# ╔══════════════════════════════════════════════════════════════╗
# ║  🌐  ＴＲＩＬＩＮＧＵＡＬ  ＥＮＧＩＮＥ                ║
# ╚══════════════════════════════════════════════════════════════╝

# Auto-detect language
if (-not $Lang) {
    $sysLang = (Get-Culture).Name
    if ($sysLang -like "ja*") { $Lang = "ja" }
    elseif ($sysLang -like "zh*") { $Lang = "zh" }
    else { $Lang = "en" }
}

$Messages = @{
    ja = @{
        subtitle = "  無 料 Ａ Ｉ コ ー デ ィ ン グ 環 境"
        tagline = "ネットワーク不要 ・ 完全無料 ・ ローカルAIコーディング"
        step1 = "Ｓ Ｙ Ｓ Ｔ Ｅ Ｍ  Ｓ Ｃ Ａ Ｎ"
        step2 = "Ｍ Ｅ Ｍ Ｏ Ｒ Ｙ  Ａ Ｎ Ａ Ｌ Ｙ Ｓ Ｉ Ｓ"
        step3 = "Ｐ Ａ Ｃ Ｋ Ａ Ｇ Ｅ  Ｉ Ｎ Ｓ Ｔ Ａ Ｌ Ｌ"
        step4 = "Ａ Ｉ  Ｍ Ｏ Ｄ Ｅ Ｌ  Ｄ Ｏ Ｗ Ｎ Ｌ Ｏ Ａ Ｄ"
        step5 = "Ｆ Ｉ Ｌ Ｅ  Ｄ Ｅ Ｐ Ｌ Ｏ Ｙ"
        step6 = "Ｃ Ｏ Ｎ Ｆ Ｉ Ｇ  Ｇ Ｅ Ｎ Ｅ Ｒ Ａ Ｔ Ｅ"
        step7 = "Ｓ Ｙ Ｓ Ｔ Ｅ Ｍ  Ｔ Ｅ Ｓ Ｔ"
        hw_scan = "ハードウェアスキャン中..."
        windows_ok = "Windows 検出"
        mem_scan = "メモリ空間マッピング中..."
        mem_label = "搭載メモリ"
        model_best = "コーディング最強"
        model_great = "高性能コーディング"
        model_min = "最低限動作"
        model_recommend = "16GB以上のメモリを推奨します"
        mem_lack = "メモリ不足"
        mem_lack_min = "最低8GB必要"
        manual_model = "手動指定モデル"
        installed = "インストール済み"
        installing = "インストール中..."
        install_done = "インストール完了"
        install_fail = "インストール失敗"
        install_fail_hint = "手動でインストールしてから再実行してください"
        ollama_starting = "Ollama を起動中..."
        ollama_wait = "Ollama 起動待ち中"
        model_downloading = "モデルをダウンロード中..."
        model_download_hint = "初回はサイズに応じて数分〜数十分かかります"
        model_downloaded = "ダウンロード済み"
        model_dl_done = "ダウンロード完了"
        file_deploy = "ファイルデプロイ中..."
        source_local = "ソース: ローカル"
        source_github = "ソース: GitHub"
        config_gen = "設定ファイル生成中..."
        config_exists = "設定ファイルが既に存在 → 既存設定を保持"
        config_file = "設定ファイル"
        path_added = "PATH 追加"
        path_set = "PATH: 設定済み"
        diag = "システム診断を実行中..."
        online = "ＯＮＬＩＮＥ"
        standby = "ＳＴＡＮＤＢＹ"
        ready = "ＲＥＡＤＹ"
        warning = "ＷＡＲＮＩＮＧ"
        loaded = "ＬＯＡＤＥＤ"
        not_loaded = "未ロード"
        complete = "ＩＮＳＴＡＬＬ  ＣＯＭＰＬＥＴＥ !!"
        usage_label = "使い方:"
        mode_interactive = "対話モード"
        mode_oneshot = "ワンショット"
        mode_auto = "ネットワーク自動判定"
        settings_label = "設定:"
        label_model = "モデル"
        label_config = "設定"
        label_command = "コマンド"
        reopen = "新しいターミナルを開いてから vibe-local を実行"
        enjoy = "無 料 Ａ Ｉ コ ー デ ィ ン グ を 楽 し も う"
    }
    en = @{
        subtitle = "  Ｆ Ｒ Ｅ Ｅ  Ａ Ｉ  Ｃ Ｏ Ｄ Ｉ Ｎ Ｇ  Ｅ Ｎ Ｖ Ｉ Ｒ Ｏ Ｎ Ｍ Ｅ Ｎ Ｔ"
        tagline = "No Network . Totally Free . Local AI Coding"
        step1 = "Ｓ Ｙ Ｓ Ｔ Ｅ Ｍ  Ｓ Ｃ Ａ Ｎ"
        step2 = "Ｍ Ｅ Ｍ Ｏ Ｒ Ｙ  Ａ Ｎ Ａ Ｌ Ｙ Ｓ Ｉ Ｓ"
        step3 = "Ｐ Ａ Ｃ Ｋ Ａ Ｇ Ｅ  Ｉ Ｎ Ｓ Ｔ Ａ Ｌ Ｌ"
        step4 = "Ａ Ｉ  Ｍ Ｏ Ｄ Ｅ Ｌ  Ｄ Ｏ Ｗ Ｎ Ｌ Ｏ Ａ Ｄ"
        step5 = "Ｆ Ｉ Ｌ Ｅ  Ｄ Ｅ Ｐ Ｌ Ｏ Ｙ"
        step6 = "Ｃ Ｏ Ｎ Ｆ Ｉ Ｇ  Ｇ Ｅ Ｎ Ｅ Ｒ Ａ Ｔ Ｅ"
        step7 = "Ｓ Ｙ Ｓ Ｔ Ｅ Ｍ  Ｔ Ｅ Ｓ Ｔ"
        hw_scan = "Scanning hardware..."
        windows_ok = "Windows detected"
        mem_scan = "Mapping memory space..."
        mem_label = "System memory"
        model_best = "Best for coding"
        model_great = "Great for coding"
        model_min = "Minimum viable"
        model_recommend = "16GB+ RAM recommended"
        mem_lack = "Insufficient memory"
        mem_lack_min = "Minimum 8GB required"
        manual_model = "Manual model"
        installed = "installed"
        installing = "Installing..."
        install_done = "installed"
        install_fail = "install failed"
        install_fail_hint = "Please install manually, then re-run this script"
        ollama_starting = "Starting Ollama..."
        ollama_wait = "Waiting for Ollama"
        model_downloading = "Downloading model..."
        model_download_hint = "First download may take several minutes depending on size"
        model_downloaded = "already downloaded"
        model_dl_done = "download complete"
        file_deploy = "Deploying files..."
        source_local = "Source: local"
        source_github = "Source: GitHub"
        config_gen = "Generating config..."
        config_exists = "Config exists -> keeping current settings"
        config_file = "Config file"
        path_added = "PATH added"
        path_set = "PATH: already set"
        diag = "Running system diagnostics..."
        online = "ＯＮＬＩＮＥ"
        standby = "ＳＴＡＮＤＢＹ"
        ready = "ＲＥＡＤＹ"
        warning = "ＷＡＲＮＩＮＧ"
        loaded = "ＬＯＡＤＥＤ"
        not_loaded = "not loaded"
        complete = "ＩＮＳＴＡＬＬ  ＣＯＭＰＬＥＴＥ !!"
        usage_label = "Usage:"
        mode_interactive = "Interactive mode"
        mode_oneshot = "One-shot"
        mode_auto = "Auto-detect network"
        settings_label = "Settings:"
        label_model = "Model"
        label_config = "Config"
        label_command = "Command"
        reopen = "Open a new terminal, then run vibe-local"
        enjoy = "Ｅ Ｎ Ｊ Ｏ Ｙ  Ｆ Ｒ Ｅ Ｅ  Ａ Ｉ  Ｃ Ｏ Ｄ Ｉ Ｎ Ｇ"
    }
    zh = @{
        subtitle = "  免 费 Ａ Ｉ 编 程 环 境"
        tagline = "无需网络 ・ 完全免费 ・ 本地AI编程"
        step1 = "Ｓ Ｙ Ｓ Ｔ Ｅ Ｍ  Ｓ Ｃ Ａ Ｎ"
        step2 = "Ｍ Ｅ Ｍ Ｏ Ｒ Ｙ  Ａ Ｎ Ａ Ｌ Ｙ Ｓ Ｉ Ｓ"
        step3 = "Ｐ Ａ Ｃ Ｋ Ａ Ｇ Ｅ  Ｉ Ｎ Ｓ Ｔ Ａ Ｌ Ｌ"
        step4 = "Ａ Ｉ  Ｍ Ｏ Ｄ Ｅ Ｌ  Ｄ Ｏ Ｗ Ｎ Ｌ Ｏ Ａ Ｄ"
        step5 = "Ｆ Ｉ Ｌ Ｅ  Ｄ Ｅ Ｐ Ｌ Ｏ Ｙ"
        step6 = "Ｃ Ｏ Ｎ Ｆ Ｉ Ｇ  Ｇ Ｅ Ｎ Ｅ Ｒ Ａ Ｔ Ｅ"
        step7 = "Ｓ Ｙ Ｓ Ｔ Ｅ Ｍ  Ｔ Ｅ Ｓ Ｔ"
        hw_scan = "扫描硬件中..."
        windows_ok = "检测到 Windows"
        mem_scan = "内存空间映射中..."
        mem_label = "系统内存"
        model_best = "编程最强"
        model_great = "高性能编程"
        model_min = "最低限运行"
        model_recommend = "推荐16GB以上内存"
        mem_lack = "内存不足"
        mem_lack_min = "最少需要8GB"
        manual_model = "手动指定模型"
        installed = "已安装"
        installing = "安装中..."
        install_done = "安装完成"
        install_fail = "安装失败"
        install_fail_hint = "请手动安装后重新运行此脚本"
        ollama_starting = "正在启动 Ollama..."
        ollama_wait = "等待 Ollama 启动"
        model_downloading = "下载模型中..."
        model_download_hint = "首次下载可能需要几分钟到几十分钟"
        model_downloaded = "已下载"
        model_dl_done = "下载完成"
        file_deploy = "部署文件中..."
        source_local = "来源: 本地"
        source_github = "来源: GitHub"
        config_gen = "生成配置文件中..."
        config_exists = "配置文件已存在 → 保持现有设置"
        config_file = "配置文件"
        path_added = "PATH 已添加"
        path_set = "PATH: 已设置"
        diag = "运行系统诊断..."
        online = "ＯＮＬＩＮＥ"
        standby = "ＳＴＡＮＤＢＹ"
        ready = "ＲＥＡＤＹ"
        warning = "ＷＡＲＮＩＮＧ"
        loaded = "ＬＯＡＤＥＤ"
        not_loaded = "未加载"
        complete = "安 装 完 成 !!"
        usage_label = "使用方法:"
        mode_interactive = "交互模式"
        mode_oneshot = "单次执行"
        mode_auto = "自动检测网络"
        settings_label = "设置:"
        label_model = "模型"
        label_config = "配置"
        label_command = "命令"
        reopen = "打开新终端后运行 vibe-local"
        enjoy = "享 受 免 费 Ａ Ｉ 编 程"
    }
}

function msg { param([string]$key) return $Messages[$Lang][$key] }

# Help
if ($Help) {
    Write-Host "Usage: install.ps1 [-Model MODEL_NAME] [-Lang LANG]"
    Write-Host ""
    Write-Host "  -Model MODEL   Specify Ollama model (e.g. qwen3:8b)"
    Write-Host "  -Lang LANG     Language: ja, en, zh"
    exit 0
}

# ╔══════════════════════════════════════════════════════════════╗
# ║  🎬  ＡＮＩＭＡＴＩＯＮ  ＥＮＧＩＮＥ                    ║
# ╚══════════════════════════════════════════════════════════════╝

function Rainbow-Text {
    param([string]$text)
    $colors = @(46, 47, 48, 49, 50, 51, 45, 39, 33, 27, 21, 57, 93, 129, 165, 201, 200, 199, 198, 197, 196)
    $result = ""
    for ($i = 0; $i -lt $text.Length; $i++) {
        $ci = $i % $colors.Count
        $result += "$(C $colors[$ci])$($text[$i])"
    }
    Write-Host "${result}${NC}"
}

function Vapor-Text {
    param([string]$text)
    $colors = @(51, 87, 123, 159, 195, 189, 183, 177, 171, 165)
    $result = ""
    for ($i = 0; $i -lt $text.Length; $i++) {
        $ci = [math]::Floor($i * $colors.Count / [math]::Max($text.Length, 1)) % $colors.Count
        $result += "$(C $colors[$ci])$($text[$i])"
    }
    Write-Host "${result}${NC}"
}

function Vaporwave-Progress {
    param([string]$label, [int]$durationMs = 2000)
    $width = 40
    $colors = @(198, 199, 207, 213, 177, 171, 165, 129, 93, 57, 51, 50, 49, 48, 47, 46)
    $steps = [math]::Max(20, [math]::Floor($durationMs / 100))
    $sparkles = @("*", "+", "o", ".", "*", "+")
    for ($s = 0; $s -le $steps; $s++) {
        $pct = [math]::Floor($s * 100 / $steps)
        $filled = [math]::Floor($s * $width / $steps)
        $empty = $width - $filled
        $bar = ""
        for ($b = 0; $b -lt $filled; $b++) {
            $ci = [math]::Floor($b * $colors.Count / $width)
            $bar += "$(C $colors[$ci])#"
        }
        for ($b = 0; $b -lt $empty; $b++) {
            $bar += "$(C 237)."
        }
        $si = $s % $sparkles.Count
        Write-Host "`r  $($sparkles[$si]) ${BOLD}${CYAN}$($label.PadRight(30))${NC} |${bar}${NC}| ${BOLD}${NEON_GREEN}$($pct.ToString().PadLeft(3))%${NC} " -NoNewline
        Start-Sleep -Milliseconds ([math]::Floor($durationMs / $steps))
    }
    Write-Host "`r  [OK] ${BOLD}${GREEN}$($label.PadRight(30))${NC} |$($(for($b=0;$b -lt $width;$b++){$ci=[math]::Floor($b*$colors.Count/$width); "$(C $colors[$ci])#"}) -join '')${NC}| ${BOLD}${NEON_GREEN}100%${NC}    "
}

$TOTAL_STEPS = 7

function Step-Header {
    param([int]$num, [string]$title)
    $icons = @(">>>", ">>>", ">>>", ">>>", ">>>", ">>>", ">>>")
    Write-Host ""
    Write-Host "  ${CYAN}=====================================================${NC}"
    Write-Host "  $($icons[$num-1])  ${BOLD}${WHITE}STEP ${num}/${TOTAL_STEPS}${NC}  ${BOLD}${WHITE}${title}${NC}"
    Write-Host "  ${CYAN}=====================================================${NC}"
}

function Vapor-Success { param([string]$msg) Write-Host "  ${NEON_GREEN}|${NC} [OK] ${BOLD}${MINT}${msg}${NC}" }
function Vapor-Info { param([string]$msg) Write-Host "  ${CYAN}|${NC} [i]  ${AQUA}${msg}${NC}" }
function Vapor-Warn { param([string]$msg) Write-Host "  ${ORANGE}|${NC} [!]  ${YELLOW}${msg}${NC}" }
function Vapor-Error { param([string]$msg) Write-Host "  ${RED}|${NC} [X]  ${RED}${BOLD}${msg}${NC}" }

function Run-WithSpinner {
    param([string]$label, [scriptblock]$cmd)
    $job = Start-Job -ScriptBlock $cmd
    $sec = 0
    $sparkles = @("|", "/", "-", "\")
    while ($job.State -eq "Running") {
        $si = $sec % $sparkles.Count
        Write-Host "`r  $($sparkles[$si]) ${BOLD}$($label.PadRight(35))${NC} ${DIM}${GRAY}$([math]::Floor($sec/2))s${NC}  " -NoNewline
        Start-Sleep -Milliseconds 500
        $sec++
    }
    Write-Host "`r$(' ' * 60)`r" -NoNewline
    $result = Receive-Job $job -ErrorAction SilentlyContinue
    $exitOk = $job.State -eq "Completed"
    Remove-Job $job -Force
    return $exitOk
}

# ╔══════════════════════════════════════════════════════════════╗
# ║  🌅  ＴＩＴＬＥ  ＳＣＲＥＥＮ                              ║
# ╚══════════════════════════════════════════════════════════════╝

Clear-Host
Write-Host ""
Write-Host "  ${PINK}##${MAGENTA}##${PURPLE}##${CYAN}##${AQUA}##${MINT}##${NEON_GREEN}##${YELLOW}##${ORANGE}##${CORAL}##${HOT_PINK}##${PINK}##${MAGENTA}##${PURPLE}##${CYAN}##${AQUA}##${NC}"
Write-Host ""
Write-Host "${MAGENTA}${BOLD}"
Write-Host "    ██╗   ██╗██╗██████╗ ███████╗"
Write-Host "    ██║   ██║██║██╔══██╗██╔════╝"
Write-Host "    ██║   ██║██║██████╔╝█████╗"
Write-Host "    ╚██╗ ██╔╝██║██╔══██╗██╔══╝"
Write-Host "     ╚████╔╝ ██║██████╔╝███████╗"
Write-Host "      ╚═══╝  ╚═╝╚═════╝ ╚══════╝"
Write-Host "${NC}${CYAN}${BOLD}"
Write-Host "              ██╗      ██████╗  ██████╗ █████╗ ██╗"
Write-Host "              ██║     ██╔═══██╗██╔════╝██╔══██╗██║"
Write-Host "              ██║     ██║   ██║██║     ███████║██║"
Write-Host "              ██║     ██║   ██║██║     ██╔══██║██║"
Write-Host "              ███████╗╚██████╔╝╚██████╗██║  ██║███████╗"
Write-Host "              ╚══════╝ ╚═════╝  ╚═════╝╚═╝  ╚═╝╚══════╝"
Write-Host "${NC}"
Write-Host "  ${PINK}##${MAGENTA}##${PURPLE}##${CYAN}##${AQUA}##${MINT}##${NEON_GREEN}##${YELLOW}##${ORANGE}##${CORAL}##${HOT_PINK}##${PINK}##${MAGENTA}##${PURPLE}##${CYAN}##${AQUA}##${NC}"
Write-Host ""
Vapor-Text "  $(msg 'subtitle')"
Write-Host ""
Rainbow-Text "  ================================================================"
Write-Host "  ${PINK}##${NC} ${BOLD}${WHITE}$(msg 'tagline')${NC} ${PINK}##${NC}"
Rainbow-Text "  ================================================================"
Write-Host ""
Start-Sleep -Milliseconds 500

# =============================================
# Step 1: OS / Architecture detection
# =============================================
Step-Header 1 (msg 'step1')

$Arch = $env:PROCESSOR_ARCHITECTURE
Vaporwave-Progress (msg 'hw_scan') 1000

Vapor-Info "OS: Windows / Arch: $Arch"

if ($Arch -eq "AMD64" -or $Arch -eq "ARM64") {
    Vapor-Success "$(msg 'windows_ok') ($Arch)"
} else {
    Vapor-Error "Unsupported architecture: $Arch"
    exit 1
}

# =============================================
# Step 2: RAM detection & model auto-select
# =============================================
Step-Header 2 (msg 'step2')

try {
    $TotalMem = (Get-CimInstance Win32_ComputerSystem).TotalPhysicalMemory
    $RamGB = [math]::Floor($TotalMem / 1073741824)
} catch {
    $RamGB = 16
    Vapor-Warn "Could not detect RAM, assuming ${RamGB}GB"
}

Vaporwave-Progress (msg 'mem_scan') 1000

Write-Host "  ${PURPLE}|${NC} ${BOLD}${WHITE}$(msg 'mem_label'): ${NEON_GREEN}${RamGB}GB${NC}"

$SidecarModel = ""

if ($Model) {
    $SelectedModel = $Model
    Vapor-Info "$(msg 'manual_model'): $SelectedModel"
} elseif ($RamGB -ge 32) {
    $SelectedModel = "qwen3-coder:30b"
    $SidecarModel = "qwen3:8b"
    Write-Host "  ${NEON_GREEN}|${NC} ${BOLD}${YELLOW}*** BEST MODEL ***${NC}"
    Write-Host "  ${NEON_GREEN}|${NC}    ${BOLD}${WHITE}${SelectedModel}${NC} ${DIM}(19GB, MoE 3.3B active, $(msg 'model_best'))${NC}"
    Write-Host "  ${NEON_GREEN}|${NC}    ${DIM}+ sidecar: ${SidecarModel} (5GB, fast helper)${NC}"
} elseif ($RamGB -ge 16) {
    $SelectedModel = "qwen3:8b"
    $SidecarModel = "qwen3:1.7b"
    Write-Host "  ${MINT}|${NC} ${BOLD}${CYAN}** GREAT MODEL **${NC}"
    Write-Host "  ${MINT}|${NC}    ${BOLD}${WHITE}${SelectedModel}${NC} ${DIM}(5GB, $(msg 'model_great'))${NC}"
    Write-Host "  ${MINT}|${NC}    ${DIM}+ sidecar: ${SidecarModel} (1.1GB, fast helper)${NC}"
} elseif ($RamGB -ge 8) {
    $SelectedModel = "qwen3:1.7b"
    Vapor-Warn "$SelectedModel (1.1GB, $(msg 'model_min'))"
    Vapor-Warn (msg 'model_recommend')
} else {
    Vapor-Error "$(msg 'mem_lack'): ${RamGB}GB ($(msg 'mem_lack_min'))"
    exit 1
}

# =============================================
# Step 3: Install dependencies
# =============================================
Step-Header 3 (msg 'step3')

# --- Git ---
if (Get-Command git -ErrorAction SilentlyContinue) {
    Vapor-Success "Git $(msg 'installed') ($(git --version 2>&1))"
} else {
    Vapor-Info "Git $(msg 'installing')"
    try {
        winget install -e --id Git.Git --accept-source-agreements --accept-package-agreements 2>&1 | Out-Null
        Vapor-Success "Git $(msg 'install_done')"
    } catch {
        Vapor-Error "Git $(msg 'install_fail')"
        Vapor-Warn "$(msg 'install_fail_hint'): winget install Git.Git"
    }
}

# --- Python ---
$PythonCmd = $null
foreach ($pyCmd in @("py", "python3", "python")) {
    if (Get-Command $pyCmd -ErrorAction SilentlyContinue) {
        $PythonCmd = $pyCmd
        break
    }
}
if ($PythonCmd) {
    $pyVer = & $PythonCmd --version 2>&1
    Vapor-Success "Python $(msg 'installed') ($pyVer)"
} else {
    Vapor-Info "Python $(msg 'installing')"
    try {
        winget install -e --id Python.Python.3.12 --accept-source-agreements --accept-package-agreements 2>&1 | Out-Null
        Vapor-Success "Python $(msg 'install_done')"
        $PythonCmd = "py"
    } catch {
        Vapor-Error "Python $(msg 'install_fail')"
        Vapor-Warn "$(msg 'install_fail_hint'): winget install Python.Python.3.12"
    }
}

# --- Ollama ---
if (Get-Command ollama -ErrorAction SilentlyContinue) {
    $ollamaVer = ollama --version 2>&1
    Vapor-Success "Ollama $(msg 'installed') ($ollamaVer)"
} else {
    Vapor-Info "Ollama $(msg 'installing')"
    try {
        winget install -e --id Ollama.Ollama --accept-source-agreements --accept-package-agreements 2>&1 | Out-Null
        Vapor-Success "Ollama $(msg 'install_done')"
    } catch {
        Vapor-Error "Ollama $(msg 'install_fail')"
        Vapor-Warn "$(msg 'install_fail_hint'): winget install Ollama.Ollama"
    }
}

# --- Claude Code CLI ---
if (Get-Command claude -ErrorAction SilentlyContinue) {
    Vapor-Success "Claude Code CLI $(msg 'installed')"
} else {
    Vapor-Info "Claude Code CLI $(msg 'installing')"
    try {
        # Official Claude Code installer for Windows
        # [SEC] Download to temp file first, then execute (avoid Invoke-Expression on remote content)
        $ccInstaller = Join-Path $env:TEMP "claude-code-install-$(Get-Random).ps1"
        try {
            Invoke-WebRequest -Uri "https://claude.ai/install.ps1" -OutFile $ccInstaller -ErrorAction Stop
            & $ccInstaller 2>&1 | Out-Null
        } finally {
            Remove-Item $ccInstaller -Force -ErrorAction SilentlyContinue
        }
        Vapor-Success "Claude Code CLI $(msg 'install_done')"
    } catch {
        # Fallback to npm
        if (Get-Command npm -ErrorAction SilentlyContinue) {
            try {
                npm install -g @anthropic-ai/claude-code 2>&1 | Out-Null
                Vapor-Success "Claude Code CLI $(msg 'install_done') (via npm)"
            } catch {
                Vapor-Error "Claude Code CLI $(msg 'install_fail')"
                Vapor-Warn "$(msg 'install_fail_hint'): npm install -g @anthropic-ai/claude-code"
            }
        } else {
            Vapor-Error "Claude Code CLI $(msg 'install_fail')"
            Vapor-Warn "Install Node.js first, then: npm install -g @anthropic-ai/claude-code"
        }
    }
}

# =============================================
# Step 4: Model download
# =============================================
Step-Header 4 (msg 'step4')

# Ensure Ollama is running
$ollamaRunning = $false
try {
    $null = Invoke-RestMethod -Uri "http://localhost:11434/api/tags" -TimeoutSec 2 -ErrorAction Stop
    $ollamaRunning = $true
} catch {}

if (-not $ollamaRunning) {
    Vapor-Info (msg 'ollama_starting')
    try {
        Start-Process ollama -ArgumentList "serve" -WindowStyle Hidden
    } catch {
        Vapor-Warn "Could not start Ollama automatically"
    }

    for ($i = 1; $i -le 30; $i++) {
        Write-Host "`r  $(msg 'ollama_wait')... ${i}s " -NoNewline
        Start-Sleep -Seconds 1
        try {
            $null = Invoke-RestMethod -Uri "http://localhost:11434/api/tags" -TimeoutSec 2 -ErrorAction Stop
            $ollamaRunning = $true
            break
        } catch {}
    }
    Write-Host "`r$(' ' * 50)"

    if ($ollamaRunning) {
        Vapor-Success "Ollama $(msg 'online')"
    } else {
        Vapor-Warn "Ollama $(msg 'standby')"
    }
}

# Download model
function Download-Model {
    param([string]$modelName, [string]$label = "")
    try {
        $tags = Invoke-RestMethod -Uri "http://localhost:11434/api/tags" -TimeoutSec 5
        $found = $tags.models | Where-Object { $_.name -eq $modelName }
        if ($found) {
            Vapor-Success "$modelName $(msg 'model_downloaded') $label"
            return $true
        }
    } catch {}

    Write-Host ""
    Write-Host "  ${PINK}##${MAGENTA}##${PURPLE}##${CYAN}##${AQUA}##${MINT}##${NEON_GREEN}##${YELLOW}##${ORANGE}##${CORAL}##${HOT_PINK}##${NC}"
    Write-Host "  ${BOLD}${MAGENTA}  >>  ${WHITE}${modelName} ${CYAN}$(msg 'model_downloading') ${label}${NC}"
    Write-Host "  ${DIM}${AQUA}      $(msg 'model_download_hint')${NC}"
    Write-Host "  ${PINK}##${MAGENTA}##${PURPLE}##${CYAN}##${AQUA}##${MINT}##${NEON_GREEN}##${YELLOW}##${ORANGE}##${CORAL}##${HOT_PINK}##${NC}"
    Write-Host ""

    & ollama pull $modelName
    Write-Host ""

    try {
        $tags2 = Invoke-RestMethod -Uri "http://localhost:11434/api/tags" -TimeoutSec 5
        $found2 = $tags2.models | Where-Object { $_.name -eq $modelName }
        if ($found2) {
            Vapor-Success "$modelName $(msg 'model_dl_done') $label"
            return $true
        }
    } catch {}

    Vapor-Warn "$modelName $(msg 'install_fail') - ollama pull $modelName"
    return $false
}

if (-not (Download-Model $SelectedModel "(main)")) {
    Vapor-Error "Failed to download main model: $SelectedModel"
    Vapor-Warn "Try manually: ollama pull $SelectedModel"
}

if ($SidecarModel -and $SidecarModel -ne $SelectedModel) {
    if (-not (Download-Model $SidecarModel "(sidecar)")) {
        Vapor-Warn "Sidecar model download failed (non-critical): $SidecarModel"
    }
}

# =============================================
# Step 5: File deployment
# =============================================
Step-Header 5 (msg 'step5')

$LibDir = Join-Path $env:USERPROFILE ".local\lib\vibe-local"
$BinDir = Join-Path $env:USERPROFILE ".local\bin"

if (-not (Test-Path $LibDir)) { New-Item -ItemType Directory -Path $LibDir -Force | Out-Null }
if (-not (Test-Path $BinDir)) { New-Item -ItemType Directory -Path $BinDir -Force | Out-Null }

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path

Vaporwave-Progress (msg 'file_deploy') 1500

if ($ScriptDir -and (Test-Path (Join-Path $ScriptDir "anthropic-ollama-proxy.py"))) {
    Vapor-Info (msg 'source_local')
    Copy-Item (Join-Path $ScriptDir "anthropic-ollama-proxy.py") -Destination $LibDir -Force
    Copy-Item (Join-Path $ScriptDir "vibe-local.ps1") -Destination $BinDir -Force
    Copy-Item (Join-Path $ScriptDir "vibe-local.cmd") -Destination $BinDir -Force
} else {
    $RepoRaw = "https://raw.githubusercontent.com/ochyai/vibe-local/main"
    Vapor-Info (msg 'source_github')
    Invoke-WebRequest -Uri "$RepoRaw/anthropic-ollama-proxy.py" -OutFile (Join-Path $LibDir "anthropic-ollama-proxy.py")
    Invoke-WebRequest -Uri "$RepoRaw/vibe-local.ps1" -OutFile (Join-Path $BinDir "vibe-local.ps1")
    Invoke-WebRequest -Uri "$RepoRaw/vibe-local.cmd" -OutFile (Join-Path $BinDir "vibe-local.cmd")
}

Vapor-Success "Proxy -> $LibDir"
Vapor-Success "Command -> $BinDir\vibe-local.cmd"

# =============================================
# Step 6: Config generation
# =============================================
Step-Header 6 (msg 'step6')

$ConfigDir = Join-Path $env:USERPROFILE ".config\vibe-local"
$ConfigFile = Join-Path $ConfigDir "config"

if (-not (Test-Path $ConfigDir)) { New-Item -ItemType Directory -Path $ConfigDir -Force | Out-Null }

Vaporwave-Progress (msg 'config_gen') 1000

if (Test-Path $ConfigFile) {
    Vapor-Warn (msg 'config_exists')
} else {
    $configContent = @"
# vibe-local config
# Auto-generated: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')

MODEL="$SelectedModel"
SIDECAR_MODEL="$SidecarModel"
PROXY_PORT=8082
OLLAMA_HOST="http://localhost:11434"
"@
    Set-Content -Path $ConfigFile -Value $configContent -Encoding UTF8
    Vapor-Success "$(msg 'config_file'): $ConfigFile"
}

# Add to PATH (User environment variable)
$currentPath = [Environment]::GetEnvironmentVariable("PATH", "User")
if ($currentPath -notlike "*$BinDir*") {
    [Environment]::SetEnvironmentVariable("PATH", "$BinDir;$currentPath", "User")
    $env:PATH = "$BinDir;$env:PATH"
    Vapor-Success "$(msg 'path_added') -> $BinDir"
} else {
    Vapor-Success (msg 'path_set')
}

# =============================================
# Step 7: System diagnostics
# =============================================
Step-Header 7 (msg 'step7')

Write-Host ""
Write-Host "  ${CYAN}|${NC} ${BOLD}${WHITE}$(msg 'diag')${NC}"
Write-Host ""

# Ollama
try {
    $null = Invoke-RestMethod -Uri "http://localhost:11434/api/tags" -TimeoutSec 2 -ErrorAction Stop
    Vapor-Success "Ollama Server       -> $(msg 'online')"
} catch {
    Vapor-Warn "Ollama Server       -> $(msg 'standby')"
}

# Proxy quick test
$TestPort = 8083
foreach ($tp in @(8083, 8084, 8085, 8086)) {
    $conn = Get-NetTCPConnection -LocalPort $tp -ErrorAction SilentlyContinue
    if (-not $conn) { $TestPort = $tp; break }
}

# Find python for test
$testPy = $null
foreach ($p in @("py", "python3", "python")) {
    if (Get-Command $p -ErrorAction SilentlyContinue) { $testPy = $p; break }
}

if ($testPy) {
    $testProxyScript = Join-Path $LibDir "anthropic-ollama-proxy.py"
    if ($testPy -eq "py") {
        $testProc = Start-Process -FilePath "py" -ArgumentList "-3 `"$testProxyScript`" $TestPort" -WindowStyle Hidden -PassThru
    } else {
        $testProc = Start-Process -FilePath $testPy -ArgumentList "`"$testProxyScript`" $TestPort" -WindowStyle Hidden -PassThru
    }
    Start-Sleep -Seconds 2
    try {
        $null = Invoke-RestMethod -Uri "http://127.0.0.1:${TestPort}/" -TimeoutSec 2 -ErrorAction Stop
        Vapor-Success "API Proxy           -> $(msg 'online')"
    } catch {
        Vapor-Warn "API Proxy           -> $(msg 'warning')"
    }
    Stop-Process -Id $testProc.Id -Force -ErrorAction SilentlyContinue
}

# Claude Code CLI
if (Get-Command claude -ErrorAction SilentlyContinue) {
    Vapor-Success "Claude Code CLI     -> $(msg 'ready')"
} else {
    Vapor-Warn "Claude Code CLI     -> $(msg 'warning') (restart terminal)"
}

# Model check
try {
    $tags = Invoke-RestMethod -Uri "http://localhost:11434/api/tags" -TimeoutSec 5
    $found = $tags.models | Where-Object { $_.name -eq $SelectedModel }
    if ($found) {
        Vapor-Success "AI Model ($SelectedModel) -> $(msg 'loaded')"
    } else {
        Vapor-Warn "AI Model ($SelectedModel) -> $(msg 'not_loaded')"
    }
    if ($SidecarModel -and $SidecarModel -ne $SelectedModel) {
        $foundSc = $tags.models | Where-Object { $_.name -eq $SidecarModel }
        if ($foundSc) {
            Vapor-Success "Sidecar  ($SidecarModel) -> $(msg 'loaded')"
        } else {
            Vapor-Warn "Sidecar  ($SidecarModel) -> $(msg 'not_loaded')"
        }
    }
} catch {}

# ╔══════════════════════════════════════════════════════════════╗
# ║  🎆  Ｃ Ｏ Ｍ Ｐ Ｌ Ｅ Ｔ Ｅ !!                           ║
# ╚══════════════════════════════════════════════════════════════╝

Write-Host ""
Write-Host ""
Write-Host "  ${PINK}##${MAGENTA}##${PURPLE}##${CYAN}##${AQUA}##${MINT}##${NEON_GREEN}##${YELLOW}##${ORANGE}##${CORAL}##${HOT_PINK}##${PINK}##${MAGENTA}##${PURPLE}##${CYAN}##${AQUA}##${NC}"
Write-Host ""
Rainbow-Text "    =========================================================="
Write-Host ""
Write-Host "          ***  ${BOLD}${MAGENTA}$(msg 'complete')${NC}  ***"
Write-Host ""
Rainbow-Text "    =========================================================="
Write-Host ""
Write-Host "  ${PINK}##${MAGENTA}##${PURPLE}##${CYAN}##${AQUA}##${MINT}##${NEON_GREEN}##${YELLOW}##${ORANGE}##${CORAL}##${HOT_PINK}##${PINK}##${MAGENTA}##${PURPLE}##${CYAN}##${AQUA}##${NC}"
Write-Host ""

Write-Host ""
Rainbow-Text "    ======================================================="
Write-Host ""
Write-Host "    ${BOLD}${WHITE}$(msg 'usage_label')${NC}"
Write-Host ""
Write-Host "    ${PINK}>${NC} ${BOLD}${CYAN}vibe-local${NC}                     ${DIM}$(msg 'mode_interactive')${NC}"
Write-Host "    ${PINK}>${NC} ${BOLD}${CYAN}vibe-local -p `"...`"${NC}            ${DIM}$(msg 'mode_oneshot')${NC}"
Write-Host "    ${PINK}>${NC} ${BOLD}${CYAN}vibe-local -Auto${NC}               ${DIM}$(msg 'mode_auto')${NC}"
Write-Host ""
Rainbow-Text "    ======================================================="
Write-Host ""
Write-Host "    ${BOLD}${WHITE}$(msg 'settings_label')${NC}"
Write-Host "    ${PURPLE}|${NC} $(msg 'label_model'):     ${BOLD}${NEON_GREEN}${SelectedModel}${NC}"
if ($SidecarModel -and $SidecarModel -ne $SelectedModel) {
    Write-Host "    ${PURPLE}|${NC} Sidecar:    ${BOLD}${AQUA}${SidecarModel}${NC}"
}
Write-Host "    ${PURPLE}|${NC} $(msg 'label_config'):       ${AQUA}${ConfigFile}${NC}"
Write-Host "    ${PURPLE}|${NC} $(msg 'label_command'):   ${AQUA}${BinDir}\vibe-local.cmd${NC}"
Write-Host ""
Rainbow-Text "    ======================================================="
Write-Host ""
Write-Host "    ${YELLOW}${BOLD}>>> $(msg 'reopen') <<<${NC}"
Write-Host ""
Write-Host ""

Vapor-Text "    $(msg 'enjoy')"
Write-Host ""
Write-Host ""
