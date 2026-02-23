# vibe-local

```
    ██╗   ██╗██╗██████╗ ███████╗
    ██║   ██║██║██╔══██╗██╔════╝
    ██║   ██║██║██████╔╝█████╗
    ╚██╗ ██╔╝██║██╔══██╗██╔══╝
     ╚████╔╝ ██║██████╔╝███████╗
      ╚═══╝  ╚═╝╚═════╝ ╚══════╝
              ██╗      ██████╗  ██████╗ █████╗ ██╗
              ██║     ██╔═══██╗██╔════╝██╔══██╗██║
              ██║     ██║   ██║██║     ███████║██║
              ██║     ██║   ██║██║     ██╔══██║██║
              ███████╗╚██████╔╝╚██████╗██║  ██║███████╗
              ╚══════╝ ╚═════╝  ╚═════╝╚═╝  ╚═╝╚══════╝
```

> **Free AI Coding Agent — Offline, Local, Open Source**
>
> Python + Ollama only. No API keys. No cloud. No cost.

オフラインのワークショップでAIエージェントを使って学習者をサポートしたり、有料プランに未加入の学生がエージェントコーディングを練習したり、ネットワークのない環境で自然言語を使ってターミナル操作を学んだり――そんな場面を想定した、非営利の研究・教育目的のユーティリティツールです。

Built for offline workshops where instructors support learners with AI agents, for students without paid plans who want to practice agent coding, and for beginners learning terminal operations through natural language — a non-profit research and education utility.

面向离线工作坊中使用AI代理辅助学习者、未订阅付费计划的学生练习代理编程、以及初学者通过自然语言学习终端操作等场景，这是一个非营利性的研究与教育实用工具。

---

## 日本語 | [やさしい日本語](#やさしい-にほんご) | [English](#english) | [中文](#中文)

### これは何？

MacやWindows、LinuxにコマンドをコピペするだけでAIがコードを書いてくれる環境。
ネットワーク不要・完全無料。**Python + Ollama だけで動く**完全OSSのコーディングエージェント。

```
vibe-local → vibe-coder.py (OSS, Python stdlib only) → Ollama (直接通信)
```

ログイン不要・Node.js不要・プロキシプロセス不要。15個の内蔵ツール、サブエージェント、画像・PDF読み取り対応。577テスト。

### インストール (3ステップ)

**1.** ターミナルを開く（Mac: Spotlight `Cmd+Space` → "ターミナル"で検索 / Windows: PowerShellを開く）

**2.** 以下をコピペしてEnter:

*Mac / Linux / Windows(WSL) の場合:*
```bash
curl -fsSL https://raw.githubusercontent.com/ochyai/vibe-local/main/install.sh | bash
```

*Windows (PowerShell) の場合:*
```powershell
Invoke-WebRequest -Uri https://raw.githubusercontent.com/ochyai/vibe-local/main/install.ps1 -UseBasicParsing | Invoke-Expression
```

**3.** 新しいターミナルを開いて起動:

```bash
vibe-local
```

### 使い方

```bash
# 対話モード（AIと会話しながらコーディング）
vibe-local

# ワンショット（1回だけ質問）
vibe-local -p "Pythonでじゃんけんゲーム作って"

# モデルを手動指定
vibe-local --model qwen3:8b
```

### 対応環境

| 環境 | メモリ | メインモデル | サイドカー | 備考 |
|------|--------|-------------|-----------|------|
| Apple Silicon Mac (M1以降) | 32GB+ | qwen3-coder:30b | qwen3:8b | **推奨** |
| Apple Silicon Mac (M1以降) | 16GB | qwen3:8b | qwen3:1.7b | 十分実用的 |
| Apple Silicon Mac (M1以降) | 8GB | qwen3:1.7b | なし | 最低限動作 |
| Intel Mac | 16GB+ | qwen3:8b | qwen3:1.7b | 動作するが遅め |
| Windows (ネイティブ) | 16GB+ | qwen3:8b | qwen3:1.7b | NVIDIA GPU推奨 |
| Windows (WSL2) | 16GB+ | qwen3:8b | qwen3:1.7b | NVIDIA GPU推奨 |
| Linux (x86_64/arm64) | 16GB+ | qwen3:8b | qwen3:1.7b | NVIDIA GPU推奨 |

> サイドカーモデル = 権限チェックや初期化プローブなど軽量タスク用。自動選択されます。

### トラブルシューティング

<details>
<summary>よくある問題と解決法</summary>

**"ollama が起動できませんでした"**
```bash
open -a Ollama        # macOS
ollama serve          # Linux / Windows
```

**"モデルが見つかりません"**
```bash
ollama pull qwen3:8b
```

**"vibe-coder.py が見つかりません"**
```bash
# 再インストール
curl -fsSL https://raw.githubusercontent.com/ochyai/vibe-local/main/install.sh | bash
```

**モデルを変更したい**
```bash
nano ~/.config/vibe-local/config
# MODEL="qwen3:8b" を変更
# SIDECAR_MODEL="qwen3:1.7b"  # 軽量タスク用（省略可・自動選択）
```

**デバッグログを確認したい**
```bash
VIBE_LOCAL_DEBUG=1 vibe-local
```

</details>

---

## やさしい にほんご

### これは なに？

Mac（まっく）や Windows（ういんどうず）で、AI（えーあい）が コードを かいて くれる どうぐ です。
インターネットが なくても つかえます。おかねも かかりません。

### いれかた（3つの ステップ）

**1.** ターミナルを ひらく（Mac: `Cmd+Space` → 「ターミナル」 / Windows: PowerShellを ひらく）

**2.** したの もじを コピーして、はりつけて、Enterを おす：

*Mac / Linux / Windows(WSL) のとき:*
```bash
curl -fsSL https://raw.githubusercontent.com/ochyai/vibe-local/main/install.sh | bash
```

*Windows (PowerShell) のとき:*
```powershell
Invoke-WebRequest -Uri https://raw.githubusercontent.com/ochyai/vibe-local/main/install.ps1 -UseBasicParsing | Invoke-Expression
```

**3.** あたらしい ターミナルを ひらいて、これを うつ：

```bash
vibe-local
```

### つかいかた

```bash
# AIと はなしながら プログラムを つくる
vibe-local

# 1かいだけ しつもんする
vibe-local -p "Pythonで じゃんけんゲームを つくって"
```

### たいわ コマンド（はなしている ときに つかえる めいれい）

| コマンド | なにを する？ |
|---|---|
| `/help` | つかえる コマンドを みる |
| `/exit` または `/quit` | おわる（セッションを ほぞんする） |
| `/clear` | かいわを けす |
| `/model <なまえ>` | モデルを かえる |
| `/status` | いまの じょうほうを みる |
| `/save` | セッションを ほぞんする |
| `/compact` | かいわを みじかくする（メモリ せつやく） |
| `/yes` | じどう きょか モード オン |
| `"""` | ながい ぶんしょうを にゅうりょく する |
| `Ctrl+C` | とめる / おわる |

### きをつけること

> **だいじ：AIが あぶない コマンドを うつことが あります！**

AIは かんぺきでは ありません。まちがった コマンドを うつことが あります。

**きけんな サイン — こんな コマンドは ゆるさないで！**

| きけんな キーワード | なぜ あぶない？ |
|---|---|
| `sudo` で はじまる | パソコンの だいじな せっていが かわる |
| `chmod` が はいっている | ファイルの まもりが なくなる |
| いみが わからない ながい コマンド | なにが おきるか わからない！ |

**あんぜんに つかう ほうほう：**

- はじめて つかうときは、しつもんに **`n`** を おして ください（あんぜんモード）
- AIが コマンドを うつまえに、「これを うっていい？」と きいてきます
- わからない コマンドは **ぜったいに ゆるさないで ください**
- だいじな ファイルが ある フォルダでは つかわないで ください
- こまったら、`Ctrl+C` で とめられます

---

## English

### What is this?

A free AI coding environment you can set up with a single command on your Mac, Windows, or Linux.
No network required. Completely free. **Python + Ollama only** — a fully open-source coding agent.

```
vibe-local → vibe-coder.py (OSS, Python stdlib only) → Ollama (direct)
```

No login. No Node.js. No proxy process. 15 built-in tools, sub-agents, image/PDF reading. 577 tests.

### Install (3 steps)

**1.** Open Terminal (Mac: Spotlight `Cmd+Space` → search "Terminal" / Windows: Open PowerShell)

**2.** Paste and hit Enter:

*For Mac / Linux / Windows(WSL):*
```bash
curl -fsSL https://raw.githubusercontent.com/ochyai/vibe-local/main/install.sh | bash
```

*For Windows (PowerShell natively):*
```powershell
Invoke-WebRequest -Uri https://raw.githubusercontent.com/ochyai/vibe-local/main/install.ps1 -UseBasicParsing | Invoke-Expression
```

**3.** Open a new terminal and run:

```bash
vibe-local
```

### Usage

```bash
# Interactive mode (chat with AI while coding)
vibe-local

# One-shot (ask once)
vibe-local -p "Create a snake game in Python"

# Specify model manually
vibe-local --model qwen3:8b
```

### Supported Environments

| Environment | RAM | Main Model | Sidecar | Notes |
|-------------|-----|------------|---------|-------|
| Apple Silicon Mac (M1+) | 32GB+ | qwen3-coder:30b | qwen3:8b | **Recommended** |
| Apple Silicon Mac (M1+) | 16GB | qwen3:8b | qwen3:1.7b | Very capable |
| Apple Silicon Mac (M1+) | 8GB | qwen3:1.7b | none | Minimum viable |
| Intel Mac | 16GB+ | qwen3:8b | qwen3:1.7b | Works but slower |
| Windows (Native) | 16GB+ | qwen3:8b | qwen3:1.7b | NVIDIA GPU recommended |
| Windows (WSL2) | 16GB+ | qwen3:8b | qwen3:1.7b | NVIDIA GPU recommended |
| Linux (x86_64/arm64) | 16GB+ | qwen3:8b | qwen3:1.7b | NVIDIA GPU recommended |

> Sidecar model = auto-selected lighter model for permission checks, init probes, and short summaries.

### Troubleshooting

<details>
<summary>Common issues and solutions</summary>

**"ollama failed to start"**
```bash
open -a Ollama        # macOS
ollama serve          # Linux / Windows
```

**"model not found"**
```bash
ollama pull qwen3:8b
```

**"vibe-coder.py not found"**
```bash
# Reinstall
curl -fsSL https://raw.githubusercontent.com/ochyai/vibe-local/main/install.sh | bash
```

**Change model**
```bash
nano ~/.config/vibe-local/config
# Change MODEL="qwen3:8b"
# SIDECAR_MODEL="qwen3:1.7b"  # For lightweight tasks (optional, auto-selected)
```

**Enable debug logging**
```bash
VIBE_LOCAL_DEBUG=1 vibe-local
```

</details>

---

## 中文

### 这是什么？

在Mac、Windows 或 Linux上只需复制粘贴一个命令，AI就能帮你写代码。
无需网络，完全免费。**Python + Ollama** 打造的完全开源编程代理。

```
vibe-local → vibe-coder.py (开源, 纯Python标准库) → Ollama (直接通信)
```

无需登录、无需Node.js、无需代理进程。15个内置工具、子代理、图像/PDF读取支持。577项测试。

### 安装（3步）

**1.** 打开终端（Mac: Spotlight `Cmd+Space` → 搜索"终端" / Windows: 打开 PowerShell）

**2.** 粘贴以下命令并按回车：

*Mac / Linux / Windows(WSL) 环境:*
```bash
curl -fsSL https://raw.githubusercontent.com/ochyai/vibe-local/main/install.sh | bash
```

*Windows (PowerShell) 环境:*
```powershell
Invoke-WebRequest -Uri https://raw.githubusercontent.com/ochyai/vibe-local/main/install.ps1 -UseBasicParsing | Invoke-Expression
```

**3.** 打开新终端并运行：

```bash
vibe-local
```

### 使用方法

```bash
# 交互模式（与AI对话编程）
vibe-local

# 单次执行（只问一次）
vibe-local -p "用Python写一个贪吃蛇游戏"

# 手动指定模型
vibe-local --model qwen3:8b
```

### 支持的环境

| 环境 | 内存 | 主模型 | 边车模型 | 备注 |
|------|------|--------|---------|------|
| Apple Silicon Mac (M1及以上) | 32GB+ | qwen3-coder:30b | qwen3:8b | **推荐** |
| Apple Silicon Mac (M1及以上) | 16GB | qwen3:8b | qwen3:1.7b | 足够实用 |
| Apple Silicon Mac (M1及以上) | 8GB | qwen3:1.7b | 无 | 最低限运行 |
| Intel Mac | 16GB+ | qwen3:8b | qwen3:1.7b | 可运行但较慢 |
| Windows (原生) | 16GB+ | qwen3:8b | qwen3:1.7b | 推荐NVIDIA GPU |
| Windows (WSL2) | 16GB+ | qwen3:8b | qwen3:1.7b | 推荐NVIDIA GPU |
| Linux (x86_64/arm64) | 16GB+ | qwen3:8b | qwen3:1.7b | 推荐NVIDIA GPU |

> 边车模型 = 用于权限检查、初始化探测等轻量任务的自动选择的较小模型。

### 故障排除

<details>
<summary>常见问题及解决方法</summary>

**"ollama 无法启动"**
```bash
open -a Ollama        # macOS
ollama serve          # Linux / Windows
```

**"未找到模型"**
```bash
ollama pull qwen3:8b
```

**"vibe-coder.py 未找到"**
```bash
# 重新安装
curl -fsSL https://raw.githubusercontent.com/ochyai/vibe-local/main/install.sh | bash
```

**更换模型**
```bash
nano ~/.config/vibe-local/config
# 修改 MODEL="qwen3:8b"
# SIDECAR_MODEL="qwen3:1.7b"  # 轻量任务用（可选，自动选择）
```

**启用调试日志**
```bash
VIBE_LOCAL_DEBUG=1 vibe-local
```

</details>

---

## Architecture

```
┌────────────────────────────────────────────────────────────┐
│  User                                                      │
│  └── vibe-local.sh / vibe-local.ps1 (launch script)       │
│       ├── Ensure Ollama is running                         │
│       └── Launch vibe-coder.py (direct, no proxy)          │
└────────────────────────┬───────────────────────────────────┘
                         │
                         ▼
┌────────────────────────────────────────────────────────────┐
│  vibe-coder.py  (single file, Python stdlib only, ~5200L)  │
│                                                            │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  Agent Loop (parallel tool execution)                │  │
│  │  User input → LLM → Tool calls → Execute →          │  │
│  │  Add results → Loop until done                       │  │
│  ├──────────────────────────────────────────────────────┤  │
│  │  15 Built-in Tools                                   │  │
│  │  Bash (+ background), Read (+ images/PDF/ipynb),     │  │
│  │  Write, Edit (+ rich diff), Glob, Grep,              │  │
│  │  WebFetch, WebSearch, NotebookEdit, SubAgent,        │  │
│  │  TaskCreate/List/Get/Update, AskUserQuestion         │  │
│  ├──────────────────────────────────────────────────────┤  │
│  │  System Prompt + OS-Specific Hints                   │  │
│  │  macOS: brew, /Users/, system_profiler               │  │
│  │  Linux: apt, /home/                                  │  │
│  │  Windows: winget, %USERPROFILE%                      │  │
│  ├──────────────────────────────────────────────────────┤  │
│  │  XML Tool Call Fallback (Qwen model compatibility)   │  │
│  │  Permission Manager (safe / ask / deny tiers)        │  │
│  │  Session Persistence (JSONL) + Context Compaction    │  │
│  │  TUI (readline, ANSI colors, markdown rendering)     │  │
│  │  Multimodal (image base64 → Ollama vision models)    │  │
│  └──────────────────────┬───────────────────────────────┘  │
└─────────────────────────┼──────────────────────────────────┘
                          │  OpenAI Chat API (/v1/chat/completions)
                          ▼
┌────────────────────────────────────────────────────────────┐
│  Ollama (localhost:11434)                                   │
│  Local LLM inference runtime                                │
│  qwen3-coder:30b / qwen3:8b / qwen3:1.7b / ...            │
└────────────────────────────────────────────────────────────┘
```

---

## Comparison with Similar Projects / 類似プロジェクトとの比較

AIコーディングエージェントの分野には、素晴らしいオープンソースプロジェクトが数多く存在します。それぞれ異なる哲学とユースケースに基づいて設計されており、vibe-local もその一つとして、**研究・教育**という特定のニッチに焦点を当てています。

There are many excellent open-source projects in the AI coding agent space. Each is built with a different philosophy and use case in mind. vibe-local contributes to this ecosystem by focusing specifically on **research and education**.

| | [**aider**](https://github.com/Aider-AI/aider) | [**opencode**](https://github.com/opencode-ai/opencode) | [**Cline**](https://github.com/cline/cline) | [**Codex CLI**](https://github.com/openai/codex) | [**Gemini CLI**](https://github.com/google-gemini/gemini-cli) | [**Goose**](https://github.com/block/goose) | **vibe-local** |
|---|---|---|---|---|---|---|---|
| Language | Python | Go | TypeScript | Rust | TypeScript | Rust + TS | Python (stdlib only) |
| External deps | ~100+ pip pkgs | Go modules | VS Code + npm | Node.js | Node.js | Cargo crates | **0** |
| Local LLM | Yes (many backends) | Yes (config) | Yes (providers) | No | No | Yes | Yes (Ollama native) |
| API key required | Yes (or local) | Yes (or local) | Yes (or local) | Yes (OpenAI) | Yes (Google) | Yes (or local) | **No** |
| Install | `pip install` | `go install` / brew | VS Code marketplace | `npm install` | `npm install` | Binary / installer | `curl \| bash` |
| Interface | Terminal | Terminal (rich TUI) | VS Code | Terminal | Terminal | Terminal + Desktop | Terminal |
| Strength | Git-aware, multi-model | Beautiful TUI, speed | Deep IDE integration | OpenAI ecosystem | Google ecosystem | Extensible, MCP | Simplicity, education |
| License | Apache 2.0 | MIT | Apache 2.0 | Apache 2.0 | Apache 2.0 | Apache 2.0 | MIT |

> **aider** is one of the most mature CLI tools, with excellent git integration and multi-model support. **opencode** stands out with its beautiful Bubble Tea TUI and fast Go implementation. **Cline** provides deep VS Code integration that feels native. **Codex CLI** and **Gemini CLI** bring the power of OpenAI and Google ecosystems respectively. **Goose** (by Block) offers an extensible MCP-based agent framework. These are all excellent tools built by talented teams — if you're a professional developer, you should try them.
>
> **aider** はgit統合とマルチモデル対応で最も成熟したCLIツールの一つ。**opencode** は美しいBubble Tea TUIと高速なGo実装が特徴。**Cline** はVS Codeとのネイティブな統合を提供。**Codex CLI**・**Gemini CLI** はOpenAI/Googleエコシステムの力を活用。**Goose** (Block社) はMCPベースの拡張可能なエージェントフレームワーク。いずれも才能あるチームが作った素晴らしいツールです。プロの開発者の方はぜひ試してみてください。
>
> vibe-local は別のアプローチを取ります：**1ファイル、外部依存ゼロ、Python標準ライブラリのみ**。プロの開発者向けではなく、「AIエージェントの仕組みを内側から学びたい」「オフラインの教室で使いたい」「ソースコードを午後1回で全部読みたい」という人のために作りました。

### Why vibe-local? / なぜ vibe-local？

**For educators and researchers / 教育者・研究者のために：**

- **Zero setup friction / セットアップの摩擦ゼロ** — `curl | bash` で全て完了。pip install も npm も venv も不要。学生はコマンド1つでAIコーディングを開始できます。
- **Single file, readable source / 1ファイル、読めるソース** — `vibe-coder.py` は外部依存ゼロの単一ファイル。AIエージェント、ツール使用、プロンプトエンジニアリングの授業教材として最適です。
- **Fully offline / 完全オフライン** — インターネットのない教室、飛行機、地方でも動作。モデルを事前DLしてUSBで配布可能。
- **Pure Python stdlib / 純粋なPython標準ライブラリ** — C拡張なし、コンパイル済みバイナリなし、仮想環境不要。Python 3.8+ と Ollama があれば動きます。
- **Research-friendly / 研究しやすい** — 単一ファイル設計により、エージェント行動、ツール使用パターン、LLM性能の実験・計測・改変が容易です。

> If you're a professional developer looking for the best coding assistant, check out [aider](https://github.com/Aider-AI/aider), [opencode](https://github.com/opencode-ai/opencode), [Cline](https://github.com/cline/cline), or [Goose](https://github.com/block/goose) — they are all excellent tools built by talented communities. If you're an educator, researcher, or student who wants to understand how AI coding agents work from the inside, or need something that runs offline with zero dependencies, vibe-local is for you.
>
> プロの開発者で最高のコーディングアシスタントを探している方は、[aider](https://github.com/Aider-AI/aider)、[opencode](https://github.com/opencode-ai/opencode)、[Cline](https://github.com/cline/cline)、[Goose](https://github.com/block/goose) をお勧めします。いずれも素晴らしいコミュニティによって作られた優れたツールです。AIコーディングエージェントの仕組みを内側から理解したい教育者・研究者・学生の方、またはオフラインで依存関係ゼロで動くものが必要な方には、vibe-local があります。

---

## CLI Reference

### CLI Flags

| Flag | Short | Description | 説明 | 说明 |
|------|-------|-------------|------|------|
| `--prompt` | `-p` | One-shot prompt (non-interactive) | ワンショットプロンプト | 单次提示 |
| `--model` | `-m` | Specify Ollama model name | Ollamaモデル名を指定 | 指定Ollama模型 |
| `--yes` | `-y` | Auto-approve all tool calls | 全ツール自動許可 | 自动批准所有工具 |
| `--debug` | | Enable debug logging | デバッグログ有効化 | 启用调试日志 |
| `--resume` | | Resume last session | 最後のセッション再開 | 恢复上一个会话 |
| `--session-id <id>` | | Resume specific session | 指定セッション再開 | 恢复特定会话 |
| `--list-sessions` | | List saved sessions | セッション一覧 | 列出会话 |
| `--ollama-host <url>` | | Ollama API endpoint | Ollamaエンドポイント | Ollama API端点 |
| `--max-tokens <n>` | | Max output tokens (default: 8192) | 最大出力トークン数 | 最大输出令牌数 |
| `--temperature <f>` | | Sampling temperature (default: 0.7) | サンプリング温度 | 采样温度 |
| `--context-window <n>` | | Context window size (default: 32768) | コンテキストウィンドウ | 上下文窗口 |
| `--version` | | Show version and exit | バージョン表示 | 显示版本 |

### Interactive Commands

| Command | Description | 説明 | 说明 |
|---------|-------------|------|------|
| `/help` | Show commands | コマンド一覧 | 显示命令 |
| `/exit`, `/quit`, `/q` | Exit (auto-saves) | 終了（自動保存） | 退出（自动保存） |
| `/clear` | Clear history | 履歴クリア | 清除历史 |
| `/model <name>` | Switch model | モデル切替 | 切换模型 |
| `/models` | List installed models with tiers | モデル一覧（ティア表示） | 模型列表（层级） |
| `/status` | Session info | セッション情報 | 会话信息 |
| `/save` | Save session | セッション保存 | 保存会话 |
| `/compact` | Compress history | 履歴圧縮 | 压缩历史 |
| `/tokens` | Token usage | トークン使用量 | 令牌使用量 |
| `/undo` | Undo last write/edit | 最後の書き込みを元に戻す | 撤销上次写入 |
| `/config` | Show config | 設定表示 | 显示配置 |
| `/commit` | Git stage + commit | gitコミット | git提交 |
| `/diff` | Show git diff | git diff表示 | 显示git diff |
| `/git <cmd>` | Run git subcommand | gitサブコマンド | 运行git子命令 |
| `/plan` | Plan mode (read-only) | プランモード | 计划模式 |
| `/execute` | Exit plan mode | プランモード終了 | 退出计划模式 |
| `/init` | Create CLAUDE.md | CLAUDE.md作成 | 创建CLAUDE.md |
| `/yes` | Enable auto-approve | 自動許可ON | 启用自动批准 |
| `exit`, `quit`, `bye` | Exit (no `/` needed) | 終了 | 退出 |
| `"""` | Multi-line input | 複数行入力 | 多行输入 |
| `Ctrl+C` | Stop (double-tap to exit) | 停止（2回で終了） | 停止（连按退出） |

---

## Configuration

### Config File

```bash
~/.config/vibe-local/config
```

Format: `KEY="value"`. Lines starting with `#` are comments.

| Key | Default | Description |
|-----|---------|-------------|
| `MODEL` | auto (by RAM) | Main model name |
| `SIDECAR_MODEL` | auto (by RAM) | Sidecar model (lighter, for compaction etc.) |
| `OLLAMA_HOST` | `http://localhost:11434` | Ollama API endpoint |
| `MAX_TOKENS` | `8192` | Max output tokens per response |
| `TEMPERATURE` | `0.7` | Sampling temperature |
| `CONTEXT_WINDOW` | `32768` | Context window size in tokens |

Example:
```bash
# ~/.config/vibe-local/config
MODEL="qwen3:8b"
SIDECAR_MODEL="qwen3:1.7b"
OLLAMA_HOST="http://localhost:11434"
```

### Model Tiers

vibe-local auto-detects installed Ollama models and picks the best one for your RAM. Use `/models` to see tiers.

| Tier | RAM (practical) | Models | Quality | Speed |
|------|-----------------|--------|---------|-------|
| **S** Frontier | 768GB+ | `deepseek-r1:671b`, `deepseek-v3:671b` | Best reasoning | Slow |
| **A** Expert | 256GB+ | `qwen3:235b`, `llama3.1:405b` | Excellent | Moderate |
| **B** Advanced | 96GB+ | `llama3.3:70b`, `mixtral:8x22b` | Very strong | Good |
| **C** Solid | 16GB+ | `qwen3-coder:30b`, `qwen2.5-coder:32b` | Good balance | Fast |
| **D** Light | 8GB+ | `qwen3:8b`, `llama3.1:8b` | Decent | Very fast |
| **E** Minimal | 4GB+ | `qwen3:1.7b`, `llama3.2:3b` | Basic | Instant |

> RAM column shows practical minimum for interactive use (model + KV cache + OS). 671B models are not auto-selected on 512GB machines — use `MODEL=` to force.

### Environment Variables

Priority: CLI flags > Environment variables > Config file > Defaults

| Variable | Description |
|----------|-------------|
| `OLLAMA_HOST` | Ollama API endpoint |
| `VIBE_CODER_MODEL` | Override main model (highest priority) |
| `VIBE_LOCAL_MODEL` | Main model (set by launcher) |
| `VIBE_CODER_SIDECAR` | Override sidecar model |
| `VIBE_LOCAL_SIDECAR_MODEL` | Sidecar model (set by launcher) |
| `VIBE_CODER_DEBUG` / `VIBE_LOCAL_DEBUG` | Set to `1` for debug logging |

---

## Security

> **Use this tool at your own risk. Pay attention to the commands the AI executes.**

vibe-local offers normal mode (confirms each action) and auto-approve mode (`-y`).
**Local LLMs are less accurate than cloud AI — they may attempt dangerous operations unintentionally.**

### Watch for these keywords

| Keyword | Risk |
|---|---|
| `sudo` | Admin privileges — affects entire system |
| `chmod` / `chown` | Changes file permissions |
| `dd` / `mkfs` / `/dev/` | Direct disk operations |
| `>` overwriting configs | Settings may be erased |
| `--force` | Skips safety checks |
| Long commands you don't understand | If you can't read it, don't allow it |

### Safe usage rules

1. **Choose `n` (normal mode) on first launch** — approve each action
2. **Never allow commands you don't understand**
3. **Practice in a new empty folder**
4. **Reject `sudo` requests**
5. **`Ctrl+C` to stop at any time**

### Built-in Security Mechanisms

| Mechanism | Description |
|-----------|-------------|
| **SAFE_TOOLS vs ASK_TOOLS** | Read/Glob/Grep/SubAgent/TaskTools are auto-approved. Bash/Write/Edit require confirmation. WebFetch/WebSearch need extra context. |
| **SSRF prevention** | OLLAMA_HOST restricted to localhost only |
| **URL scheme validation** | Only http:// and https:// allowed |
| **Session ID sanitization** | Path traversal prevention |
| **Max iteration limit** | Agent loop stops after 50 iterations |
| **Symlink protection** | Refuses to read/write through symlinks |
| **Protected path blocking** | Blocks writes to config/permission files |
| **Dangerous command detection** | Blocks `curl|sh`, `rm -rf /`, `eval base64` patterns |

---

## Workshop Guide

### For instructors / 講師向け

```bash
# 1. Pre-install on venue computers (while online)
curl -fsSL https://raw.githubusercontent.com/ochyai/vibe-local/main/install.sh | bash

# 2. Pre-download models (for offline use)
ollama pull qwen3:8b          # For 16GB machines
ollama pull qwen3-coder:30b   # For 32GB machines (recommended)

# 3. Verify
vibe-local -p "Write Hello World in Python"
```

### Starter exercises / 課題例

```
1. "Create a rock-paper-scissors game in Python"  → Basic programming
2. "List all files in this folder"                 → Terminal operations
3. "Create a timer app in HTML and open it"        → Web development
4. "Create minesweeper in HTML"                    → Game development
5. "Check the current system information"          → OS operations
```

---

## Offline Capabilities

| Feature | Offline | Notes |
|---------|:-------:|-------|
| Code generation & execution | Yes | All processed locally |
| File operations | Yes | |
| Terminal commands | Yes | |
| Git (local) | Yes | push/pull need network |
| HTML app creation | Yes | Opens in browser |
| Web search | Online only | DuckDuckGo |
| URL fetch | Online only | |
| Package install | Online only | pip/brew/winget |

---

## Legal

**What this tool does:**
- Runs `vibe-coder.py`, a fully open-source Python coding agent
- Communicates directly with Ollama (open-source LLM runtime) running locally
- No communication with external servers (Web search/fetch are optional)
- Does not use any Anthropic software

**Licenses:**
- **vibe-coder.py**: MIT License
- **Ollama**: MIT License
- **Qwen3 models**: Apache 2.0 License
- **vibe-local**: MIT License

All components are open-source. This tool is intended for research and education.

---

## Disclaimer

> **This project is NOT affiliated with, endorsed by, or associated with Anthropic.**
> "Claude" is a trademark of Anthropic, PBC. This is an unofficial community tool.
>
> Since v0.3.0, this tool does not use any proprietary software.
> All components (vibe-coder.py, Ollama, Qwen3 models) are open-source licensed.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
> The authors are not liable for any damages arising from the use of this software.
> **Use entirely at your own risk.**

本プロジェクトは Anthropic 社とは一切関係ありません。v0.3.0 以降、プロプライエタリソフトウェアを使用していません。本ソフトウェアは現状有姿（AS IS）で提供され、いかなる保証もありません。

本项目与 Anthropic 公司无任何关联。自v0.3.0起不使用任何专有软件。本软件按"原样"提供，不提供任何保证。

## License

MIT
