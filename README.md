# 🤖⚡ Ｖ Ｉ Ｂ Ｅ  Ｌ Ｏ Ｃ Ａ Ｌ ⚡🤖

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

> 🌴✨ **Free AI Coding Environment** ✨🌴
>
> No network. No cost. Local LLM agent coding.

**🇯🇵** オフラインのワークショップでAIエージェントを使って学習者をサポートしたり、有料プランに未加入の学生がエージェントコーディングを練習したり、ネットワークのない環境で自然言語を使ってターミナル操作を学んだり――そんな場面を想定した、非営利の研究・教育目的のユーティリティツールです。

**🌱** やさしい にほんご：これは、むりょう（おかね いらない）で、AI（えーあい）に プログラムを かいて もらう ための どうぐ です。インターネットが なくても つかえます。がっこう や ワークショップで つかう ために つくりました。

**🇺🇸** Built for offline workshops where instructors support learners with AI agents, for students without paid plans who want to practice agent coding, and for beginners learning terminal operations through natural language — a non-profit research and education utility.

**🇨🇳** 面向离线工作坊中使用AI代理辅助学习者、未订阅付费计划的学生练习代理编程、以及初学者通过自然语言学习终端操作等场景，这是一个非营利性的研究与教育实用工具。

---

## 🇯🇵 日本語 | [🌱 やさしい日本語](#-やさしい-にほんご) | [🇺🇸 English](#-english) | [🇨🇳 中文](#-中文)

### これは何？

MacにコマンドをコピペするだけでAIがコードを書いてくれる環境。
ネットワーク不要・完全無料。Ollama + ローカルLLM で Claude Code のインターフェースをそのまま使える。

### インストール (3ステップ)

**1.** ターミナルを開く（Spotlight `Cmd+Space` → "ターミナル"で検索）

**2.** 以下をコピペしてEnter:

```bash
curl -fsSL https://raw.githubusercontent.com/ochyai/vibe-local/main/install.sh | bash
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

# ネットワーク自動判定（ネットがあればClaude API、なければローカル）
vibe-local --auto

# モデルを手動指定
vibe-local --model qwen3:8b
```

### 対応環境

| 環境 | メモリ | メインモデル | サイドカー | 備考 |
|------|--------|-------------|-----------|------|
| Apple Silicon Mac (M1以降) | 32GB+ | qwen3-coder:30b | qwen3:8b | 🏆 **推奨** |
| Apple Silicon Mac (M1以降) | 16GB | qwen3:8b | qwen3:1.7b | ⭐ 十分実用的 |
| Apple Silicon Mac (M1以降) | 8GB | qwen3:1.7b | なし | 最低限動作 |
| Intel Mac | 16GB+ | qwen3:8b | qwen3:1.7b | 動作するが遅め |
| Linux (x86_64/arm64) | 16GB+ | qwen3:8b | qwen3:1.7b | NVIDIA GPU推奨 |

> サイドカーモデル = 権限チェックや初期化プローブなど軽量タスク用。自動選択されます。

### トラブルシューティング

<details>
<summary>💡 よくある問題と解決法</summary>

**"ollama が起動できませんでした"**
```bash
open -a Ollama        # macOS
ollama serve          # Linux
```

**"モデルが見つかりません"**
```bash
ollama pull qwen3:8b
```

**"claude: command not found"**
```bash
npm install -g @anthropic-ai/claude-code
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
# ログにモデルルーティング情報（sidecar/main）が表示されます
```

</details>

---

## 🌱 やさしい にほんご

### これは なに？

Mac（まっく）で、AI（えーあい）が コードを かいて くれる どうぐ です。
インターネットが なくても つかえます。おかねも かかりません。

### いれかた（3つの ステップ）

**1.** ターミナルを ひらく（`Cmd+Space` → 「ターミナル」で けんさく）

**2.** したの もじを コピーして、はりつけて、Enterを おす：

```bash
curl -fsSL https://raw.githubusercontent.com/ochyai/vibe-local/main/install.sh | bash
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

### きをつけること

> **⚠️ だいじ：AIが あぶない コマンドを うつことが あります！**

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

## 🇺🇸 English

### What is this?

A free AI coding environment you can set up with a single command on your Mac.
No network required. Completely free. Uses Ollama + local LLM with the Claude Code interface.

### Install (3 steps)

**1.** Open Terminal (Spotlight `Cmd+Space` → search "Terminal")

**2.** Paste and hit Enter:

```bash
curl -fsSL https://raw.githubusercontent.com/ochyai/vibe-local/main/install.sh | bash
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

# Auto-detect network (uses Claude API if online, local if offline)
vibe-local --auto

# Specify model manually
vibe-local --model qwen3:8b
```

### Supported Environments

| Environment | RAM | Main Model | Sidecar | Notes |
|-------------|-----|------------|---------|-------|
| Apple Silicon Mac (M1+) | 32GB+ | qwen3-coder:30b | qwen3:8b | 🏆 **Recommended** |
| Apple Silicon Mac (M1+) | 16GB | qwen3:8b | qwen3:1.7b | ⭐ Very capable |
| Apple Silicon Mac (M1+) | 8GB | qwen3:1.7b | none | Minimum viable |
| Intel Mac | 16GB+ | qwen3:8b | qwen3:1.7b | Works but slower |
| Linux (x86_64/arm64) | 16GB+ | qwen3:8b | qwen3:1.7b | NVIDIA GPU recommended |

> Sidecar model = auto-selected lighter model for permission checks, init probes, and short summaries.

### Troubleshooting

<details>
<summary>💡 Common issues and solutions</summary>

**"ollama failed to start"**
```bash
open -a Ollama        # macOS
ollama serve          # Linux
```

**"model not found"**
```bash
ollama pull qwen3:8b
```

**"claude: command not found"**
```bash
npm install -g @anthropic-ai/claude-code
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
# Logs show model routing info — which requests go to main vs sidecar
```

</details>

---

## 🇨🇳 中文

### 这是什么？

在Mac上只需复制粘贴一个命令，AI就能帮你写代码。
无需网络，完全免费。使用 Ollama + 本地大语言模型，享受 Claude Code 的界面体验。

### 安装（3步）

**1.** 打开终端（Spotlight `Cmd+Space` → 搜索"终端"或"Terminal"）

**2.** 粘贴以下命令并按回车：

```bash
curl -fsSL https://raw.githubusercontent.com/ochyai/vibe-local/main/install.sh | bash
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

# 自动检测网络（有网用Claude API，没网用本地）
vibe-local --auto

# 手动指定模型
vibe-local --model qwen3:8b
```

### 支持的环境

| 环境 | 内存 | 主模型 | 边车模型 | 备注 |
|------|------|--------|---------|------|
| Apple Silicon Mac (M1及以上) | 32GB+ | qwen3-coder:30b | qwen3:8b | 🏆 **推荐** |
| Apple Silicon Mac (M1及以上) | 16GB | qwen3:8b | qwen3:1.7b | ⭐ 足够实用 |
| Apple Silicon Mac (M1及以上) | 8GB | qwen3:1.7b | 无 | 最低限运行 |
| Intel Mac | 16GB+ | qwen3:8b | qwen3:1.7b | 可运行但较慢 |
| Linux (x86_64/arm64) | 16GB+ | qwen3:8b | qwen3:1.7b | 推荐NVIDIA GPU |

> 边车模型 = 用于权限检查、初始化探测等轻量任务的自动选择的较小模型。

### 故障排除

<details>
<summary>💡 常见问题及解决方法</summary>

**"ollama 无法启动"**
```bash
open -a Ollama        # macOS
ollama serve          # Linux
```

**"未找到模型"**
```bash
ollama pull qwen3:8b
```

**"claude: command not found"**
```bash
npm install -g @anthropic-ai/claude-code
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
# 日志会显示模型路由信息（主模型/边车模型）
```

</details>

---

## 🔧 Architecture

```
┌─────────────────────────────────────────────────────────┐
│  User                                                   │
│  └─> vibe-local.sh (launch script)                      │
│       ├─ Ensure Ollama is running                       │
│       ├─ Start anthropic-ollama-proxy.py                │
│       ├─ Set ANTHROPIC_BASE_URL → proxy                 │
│       └─ Launch Claude Code CLI                         │
└──────────────────────┬──────────────────────────────────┘
                       │ Anthropic Messages API
                       ▼
┌─────────────────────────────────────────────────────────┐
│  anthropic-ollama-proxy.py                              │
│  ┌────────────────────────────────────────────────────┐ │
│  │ 1. System Prompt Optimizer                         │ │
│  │    - Replace ~15K Claude prompt → ~1K local prompt │ │
│  │    - Extract & inject environment (OS, cwd, shell) │ │
│  │    - Preserve CLAUDE.md user instructions          │ │
│  │    - Add function-calling reinforcement hints      │ │
│  ├────────────────────────────────────────────────────┤ │
│  │ 2. Tool Filter                                     │ │
│  │    - 20+ tools → 9 essential (Bash, Read, Write,   │ │
│  │      Edit, Glob, Grep, WebFetch, WebSearch,        │ │
│  │      NotebookEdit)                                 │ │
│  ├────────────────────────────────────────────────────┤ │
│  │ 3. Model Router                                    │ │
│  │    ┌───────────────────┐  ┌──────────────────────┐ │ │
│  │    │ Main Model        │  │ Sidecar Model        │ │ │
│  │    │ (qwen3-coder:30b) │  │ (qwen3:8b)           │ │ │
│  │    │ - Coding tasks    │  │ - Permission checks  │ │ │
│  │    │ - Tool use        │  │ - Init probes        │ │ │
│  │    │ - Long context    │  │ - haiku/flash/mini   │ │ │
│  │    │ - max_tokens:8192 │  │ - max_tokens:1024    │ │ │
│  │    └───────┬───────────┘  └──────────┬───────────┘ │ │
│  ├────────────┼─────────────────────────┼─────────────┤ │
│  │ 4. API Translation (Anthropic → OpenAI format)     │ │
│  │ 5. XML Tool Call Fallback Parser                   │ │
│  │ 6. SSE Stream Conversion                           │ │
│  └────────────┼─────────────────────────┼─────────────┘ │
└───────────────┼─────────────────────────┼───────────────┘
                │  OpenAI Chat API        │
                ▼                         ▼
┌─────────────────────────────────────────────────────────┐
│  Ollama (localhost:11434)                               │
│  Local LLM inference runtime                            │
└─────────────────────────────────────────────────────────┘
```

### Dual-model routing / デュアルモデル構成 / 双模型架构

The proxy automatically routes requests to two models:
- **Main model** (e.g. `qwen3-coder:30b`): Coding tasks with tool use, long context
- **Sidecar model** (e.g. `qwen3:8b`): Permission checks, init probes, short summaries

Routing rules (checked in order):
1. Model name contains `haiku`/`flash`/`mini` → sidecar
2. `max_tokens==1`, no tools, ≤1 message (init probe) → sidecar
3. Everything else → main model

Debug logs show `(sidecar)` for routed requests: `VIBE_LOCAL_DEBUG=1 vibe-local`

### Configuration / 設定 / 配置

```bash
~/.config/vibe-local/config
```

| Key | Default | Description |
|-----|---------|-------------|
| `MODEL` | auto (by RAM) | Main model name |
| `SIDECAR_MODEL` | auto (by RAM) | Sidecar model name |
| `PROXY_PORT` | 8082 | Proxy listen port |
| `OLLAMA_HOST` | http://localhost:11434 | Ollama API endpoint |

## 🚨 Security / セキュリティ / 安全须知

### 🇯🇵 日本語

> **⚠️ このツールは自己責任でご利用ください。AIが実行するコマンドには注意が必要です。**

`vibe-local` は初回起動時に **ツール自動許可モード** (`--dangerously-skip-permissions`) を使うか確認します。
自動許可モードを選ぶと、AIがファイルの読み書き・コマンド実行・システム操作を **確認なしで** 実行します。

**ローカルLLMはクラウドAIより精度が低いため、意図しない危険な操作を実行するリスクがあります。**

#### こんなコマンドに注意

AIが提案するコマンドの中に以下のキーワードが含まれていたら、**内容を理解できない限り拒否してください：**

| 注意すべきキーワード | リスク |
|---|---|
| `sudo` で始まるコマンド | システム全体に影響する管理者権限での操作 |
| `chmod` / `chown` | ファイルの権限やセキュリティ設定が変わる |
| `dd` / `mkfs` / `/dev/` | ディスクやパーティションを直接操作する |
| `>` で設定ファイルを上書き | 大事な設定が消える |
| `--force` が付いたコマンド | 安全確認をスキップして強制実行する |
| 意味がわからない長いコマンド | 何が起きるかわからない＝許可してはいけない |

#### 安全に使うためのルール

1. **初回起動時は必ず `n`（通常モード）を選択する** — AIの各操作を事前に確認できます
2. **わからないコマンドは許可しない** — 少しでも不安なら `n` で拒否
3. **大事なファイルがあるフォルダでは使わない** — 新しい空フォルダで練習
4. **`sudo` を求められたら基本的に拒否** — ローカルLLMの判断でシステム操作させない
5. **困ったら `Ctrl+C` で停止**

```bash
vibe-local        # 通常モード（推奨）：毎回確認あり
vibe-local -y     # 自動許可モード（上級者向け・自己責任）
```

### 🌱 やさしい にほんご

> **⚠️ だいじな おしらせ：AIは まちがえることが あります！**

AIが うごかそうとする コマンド（めいれい）を よく みてください。
わからない コマンドは、**ぜったいに `y`（はい）を おさないで ください。**

- さいしょに きかれたら **`n`** を おす → AIが まいかい 「これ やっていい？」と きく
- `rm`（さくじょ）や `sudo`（かんりしゃ）が はいった コマンドは きけん
- こまったら **`Ctrl+C`**（コントロール と C を いっしょに おす）で とまる
- れんしゅうは **あたらしい からの フォルダ** で やる

### 🇺🇸 English

> **⚠️ Use this tool at your own risk. Pay attention to the commands the AI executes.**

On first launch, `vibe-local` asks whether to enable **auto-approve mode** (`--dangerously-skip-permissions`).
In auto-approve mode, the AI can read/write files, execute commands, and modify your system **without asking**.

**Local LLMs are less accurate than cloud AI — they may attempt dangerous operations unintentionally.**

#### Watch for these keywords in commands

If a command contains any of these keywords and you don't fully understand it, **always reject:**

| Keyword to watch | Risk |
|---|---|
| Commands starting with `sudo` | Runs with admin privileges — affects entire system |
| `chmod` / `chown` | Changes file permissions and security settings |
| `dd` / `mkfs` / `/dev/` | Directly modifies disks and partitions |
| `>` overwriting config files | Important settings may be erased |
| `--force` flag | Skips safety checks and forces execution |
| Long commands you don't understand | If you can't read it, don't allow it |

#### Rules for safe usage

1. **Always choose `n` (normal mode) on first launch** — you approve each action
2. **Never allow commands you don't understand** — if unsure, reject
3. **Don't use in folders with important files** — practice in a new empty folder
4. **Reject `sudo` requests** — don't let a local LLM run system-level commands
5. **Press `Ctrl+C` to stop at any time**

```bash
vibe-local        # Normal mode (recommended): confirms each action
vibe-local -y     # Auto-approve mode (advanced users only, at your own risk)
```

### 🇨🇳 中文

> **⚠️ 使用本工具风险自负。请注意AI执行的每一个命令。**

首次启动时，`vibe-local` 会询问是否启用 **工具自动批准模式** (`--dangerously-skip-permissions`)。
在自动批准模式下，AI可以读写文件、执行命令、修改系统，**无需确认**。

**本地LLM的精度低于云端AI，可能意外执行危险操作。**

#### 注意以下关键词

如果命令中包含以下关键词且你不完全理解其含义，**务必拒绝：**

| 需注意的关键词 | 风险 |
|---|---|
| 以 `sudo` 开头的命令 | 以管理员权限运行，影响整个系统 |
| `chmod` / `chown` | 更改文件权限和安全设置 |
| `dd` / `mkfs` / `/dev/` | 直接操作磁盘和分区 |
| 用 `>` 覆盖配置文件 | 重要设置可能被清除 |
| 带 `--force` 的命令 | 跳过安全检查强制执行 |
| 看不懂的长命令 | 看不懂 = 不能允许 |

#### 安全使用规则

1. **首次启动必须选择 `n`（普通模式）** — 每次操作前确认
2. **不理解的命令一律拒绝** — 有疑问就按 `n`
3. **不要在有重要文件的文件夹中使用** — 在新的空文件夹中练习
4. **拒绝 `sudo` 请求** — 不要让本地LLM执行系统级命令
5. **随时按 `Ctrl+C` 停止**

```bash
vibe-local        # 普通模式（推荐）：每次操作前确认
vibe-local -y     # 自动批准模式（仅限高级用户，风险自负）
```

---

## ⚙️ Notes

- Local LLM accuracy is lower than Claude API
- First model download takes time (several GB to 20GB)
- Use `vibe-local --auto` to auto-switch to Claude API when online

---

## 📜 Disclaimer / 免責事項 / 免责声明

### 🌱 やさしい にほんご

> **この どうぐは Anthropic（あんそろぴっく）という かいしゃとは かんけい ありません。**
> じぶんの せきにんで つかってください。
> なにか もんだいが おきても、つくった ひとは せきにんを とれません。
> **つかうまえに、せんせいや くわしいひとに そうだん してください。**

### 🇯🇵

> **本プロジェクトは Anthropic 社とは一切関係ありません。**
> Anthropic が提供・推奨・保証するものではありません。
> 「Claude」は Anthropic, PBC の商標です。本プロジェクトは非公式のコミュニティツールです。
>
> 本ツールは Claude Code CLI を非標準の方法で使用しています（ローカルプロキシ経由でサードパーティLLMに接続）。
> Claude Code CLI の利用規約に抵触する可能性があります。利用者は自身で利用規約を確認してください。
>
> 本ソフトウェアは現状有姿（AS IS）で提供され、明示的・暗示的を問わず、いかなる保証もありません。
> 使用によって生じたいかなる損害についても、著者は一切責任を負いません。
> **すべて自己責任でご利用ください。**

### 🇺🇸

> **This project is NOT affiliated with, endorsed by, or associated with Anthropic.**
> "Claude" is a trademark of Anthropic, PBC. This is an unofficial community tool.
>
> This tool uses the Claude Code CLI in a non-standard way (connecting to third-party LLMs via a local proxy).
> This may not comply with the Claude Code CLI's terms of service. Users should review the terms themselves.
>
> Third-party dependencies (Ollama, Qwen models, Node.js, etc.) have their own licenses and terms.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
> The authors are not liable for any damages arising from the use of this software.
> **Use entirely at your own risk.**

### 🇨🇳

> **本项目与 Anthropic 公司无任何关联。**
> 非 Anthropic 提供、推荐或担保。"Claude"是 Anthropic, PBC 的商标。本项目是非官方社区工具。
>
> 本工具以非标准方式使用 Claude Code CLI（通过本地代理连接第三方LLM）。
> 这可能不符合 Claude Code CLI 的服务条款。用户应自行确认相关条款。
>
> 第三方依赖（Ollama、Qwen模型、Node.js等）有各自的许可证和使用条款。
>
> 本软件按"原样"提供，不提供任何明示或暗示的保证。
> 作者不对因使用本软件而产生的任何损害承担责任。
> **使用本工具风险完全自负。**

## 📄 License

MIT
