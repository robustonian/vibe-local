#!/bin/bash
# vibe-local.sh
# ローカルLLM (Ollama) で Claude Code を起動するスクリプト
# Anthropic API → Ollama 変換プロキシを自動管理
#
# NOTE: This project is NOT affiliated with, endorsed by, or associated with Anthropic.
#
# 使い方:
#   vibe-local                    # インタラクティブモード
#   vibe-local -p "質問"          # ワンショット
#   vibe-local --auto             # ネットワーク状況で自動判定
#   vibe-local --model qwen3:8b   # モデル手動指定
#   vibe-local -y                 # パーミッション確認スキップ (自己責任)
#   vibe-local --debug            # デバッグモード (フルリクエスト/レスポンスログ)

# NOTE: set -e を使わない (途中停止を防ぐ)
set -uo pipefail

# --- ディレクトリ初期化 ---
STATE_DIR="${HOME}/.local/state/vibe-local"
mkdir -p "$STATE_DIR" 2>/dev/null || true
chmod 700 "$STATE_DIR" 2>/dev/null || true

# --- 設定読み込み (安全なパーサー) ---
CONFIG_FILE="${HOME}/.config/vibe-local/config"
PROXY_LIB_DIR="${HOME}/.local/lib/vibe-local"
PROXY_SCRIPT="${PROXY_LIB_DIR}/anthropic-ollama-proxy.py"

# デフォルト値
MODEL=""
SIDECAR_MODEL=""
OLLAMA_HOST="http://localhost:11434"
PROXY_PORT=8082
VIBE_LOCAL_DEBUG=0

# [C1 fix] source ではなく grep で既知キーのみ安全に読む
if [ -f "$CONFIG_FILE" ]; then
    _val() { grep -E "^${1}=" "$CONFIG_FILE" 2>/dev/null | head -1 | sed "s/^${1}=[\"']\{0,1\}\([^\"']*\)[\"']\{0,1\}/\1/" || true; }
    _m="$(_val MODEL)"
    _s="$(_val SIDECAR_MODEL)"
    _p="$(_val PROXY_PORT)"
    _h="$(_val OLLAMA_HOST)"
    _d="$(_val VIBE_LOCAL_DEBUG)"
    [ -n "$_m" ] && MODEL="$_m"
    [ -n "$_s" ] && SIDECAR_MODEL="$_s"
    [ -n "$_p" ] && PROXY_PORT="$_p"
    [ -n "$_h" ] && OLLAMA_HOST="$_h"
    [ -n "$_d" ] && VIBE_LOCAL_DEBUG="$_d"
    unset _val _m _s _p _h _d
fi

# [SEC] Validate OLLAMA_HOST - only allow localhost (SSRF prevention)
case "$OLLAMA_HOST" in
    http://localhost:*|http://127.0.0.1:*|http://\[::1\]:*)
        ;; # allowed
    *)
        echo "⚠️  OLLAMA_HOST='$OLLAMA_HOST' はlocalhostではありません。セキュリティのためlocalhostにリセットします。"
        OLLAMA_HOST="http://localhost:11434"
        ;;
esac

# [SEC] Validate PROXY_PORT is numeric (1024-65535)
case "$PROXY_PORT" in
    ''|*[!0-9]*)
        echo "⚠️  PROXY_PORT='$PROXY_PORT' が不正です。デフォルト8082にリセットします。"
        PROXY_PORT=8082
        ;;
    *)
        if [ "$PROXY_PORT" -lt 1024 ] || [ "$PROXY_PORT" -gt 65535 ]; then
            echo "⚠️  PROXY_PORT=$PROXY_PORT は範囲外です (1024-65535)。デフォルト8082にリセットします。"
            PROXY_PORT=8082
        fi
        ;;
esac

# RAM 検出 (モデルまたはサイドカーの自動判定に使用)
if [[ "$(uname)" == "Darwin" ]]; then
    RAM_GB=$(( $(sysctl -n hw.memsize) / 1073741824 ))
else
    RAM_GB=$(( $(grep MemTotal /proc/meminfo | awk '{print $2}') / 1048576 ))
fi

# config が無い場合、RAM からメインモデルを自動判定
if [ -z "$MODEL" ]; then
    if [ "$RAM_GB" -ge 32 ]; then
        MODEL="qwen3-coder:30b"
    elif [ "$RAM_GB" -ge 16 ]; then
        MODEL="qwen3:8b"
    elif [ "$RAM_GB" -ge 8 ]; then
        MODEL="qwen3:1.7b"
    else
        echo "エラー: メモリが不足しています (${RAM_GB}GB)。最低8GB必要です。"
        exit 1
    fi
fi

# サイドカーモデル自動判定 (config に無い場合、RAM から選択)
if [ -z "$SIDECAR_MODEL" ]; then
    if [ "$RAM_GB" -ge 32 ]; then
        SIDECAR_MODEL="qwen3:8b"
    elif [ "$RAM_GB" -ge 16 ]; then
        SIDECAR_MODEL="qwen3:1.7b"
    fi
    # 8GB未満: サイドカー無し (メインモデルのみ)
fi

PROXY_URL="http://127.0.0.1:${PROXY_PORT}"
PROXY_PID_FILE="${STATE_DIR}/proxy.pid"
PROXY_LOG="${STATE_DIR}/proxy.log"

# --- 開発時フォールバック: プロキシスクリプトの探索 ---
if [ ! -f "$PROXY_SCRIPT" ]; then
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" 2>/dev/null && pwd || echo "")"
    if [ -n "$SCRIPT_DIR" ] && [ -f "${SCRIPT_DIR}/anthropic-ollama-proxy.py" ]; then
        PROXY_SCRIPT="${SCRIPT_DIR}/anthropic-ollama-proxy.py"
    else
        echo "エラー: プロキシスクリプトが見つかりません"
        echo "  install.sh を実行するか、anthropic-ollama-proxy.py を同じディレクトリに置いてください"
        exit 1
    fi
fi

# --- ollama が起動しているか確認・起動 ---
ensure_ollama() {
    if curl -s --max-time 2 "$OLLAMA_HOST/api/tags" &>/dev/null; then
        return 0
    fi

    echo "🦙 ollama を起動中..."
    if [[ "$(uname)" == "Darwin" ]]; then
        open -a Ollama 2>/dev/null || (ollama serve &>/dev/null &)
    else
        ollama serve &>/dev/null &
    fi

    for i in $(seq 1 15); do
        printf "\r  🦙 ollama 起動待ち... %ds " "$((i * 2))"
        sleep 2
        if curl -s --max-time 2 "$OLLAMA_HOST/api/tags" &>/dev/null; then
            printf "\r%-40s\n" ""
            echo "✅ ollama 起動完了"
            return 0
        fi
    done
    printf "\r%-40s\n" ""

    echo "❌ エラー: ollama が起動できませんでした"
    echo ""
    echo "対処法:"
    echo "  macOS: Ollama アプリを手動で起動してください"
    echo "  Linux: ollama serve を実行してください"
    return 1
}

# --- 変換プロキシの起動 ---
ensure_proxy() {
    # プロキシスクリプトが更新されていたら古いプロキシを再起動
    if [ -f "$PROXY_PID_FILE" ]; then
        local old_pid
        old_pid="$(cat "$PROXY_PID_FILE")"
        if kill -0 "$old_pid" 2>/dev/null; then
            # スクリプトの更新チェック (mtime比較)
            local script_mtime pid_mtime
            script_mtime="$(stat -f %m "$PROXY_SCRIPT" 2>/dev/null || stat -c %Y "$PROXY_SCRIPT" 2>/dev/null || echo 0)"
            pid_mtime="$(stat -f %m "$PROXY_PID_FILE" 2>/dev/null || stat -c %Y "$PROXY_PID_FILE" 2>/dev/null || echo 0)"
            if [ "$script_mtime" -gt "$pid_mtime" ]; then
                echo "🔄 プロキシスクリプトが更新されています。再起動します..."
                kill "$old_pid" 2>/dev/null || true
                rm -f "$PROXY_PID_FILE"
                sleep 1
            else
                # スクリプト未更新 & プロキシ応答あり → 再利用
                if curl -s --max-time 1 "$PROXY_URL/" &>/dev/null; then
                    return 0
                fi
            fi
        else
            # プロセスが死んでいる → PIDファイル掃除
            rm -f "$PROXY_PID_FILE"
        fi
    fi

    # 既に起動している場合はスキップ (PIDファイル無し but ポート応答あり)
    if curl -s --max-time 1 "$PROXY_URL/" &>/dev/null; then
        return 0
    fi

    # ポートが使用中なら別のポートを試す
    local port="$PROXY_PORT"
    for try_port in $PROXY_PORT 8083 8084 8085; do
        if ! curl -s --max-time 1 "http://127.0.0.1:${try_port}/" &>/dev/null; then
            port=$try_port
            break
        fi
    done
    PROXY_PORT=$port
    PROXY_URL="http://127.0.0.1:${PROXY_PORT}"

    echo "🔄 Anthropic→Ollama 変換プロキシを起動中 (port: $PROXY_PORT)..."

    # プロキシスクリプトが有効な Python かチェック
    if ! python3 -c "import ast, sys; ast.parse(open(sys.argv[1]).read())" "$PROXY_SCRIPT" 2>/dev/null; then
        echo "❌ エラー: プロキシスクリプトが壊れています"
        echo "  再インストール: curl -fsSL https://raw.githubusercontent.com/ochyai/vibe-local/main/install.sh | bash"
        return 1
    fi

    # プロキシ起動 (環境変数でモデル設定を渡す)
    OLLAMA_HOST="$OLLAMA_HOST" \
    VIBE_LOCAL_MODEL="$MODEL" \
    VIBE_LOCAL_SIDECAR_MODEL="${SIDECAR_MODEL:-$MODEL}" \
    VIBE_LOCAL_DEBUG="$VIBE_LOCAL_DEBUG" \
    python3 "$PROXY_SCRIPT" "$PROXY_PORT" >>"$PROXY_LOG" 2>&1 &
    local pid=$!
    echo "$pid" > "$PROXY_PID_FILE"

    # 起動待ち（スピナー付き）
    for i in $(seq 1 15); do
        printf "\r  🔄 プロキシ起動待ち... %ds " "$i"
        sleep 1
        if curl -s --max-time 1 "$PROXY_URL/" &>/dev/null; then
            printf "\r%-40s\n" ""
            echo "✅ 変換プロキシ起動完了 (PID: $pid, port: $PROXY_PORT)"
            return 0
        fi
        # プロセスが死んでいたら早期終了
        if ! kill -0 "$pid" 2>/dev/null; then
            printf "\r%-40s\n" ""
            echo "❌ エラー: プロキシが起動直後にクラッシュしました"
            echo ""
            echo "--- ログ ---"
            cat "$PROXY_LOG" 2>/dev/null || echo "(ログなし)"
            echo "--- ここまで ---"
            echo ""
            echo "対処法:"
            echo "  python3 がインストールされているか確認: python3 --version"
            echo "  手動で起動を試す: python3 $PROXY_SCRIPT $PROXY_PORT"
            return 1
        fi
    done
    printf "\r%-40s\n" ""

    echo "❌ エラー: 変換プロキシが応答しません (15秒タイムアウト)"
    echo ""
    echo "--- ログ ---"
    cat "$PROXY_LOG" 2>/dev/null || echo "(ログなし)"
    echo "--- ここまで ---"
    echo ""
    echo "対処法:"
    echo "  python3 がインストールされているか確認: python3 --version"
    echo "  手動で起動を試す: python3 $PROXY_SCRIPT $PROXY_PORT"
    return 1
}

# --- ネットワーク接続チェック ---
check_network() {
    curl -s --max-time 3 https://api.anthropic.com/ &>/dev/null
}

# --- クリーンアップ ---
cleanup() {
    if [ -f "$PROXY_PID_FILE" ]; then
        kill "$(cat "$PROXY_PID_FILE")" 2>/dev/null || true
        rm -f "$PROXY_PID_FILE"
    fi
}
trap cleanup EXIT

# --- 引数パース ---
AUTO_MODE=0
YES_FLAG=0
EXTRA_ARGS=()

while [[ $# -gt 0 ]]; do
    case "$1" in
        --auto)
            AUTO_MODE=1
            shift
            ;;
        --model)
            if [[ $# -lt 2 ]]; then
                echo "Error: --model requires an argument"
                exit 1
            fi
            MODEL="$2"
            shift 2
            ;;
        -y|--yes)
            YES_FLAG=1
            shift
            ;;
        --debug)
            VIBE_LOCAL_DEBUG=1
            shift
            ;;
        *)
            EXTRA_ARGS+=("$1")
            shift
            ;;
    esac
done

# --- 自動判定モード ---
if [ "$AUTO_MODE" -eq 1 ]; then
    if check_network; then
        echo "🌐 ネットワーク接続あり → 通常の Claude Code を起動"
        exec claude ${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}
    else
        echo "📡 ネットワーク接続なし → ローカルモード ($MODEL)"
    fi
fi

# --- ローカルモードで起動 ---
if ! ensure_ollama; then
    echo ""
    echo "ollama が起動できないため終了します。"
    exit 1
fi

# モデルがロード済みか確認
if ! curl -s "$OLLAMA_HOST/api/tags" 2>/dev/null | grep -qF "$MODEL"; then
    echo "❌ エラー: モデル $MODEL が見つかりません"
    echo ""
    echo "対処法:"
    echo "  ollama pull \"$MODEL\""
    echo ""
    echo "利用可能なモデル:"
    curl -s "$OLLAMA_HOST/api/tags" 2>/dev/null | python3 -c "
import sys, json
try:
    data = json.load(sys.stdin)
    for m in data.get('models', []):
        print(f\"  - {m['name']}\")
except: pass
" 2>/dev/null || echo "  (一覧取得失敗)"
    exit 1
fi

# 変換プロキシ起動
if ! ensure_proxy; then
    echo ""
    echo "プロキシが起動できないため終了します。"
    exit 1
fi

# --- パーミッション確認 ---
PERM_ARGS=()

if [ "$YES_FLAG" -eq 1 ]; then
    PERM_ARGS+=(--dangerously-skip-permissions)
else
    echo ""
    echo "============================================"
    echo " ⚠️  パーミッション確認 / Permission Check"
    echo "============================================"
    echo ""
    echo " vibe-local はツール自動許可モード"
    echo " (--dangerously-skip-permissions) で起動できます。"
    echo ""
    echo " This means the AI can execute commands, read/write"
    echo " files, and modify your system WITHOUT asking."
    echo ""
    echo " ローカルLLMはクラウドAIより精度が低いため、"
    echo " 意図しない操作が実行される可能性があります。"
    echo ""
    echo " Local LLMs are less accurate than cloud AI."
    echo " Unintended actions may occur."
    echo ""
    echo " 本地LLM精度较低，可能执行非预期操作。"
    echo ""
    echo "--------------------------------------------"
    echo " [y] 自動許可モード (Auto-approve all tools)"
    echo " [N] 通常モード (Ask before each tool use)"
    echo "--------------------------------------------"
    echo ""
    printf " 続行しますか？ / Continue? [y/N]: "
    read -r REPLY </dev/tty 2>/dev/null || read -r REPLY 2>/dev/null || REPLY="n"
    echo ""

    case "$REPLY" in
        [yY]|[yY][eE][sS]|はい|是)
            PERM_ARGS+=(--dangerously-skip-permissions)
            echo " → 自動許可モードで起動します"
            ;;
        *)
            echo " → 通常モード (毎回確認) で起動します"
            ;;
    esac
fi

PERM_LABEL="通常モード (ask each time)"
if [ ${#PERM_ARGS[@]} -gt 0 ]; then
    PERM_LABEL="ツール自動許可 (auto-approve)"
fi

DEBUG_LABEL="OFF"
if [ "$VIBE_LOCAL_DEBUG" -eq 1 ]; then
    DEBUG_LABEL="ON (full request/response logging)"
fi

SIDECAR_LABEL="${SIDECAR_MODEL:-none}"
echo ""
echo "============================================"
echo " 🤖 vibe-local"
echo " Model: $MODEL"
echo " Sidecar: $SIDECAR_LABEL"
echo " Proxy: $PROXY_URL → $OLLAMA_HOST"
echo " Permissions: $PERM_LABEL"
echo " Debug: $DEBUG_LABEL"
echo "============================================"
echo ""
echo " Launching Claude Code..."
echo ""

ANTHROPIC_BASE_URL="$PROXY_URL" \
ANTHROPIC_API_KEY="local" \
VIBE_LOCAL_DEBUG="$VIBE_LOCAL_DEBUG" \
exec claude --model "$MODEL" ${PERM_ARGS[@]+"${PERM_ARGS[@]}"} ${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}
