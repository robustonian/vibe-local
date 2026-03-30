import importlib.util
import json
import os
import subprocess
import sys
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]
VIBE_CODER_PATH = REPO_ROOT / "vibe-coder.py"
PROXY_PATH = REPO_ROOT / "anthropic-ollama-proxy.py"


def _load_module(module_name, path, monkeypatch, tmp_path):
    monkeypatch.setenv("HOME", str(tmp_path))
    monkeypatch.setenv("VIBE_LOCAL_PROXY_PORT", "18082")
    spec = importlib.util.spec_from_file_location(module_name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _load_vibe_coder(monkeypatch, tmp_path):
    original_isatty = sys.stdout.isatty
    try:
        sys.stdout.isatty = lambda: True
        return _load_module("vibe_coder_usage_accounting", VIBE_CODER_PATH, monkeypatch, tmp_path)
    finally:
        sys.stdout.isatty = original_isatty


def _run_proxy_normalizer(tmp_path, usage):
    env = os.environ.copy()
    env["HOME"] = str(tmp_path)
    env["VIBE_LOCAL_PROXY_PORT"] = "18082"
    script = f"""
import importlib.util
import json

spec = importlib.util.spec_from_file_location("anthropic_ollama_proxy_usage_accounting", {str(PROXY_PATH)!r})
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
print(json.dumps(module._normalize_usage_for_anthropic({json.dumps(usage)}), sort_keys=True))
"""
    completed = subprocess.run(
        [sys.executable, "-c", script],
        check=True,
        capture_output=True,
        text=True,
        env=env,
    )
    return json.loads(completed.stdout.strip().splitlines()[-1])


def test_vibe_coder_normalizes_cached_prompt_tokens(monkeypatch, tmp_path):
    vc = _load_vibe_coder(monkeypatch, tmp_path)
    usage = {
        "prompt_tokens": 120,
        "completion_tokens": 30,
        "prompt_tokens_details": {"cached_tokens": 80},
    }

    normalized = vc._normalize_usage_for_telemetry(usage)

    assert normalized["prompt_tokens"] == 120
    assert normalized["completion_tokens"] == 30
    assert normalized["input_tokens"] == 40
    assert normalized["output_tokens"] == 30
    assert normalized["cache_read_tokens"] == 80

    cfg = vc.Config()
    telemetry = vc.SessionTelemetry(cfg, "session-id")
    telemetry.start_turn()
    telemetry.record_model_response("Qwen3.5-27B-UD-Q4_K_XL", usage)

    turn = telemetry.turns[0]
    assert turn["inputTokens"] == 40
    assert turn["outputTokens"] == 30
    assert turn["cacheReadTokens"] == 80


def test_vibe_coder_preserves_plain_prompt_tokens(monkeypatch, tmp_path):
    vc = _load_vibe_coder(monkeypatch, tmp_path)
    usage = {
        "prompt_tokens": 120,
        "completion_tokens": 30,
    }

    normalized = vc._normalize_usage_for_telemetry(usage)

    assert normalized["prompt_tokens"] == 120
    assert normalized["completion_tokens"] == 30
    assert normalized["input_tokens"] == 120
    assert normalized["output_tokens"] == 30
    assert normalized["cache_read_tokens"] == 0


def test_proxy_normalizes_cached_prompt_tokens(tmp_path):
    usage = {
        "prompt_tokens": 120,
        "completion_tokens": 30,
        "prompt_tokens_details": {"cached_tokens": 80},
    }

    normalized = _run_proxy_normalizer(tmp_path, usage)

    assert normalized == {
        "input_tokens": 40,
        "output_tokens": 30,
        "cache_creation_input_tokens": 0,
        "cache_read_input_tokens": 80,
    }


def test_proxy_preserves_plain_prompt_tokens(tmp_path):
    usage = {
        "prompt_tokens": 120,
        "completion_tokens": 30,
    }

    normalized = _run_proxy_normalizer(tmp_path, usage)

    assert normalized == {
        "input_tokens": 120,
        "output_tokens": 30,
        "cache_creation_input_tokens": 0,
        "cache_read_input_tokens": 0,
    }
