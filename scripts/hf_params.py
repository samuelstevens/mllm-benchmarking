"""Print the number of parameters (in millions) for a HuggingFace model.

Usage:
    python hf_params.py https://huggingface.co/LiquidAI/LFM2.5-VL-1.6B
    python hf_params.py LiquidAI/LFM2.5-VL-1.6B
"""

import json
import sys
import urllib.request


def main():
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <huggingface-url-or-repo-id>", file=sys.stderr)
        sys.exit(1)

    repo = sys.argv[1]
    # Strip HuggingFace URL prefix if provided
    repo = repo.removeprefix("https://huggingface.co/")
    repo = repo.rstrip("/")

    url = f"https://huggingface.co/api/models/{repo}"
    try:
        with urllib.request.urlopen(url) as resp:
            data = json.load(resp)
    except urllib.error.HTTPError as e:
        print(f"Error: {e.code} for {url}", file=sys.stderr)
        sys.exit(1)

    params = data.get("safetensors", {}).get("parameters")
    if not params:
        print(f"Error: no safetensors parameter info for {repo}", file=sys.stderr)
        sys.exit(1)

    total = sum(params.values())
    print(round(total / 1e6))


if __name__ == "__main__":
    main()
