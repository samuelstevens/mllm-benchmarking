"""Print the number of parameters (in millions) for a HuggingFace model.

Usage:
    python hf_params.py https://huggingface.co/LiquidAI/LFM2.5-VL-1.6B
    python hf_params.py LiquidAI/LFM2.5-VL-1.6B
"""

import json
import math
import struct
import sys
import urllib.request


def get_repo(arg):
    """Parse a HuggingFace URL or repo id into an org/model string."""
    repo = arg.removeprefix("https://huggingface.co/")
    repo = repo.rstrip("/")
    for suffix in ("/tree/", "/blob/"):
        if suffix in repo:
            repo = repo[: repo.index(suffix)]
    return repo


def params_from_api(repo):
    """Try the HuggingFace model API for pre-computed parameter counts."""
    url = f"https://huggingface.co/api/models/{repo}"
    try:
        with urllib.request.urlopen(url) as resp:
            data = json.load(resp)
    except urllib.error.HTTPError as e:
        print(f"Error: {e.code} for {url}", file=sys.stderr)
        sys.exit(1)

    params = (data.get("safetensors") or {}).get("parameters")
    if params:
        return sum(params.values())

    return None


def safetensor_filenames(repo):
    """List safetensor filenames from the repo file listing."""
    url = f"https://huggingface.co/api/models/{repo}"
    with urllib.request.urlopen(url) as resp:
        data = json.load(resp)

    return [
        s["rfilename"]
        for s in data.get("siblings", [])
        if s["rfilename"].endswith(".safetensors")
    ]


def params_from_safetensor_header(repo, filename):
    """Read a safetensors file header via HTTP range request and sum params."""
    url = f"https://huggingface.co/{repo}/resolve/main/{filename}"

    # First 8 bytes are a little-endian u64 giving the header size.
    req = urllib.request.Request(url, headers={"Range": "bytes=0-7"})
    with urllib.request.urlopen(req) as resp:
        header_size = struct.unpack("<Q", resp.read(8))[0]

    # Now fetch the JSON header.
    req = urllib.request.Request(
        url, headers={"Range": f"bytes=8-{7 + header_size}"}
    )
    with urllib.request.urlopen(req) as resp:
        header = json.loads(resp.read(header_size))

    total = 0
    for key, meta in header.items():
        if key == "__metadata__":
            continue
        total += math.prod(meta["shape"])
    return total


def params_from_safetensors(repo):
    """Fall back to reading safetensor file headers directly."""
    filenames = safetensor_filenames(repo)
    if not filenames:
        return None

    total = 0
    for fn in filenames:
        total += params_from_safetensor_header(repo, fn)
    return total


def main():
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <huggingface-url-or-repo-id>", file=sys.stderr)
        sys.exit(1)

    repo = get_repo(sys.argv[1])

    total = params_from_api(repo)
    if total is None:
        total = params_from_safetensors(repo)
    if total is None:
        print(f"Error: no parameter info for {repo}", file=sys.stderr)
        sys.exit(1)

    print(round(total / 1e6))


if __name__ == "__main__":
    main()
