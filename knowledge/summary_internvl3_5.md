# InternVL3.5 Summary

Paper: https://arxiv.org/abs/2508.18265

## What it is

InternVL3.5 is an open multimodal model family spanning small dense models through large MoE variants. The paper emphasizes three themes:

- stronger multimodal reasoning, especially with Cascade RL and optional parallel thinking
- broader versatility across OCR, grounding, multilingual, GUI, embodied, video, and SVG tasks
- better efficiency through techniques like visual resolution routing and decoupled vision-language deployment

## Main reported model family

- InternVL3.5-1B
- InternVL3.5-2B
- InternVL3.5-4B
- InternVL3.5-8B
- InternVL3.5-14B
- InternVL3.5-20B-A4B
- InternVL3.5-30B-A3B
- InternVL3.5-38B
- InternVL3.5-241B-A28B

The paper also reports “Parallel Thinking” variants for the reasoning table and compares against a large number of open and closed baselines.

## Benchmarks covered

The main evaluation tables span:

- general multimodal understanding
- multimodal reasoning and math
- OCR, chart, and document understanding
- multi-image and real-world understanding
- hallucination and comprehensive evaluation
- grounding
- multilingual evaluation
- video understanding
- GUI agent tasks
- embodied tasks
- SVG understanding and SVG generation
- text-only capability

## High-level takeaways

- The strongest InternVL3.5 models are competitive with top open models across a very broad task surface.
- The 30B-A3B and 241B-A28B variants are the main flagship checkpoints in the paper’s headline comparison table.
- The reasoning table shows especially large gains over InternVL3, and the paper attributes much of that jump to Cascade RL.
- The paper goes beyond standard VQA and OCR benchmarks into agentic and grounding-heavy evaluations, which makes it unusually broad for one release paper.

## Repo import note

For this repo, I treated the paper’s main evaluation tables as import targets and kept the paper itself as the source URL for the newly added task rows and score rows.
