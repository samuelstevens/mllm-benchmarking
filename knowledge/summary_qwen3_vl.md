## Qwen3-VL Technical Report

Source: [arXiv:2511.21631](https://arxiv.org/abs/2511.21631)

Qwen3-VL introduces a new Qwen vision-language family with dense 2B/4B/8B/32B variants and MoE 30B-A3B / 235B-A22B variants, each offered in instruct and thinking forms. The paper emphasizes three system upgrades over the prior generation: interleaved MRoPE for stronger spatial-temporal position modeling, DeepStack-style multi-layer visual feature injection, and explicit textual timestamp tokens for video understanding.

The report evaluates the models across a broad multimodal suite spanning general VQA, OCR and document understanding, chart and STEM reasoning, spatial reasoning, grounding, multi-image understanding, video benchmarks, and agentic UI tasks. In the current benchmark repo, the most directly reusable rows are for tasks we already track, such as MMMU, MathVista, MathVision, MathVerse, ZeroBench, VLMsAreBlind, LogicVista, RealWorldQA, MMStar, SimpleVQA, HallusionBench, DocVQA, InfoVQA, ChartQA, OCRBench, CharXiv, CountBench, ERQA, VSI-Bench, BLINK, MuirBench, MVBench, Video-MME without subtitles, and MLVU.

For this repo, I imported only rows whose model IDs and task IDs already exist locally. I skipped mismatched benchmark variants that would blur semantics in the dashboard, including AI2D with mask, RefCOCO averaged across splits, OmniDocBench language-specific splits, ScreenSpot Pro, and several tasks we do not currently track as first-class IDs.
