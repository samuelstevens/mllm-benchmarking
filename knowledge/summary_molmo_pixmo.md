# Molmo and PixMo

Source: https://arxiv.org/abs/2409.17146

`Molmo and PixMo: Open Weights and Open Data for State-of-the-Art Vision-Language Models` introduces the Molmo family of open VLMs and the PixMo data collection pipeline. The paper argues that strong open VLMs had been bottlenecked by proprietary or synthetic multimodal training data, so it releases both models and a new suite of human-collected and non-VLM-generated datasets.

Key points:

- `PixMo` combines dense speech-to-text captions, instruction-following QA, pointing annotations, and targeted synthetic datasets without relying on external VLM distillation.
- The benchmark table reports 11 evaluation columns: `AI2D`, `ChartQA`, `VQAv2`, `DocVQA`, `InfoVQA`, `TextVQA`, `RealWorldQA`, `MMMU`, `MathVista`, `CountBenchQA`, and the paper's new `PixMo-Count`.
- Within that table, `MolmoE-1B`, `Molmo-7B-O`, `Molmo-7B-D`, and `Molmo-72B` are the paper's first-party models. `Molmo-72B` is the strongest row overall, with especially strong counting results on `CountBenchQA` and `PixMo-Count`.
- The paper explicitly notes that `Molmo-1B-E` has `1.2B` active parameters but `6.9B` total parameters because it is based on `OLMoE-1B-7B`.

Benchmark rows added to this repo from the paper:

- Models: `MolmoE-1B`, `Molmo-7B-O`, `Molmo-7B-D`, `Molmo-72B`
- New tasks needed for coverage: `AI2D`, `CountBenchQA`, `PixMo-Count`
