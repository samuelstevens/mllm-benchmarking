# Falcon Perception

Paper: [arXiv:2603.27365](https://arxiv.org/abs/2603.27365)

## Summary

This paper introduces two compact early-fusion vision-language models from TII:

- `Falcon Perception`, a 600M dense transformer for open-vocabulary / promptable segmentation.
- `FalconOCR`, a 300M OCR model built from the same early-fusion backbone for document parsing.

The main architectural claim is that a single early-fusion transformer can jointly model image patches and text tokens from the first layer onward, instead of using a separate vision encoder plus downstream task-specific decoder. Falcon Perception uses hybrid attention and lightweight specialized heads for coordinates, size, and masks, while keeping one shared backbone.

## Headline Results

- On SA-Co, Falcon Perception reports `68.0` Macro-F1.
- On PBench, Falcon Perception reports `57.0` average Macro-F1.
- On RefCOCOm validation, Falcon Perception reports `77.3` Macro-F1.
- On olmOCR, FalconOCR reports `80.3` accuracy.
- On OmniDocBench v1.5 English-only, FalconOCR reports `88.64` overall.

## Notes

- Falcon Perception is positioned as a perception-first model rather than a general VQA model.
- FalconOCR is evaluated in a two-stage pipeline with PP-DocLayoutV3 for layout detection and FalconOCR for element-level recognition.
- The paper reports many baseline comparisons, but the repo update for this pass focuses on the two first-party Falcon models and their headline benchmark scores.
