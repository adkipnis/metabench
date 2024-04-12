# SOURCE: https://huggingface.co/spaces/HuggingFaceH4/open_llm_leaderboard/tree/main
import json
import math
import numpy as np
from dataclasses import dataclass
from enum import Enum


@dataclass
class Task:
    benchmark: str
    metric: str
    col_name: str


class Tasks(Enum):
    arc = Task("arc:challenge", "acc_norm", "ARC")
    hellaswag = Task("hellaswag", "acc_norm", "HellaSwag")
    mmlu = Task("hendrycksTest", "acc", "MMLU")
    truthfulqa = Task("truthfulqa:mc", "mc2", "TruthfulQA")
    winogrande = Task("winogrande", "acc", "Winogrande")
    gsm8k = Task("gsm8k", "acc", "GSM8K")


@dataclass
class ModelDetails:
    name: str
    symbol: str = ""  # emoji, only for the model type


class Precision(Enum):
    float16 = ModelDetails("float16")
    bfloat16 = ModelDetails("bfloat16")
    qt_8bit = ModelDetails("8bit")
    qt_4bit = ModelDetails("4bit")
    qt_GPTQ = ModelDetails("GPTQ")
    Unknown = ModelDetails("?")

    def from_str(precision):
        if precision in ["torch.float16", "float16"]:
            return Precision.float16
        if precision in ["torch.bfloat16", "bfloat16"]:
            return Precision.bfloat16
        if precision in ["8bit"]:
            return Precision.qt_8bit
        if precision in ["4bit"]:
            return Precision.qt_4bit
        if precision in ["GPTQ", "None"]:
            return Precision.qt_GPTQ
        return Precision.Unknown


class ModelType(Enum):
    PT = ModelDetails(name="pretrained", symbol="🟢")
    CPT = ModelDetails(name="continuously pretrained", symbol="🟩")
    FT = ModelDetails(
        name="fine-tuned on domain-specific datasets", symbol="🔶")
    chat = ModelDetails(name="chat models (RLHF, DPO, IFT, ...)", symbol="💬")
    merges = ModelDetails(name="base merges and moerges", symbol="🤝")
    Unknown = ModelDetails(name="", symbol="?")

    def to_str(self, separator=" "):
        return f"{self.value.symbol}{separator}{self.value.name}"

    @staticmethod
    def from_str(type):
        if "fine-tuned" in type or "🔶" in type:
            return ModelType.FT
        if "continously pretrained" in type or "🟩" in type:
            return ModelType.CPT
        if "pretrained" in type or "🟢" in type:
            return ModelType.PT
        if any([k in type for k in ["instruction-tuned", "RL-tuned", "chat", "🟦", "⭕", "💬"]]):
            return ModelType.chat
        if "merge" in type or "🤝" in type:
            return ModelType.merges
        return ModelType.Unknown


class WeightType(Enum):
    Adapter = ModelDetails("Adapter")
    Original = ModelDetails("Original")
    Delta = ModelDetails("Delta")


@dataclass
class EvalResult:
    # Also see src.display.utils.AutoEvalColumn for what will be displayed.
    eval_name: str  # org_model_precision (uid)
    full_model: str  # org/model (path on hub)
    org: str
    model: str
    revision: str  # commit hash, "" if main
    results: dict
    precision: Precision = Precision.Unknown
    model_type: ModelType = ModelType.Unknown  # Pretrained, fine tuned, ...
    weight_type: WeightType = WeightType.Original  # Original or Adapter
    architecture: str = "Unknown"  # From config file
    license: str = "?"
    likes: int = 0
    num_params: int = 0
    date: str = ""  # submission date of request file
    still_on_hub: bool = True
    is_merge: bool = False
    flagged: bool = False
    status: str = "FINISHED"
    tags: list = None

    @classmethod
    def init_from_json_file(self, json_filepath):
        """Inits the result from the specific model result file"""
        with open(json_filepath) as fp:
            data = json.load(fp)

        # We manage the legacy config format
        config = data.get("config_general")

        # Precision
        precision = Precision.from_str(config.get("model_dtype"))

        # Get model and org
        org_and_model = config.get("model_name")
        org_and_model = org_and_model.split("/", 1)

        if len(org_and_model) == 1:
            org = None
            model = org_and_model[0]
            result_key = f"{model}_{precision.value.name}"
        else:
            org = org_and_model[0]
            model = org_and_model[1]
            result_key = f"{org}_{model}_{precision.value.name}"
        full_model = "/".join(org_and_model)

        # Extract results available in this file (some results are split in several files)
        results = {}
        for task in Tasks:
            task = task.value
            # We skip old mmlu entries
            wrong_mmlu_version = False
            if task.benchmark == "hendrycksTest":
                for mmlu_k in ["harness|hendrycksTest-abstract_algebra|5", "hendrycksTest-abstract_algebra"]:
                    if mmlu_k in data["versions"] and data["versions"][mmlu_k] == 0:
                        wrong_mmlu_version = True

            if wrong_mmlu_version:
                continue

            # Some truthfulQA values are NaNs
            if task.benchmark == "truthfulqa:mc" and "harness|truthfulqa:mc|0" in data["results"]:
                if math.isnan(float(data["results"]["harness|truthfulqa:mc|0"][task.metric])):
                    results[task.benchmark] = 0.0
                    continue

            # We average all scores of a given metric (mostly for mmlu)
            accs = np.array([v.get(task.metric, None)
                            for k, v in data["results"].items() if task.benchmark in k])
            if accs.size == 0 or any([acc is None for acc in accs]):
                continue

            mean_acc = np.mean(accs) * 100.0
            results[task.benchmark] = mean_acc

        return self(
            eval_name=result_key,
            full_model=full_model,
            org=org,
            model=model,
            results=results,
            precision=precision,
            revision=config.get("model_sha", ""),
        )
