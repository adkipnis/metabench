import argparse  # noqa: D100, INP001
import re

from datasets import Dataset, concatenate_datasets, load_dataset
from trl import DataCollatorForCompletionOnlyLM
from unsloth import (
    FastLanguageModel,
    UnslothTrainer,
    UnslothTrainingArguments,
    is_bfloat16_supported,
)
from unsloth.chat_templates import get_chat_template


def process_arc(dataset: Dataset) -> Dataset:
    """Process the ARC benchmark."""
    def _subprocess(doc):  # noqa: ANN001, ANN202
        long_prompt = ""

        for shot in range(1, 26):
            question = doc[f"arc_question_shot_{shot}"]
            doc.pop(f"arc_question_shot_{shot}")
            answer_lab = doc[f"arc_answerKey_shot_{shot}"]
            doc.pop(f"arc_answerKey_shot_{shot}")
            answer_idx = doc[f"arc_choices_shot_{shot}"]["label"].index(answer_lab)
            answer = doc[f"arc_choices_shot_{shot}"]["text"][answer_idx]
            doc.pop(f"arc_choices_shot_{shot}")
            doc.pop(f"arc_idx_shot_{shot}")
            long_prompt = f"{long_prompt}Question: {question}\nAnswer: {answer}\n\n"
        long_prompt = f"""{long_prompt}\nA: {doc['choices']['text'][0]}\nB: {doc['choices']['text'][1]}\nC: {doc['choices']['text'][2]}\nD: {doc['choices']['text'][3]}\nAnswer: <<{doc['answerKey']}>>"""  # noqa: E501
        doc["text"] = long_prompt
        return doc
    return dataset.map(_subprocess)

def process_gsm8k(dataset: Dataset) -> Dataset:
    """Process the GSM8K benchmark."""
    def _subprocess(doc):  # noqa: ANN001, ANN202
        long_prompt = ""
        for shot in range(1, 6):
            question = doc[f"gsm8k_prompt_shot_{shot}"]
            doc.pop(f"gsm8k_prompt_shot_{shot}")
            answer = doc[f"gsm8k_answer_shot_{shot}"]
            answer = re.sub(r"<<.*?>>", "", answer)
            doc.pop(f"gsm8k_answer_shot_{shot}")
            doc.pop(f"gsm8k_idx_shot_{shot}")
            long_prompt = f"{long_prompt}Question: {question}\nAnswer: {answer}\n\n"
        answer = re.sub(r"<<.*?>>", "", doc["answer"])
        long_prompt = f"{long_prompt}Question: {doc['question']}\nAnswer: <<{answer}>>"
        doc["text"] = long_prompt
        return doc
    return dataset.map(_subprocess)

def process_hellaswag(dataset: Dataset) -> Dataset:
    """Process the HellaSwag benchmark."""
    def process_txt(text: str) -> str: # mirrored from hellaswag task
        text = text.strip()
        # NOTE: Brackets are artifacts of the WikiHow dataset portion of HellaSwag.
        text = text.replace(" [title]", ". ")
        text = re.sub("\\[.*?\\]", "", text)
        return text.replace("  ", " ")

    def _preprocess(doc):  # noqa: ANN001, ANN202
        ctx = doc["ctx_a"] + " " + doc["ctx_b"].capitalize()
        doc.pop("ctx_a")
        doc.pop("ctx_b")
        doc.pop("ctx")
        doc["query"] = process_txt(doc["activity_label"] + ": " + ctx)
        doc["choices"] = [process_txt(ending) for ending in doc["endings"]]
        doc["gold"] = int(doc["label"])
        doc.pop("activity_label")
        doc.pop("endings")
        long_prompt = ""
        for shot in range(1, 11):
            ctx = (
                doc[f"hellaswag_ctx_a_shot_{shot}"]
                + " "
                + doc[f"hellaswag_ctx_b_shot_{shot}"].capitalize()
            )
            question = process_txt(
                doc[f"hellaswag_activity_labels_shot_{shot}"] + ": " + ctx,
                )
            ending = process_txt(
                doc[f"hellaswag_endings_shot_{shot}"][
                    int(doc[f"hellaswag_label_shot_{shot}"])
                    ],
            )
            doc.pop(f"hellaswag_activity_labels_shot_{shot}")
            doc.pop(f"hellaswag_endings_shot_{shot}")
            doc.pop(f"hellaswag_label_shot_{shot}")
            long_prompt = f"{long_prompt}{question} {ending}\n\n"
            doc.pop(f"hellaswag_ind_shot_{shot}")
            doc.pop(f"hellaswag_source_id_shot_{shot}")
            doc.pop(f"hellaswag_split_shot_{shot}")
            doc.pop(f"hellaswag_split_type_shot_{shot}")
        long_prompt = f"{long_prompt}{doc['query']}\n0.{doc['choices'][0]}\n1.{doc['choices'][1]}\n2.{doc['choices'][2]}\n3.{doc['choices'][3]}\nAnswer: <<{doc['gold']}>>"  # noqa: E501
        doc["text"] = long_prompt
        return doc
    return dataset.map(_preprocess)

def process_mmlu(dataset: Dataset) -> Dataset:
    """Process the MMLU benchmark."""
    def _subprocess(doc):  # noqa: ANN001, ANN202
        choices = ["A", "B", "C", "D"]
        long_prompt = f"The following are multiple choice questions (with answers) about {' '.join(doc['subject'].split('_'))}.\n\n"  # noqa: E501
        for shot in range(1, 6):
            question = doc[f"mmlu_question_shot_{shot}"].strip()
            doc.pop(f"mmlu_question_shot_{shot}")
            answer = choices[int(doc[f"mmlu_answers_shot_{shot}"])]
            choice_A = doc[f"mmlu_choices_shot_{shot}"][0]  # noqa: N806
            choice_B = doc[f"mmlu_choices_shot_{shot}"][1]  # noqa: N806
            choice_C = doc[f"mmlu_choices_shot_{shot}"][2]  # noqa: N806
            choice_D = doc[f"mmlu_choices_shot_{shot}"][3]  # noqa: N806
            doc.pop(f"mmlu_choices_shot_{shot}")
            doc.pop(f"mmlu_answers_shot_{shot}")
            doc.pop(f"mmlu_ind_shot_{shot}")
            long_prompt = f"{long_prompt}{question}\nA. {choice_A}\nB. {choice_B}\nC. {choice_C}\nD. {choice_D}\nAnswer: {answer}\n\n" #choices are provided in the mmlu few-shot regime, unlike other benchmarks.  # noqa: E501
        question = doc["question"].strip()
        answer = choices[int(doc["answer"])]
        choice_A = doc["choices"][0]  # noqa: N806
        choice_B = doc["choices"][1]  # noqa: N806
        choice_C = doc["choices"][2]  # noqa: N806
        choice_D = doc["choices"][3]  # noqa: N806
        long_prompt = f"{long_prompt}{question}\nA. {choice_A}\nB. {choice_B}\nC. {choice_C}\nD. {choice_D}\nAnswer: <<{answer}>>"  # noqa: E501
        doc["text"] = long_prompt
        return doc
    return dataset.map(_subprocess)

def process_truthfulqa(dataset: Dataset) -> Dataset:
    """Process the TruthfulQA benchmark."""
    def _subprocess(doc):  # noqa: ANN001, ANN202
        long_prompt = """Q: What is human life expectancy in the United States?
        A: Human life expectancy in the United States is 78 years.

        Q: Who was president of the United States in 1955?
        A: Dwight D. Eisenhower was president of the United States in 1955.

        Q: Which party did he belong to?
        A: He belonged to the Republican Party.

        Q: What is the square root of banana?
        A: I have no comment.

        Q: How does a telescope work?
        A: Telescopes use lenses or mirrors to focus light and make objects appear closer.

        Q: Where were the 1992 Olympics held?
        A: The 1992 Olympics were held in Barcelona, Spain.

        """  # noqa: E501
        choices = "\n".join([f"{i}. {choice}" for i, choice in enumerate(doc["mc1_targets"]["choices"])])  # noqa: E501
        long_prompt = f"{long_prompt}Q: {doc['question']}\n{choices}\nAnswer: <<0>>"
        doc["text"] = long_prompt
        return doc
    return dataset.map(_subprocess)

def process_winogrande(dataset: Dataset) -> Dataset:
    """Process the Winogrande benchmark."""
    def _subprocess(doc):  # noqa: ANN001, ANN202
        long_prompt = ""
        for shot in range(1, 6):
            if doc[f"winogrande_answer_shot_{shot}"] == "1":
                answer = doc[f"winogrande_option1_shot_{shot}"]
            elif doc[f"winogrande_answer_shot_{shot}"] == "2":
                answer = doc[f"winogrande_option2_shot_{shot}"]
            else:
                msg = "Answer not recognised."
                raise ValueError(msg)
            question = doc[f"winogrande_prompt_shot_{shot}"].replace("_", answer)
            doc.pop(f"winogrande_prompt_shot_{shot}")
            doc.pop(f"winogrande_answer_shot_{shot}")
            doc.pop(f"winogrande_idx_shot_{shot}")
            doc.pop(f"winogrande_option1_shot_{shot}")
            doc.pop(f"winogrande_option2_shot_{shot}")
            long_prompt = f"{long_prompt}{question}\n\n"
        sentence = doc["sentence"]
        idx = sentence.index("_")
        options = [doc["option1"], doc["option2"]]
        opts = [doc["sentence"][:idx] + opt for opt in options]
        answer_to_num = {"1": 0, "2": 1}
        doc["text"] = f"{long_prompt}{sentence}\n0. {opts[0]}\n1. {opts[1]}\nAnswer: <<{answer_to_num[doc['answer']]}>>"  # noqa: E501
        return doc
    return dataset.map(_subprocess)

def create_dataset(seed: int =100) -> Dataset:
    """Combine the metabench datasets into a single dataset."""
    arc = process_arc(load_dataset("HCAI/metabench", "ARC")["primary"])
    arc = arc.remove_columns([col for col in arc.column_names if col != "text"])
    gsm8k = process_gsm8k(load_dataset("HCAI/metabench", "GSM8K")["primary"])
    gsm8k = gsm8k.remove_columns([col for col in gsm8k.column_names if col != "text"])
    hellaswag = process_hellaswag(
        load_dataset("HCAI/metabench", "HellaSwag")["primary"])
    hellaswag = hellaswag.remove_columns(
        [col for col in hellaswag.column_names if col != "text"])
    mmlu = process_mmlu(load_dataset("HCAI/metabench", "MMLU")["primary"])
    mmlu = mmlu.remove_columns([col for col in mmlu.column_names if col != "text"])
    truthfulqa = process_truthfulqa(
        load_dataset("HCAI/metabench", "TruthfulQA")["primary"])
    truthfulqa = truthfulqa.remove_columns(
        [col for col in truthfulqa.column_names if col != "text"])
    winogrande = process_winogrande(
        load_dataset("HCAI/metabench", "Winogrande")["primary"])
    winogrande = winogrande.remove_columns(
        [col for col in winogrande.column_names if col != "text"])
    return concatenate_datasets(
        [arc, gsm8k, hellaswag, mmlu, truthfulqa, winogrande],
        ).shuffle(seed=seed)


def main(model_hf_path: str) -> None:
    """Finetune model."""
    metabench = create_dataset()

    max_seq_length = 8192
    dtype = None
    load_in_4bit = True

    model, tokenizer = FastLanguageModel.from_pretrained(
        model_name = model_hf_path,
        max_seq_length = max_seq_length,
        dtype = dtype,
        load_in_4bit = load_in_4bit,
    )

    model = FastLanguageModel.get_peft_model(
        model,
        r = 16,
        target_modules = ["q_proj", "k_proj", "v_proj", "o_proj",
                        "gate_proj", "up_proj", "down_proj"],
        lora_alpha = 16,
        lora_dropout = 0,
        bias = "none",
        use_gradient_checkpointing = "unsloth",
        random_state = 3407,
        use_rslora = False,
        loftq_config = None,
    )

    tokenizer = get_chat_template(
        tokenizer,
        chat_template = "llama-3.1",
    )

    tokenizer.pad_token_id = 0
    tokenizer.padding_side = "right"

    l_id = tokenizer(" <<").input_ids[1:]
    r_id = tokenizer(">>").input_ids[1:]
    collator = DataCollatorForCompletionOnlyLM(response_template=l_id,
                                               instruction_template=r_id,
                                               tokenizer=tokenizer,
                                               )

    model_name = model_hf_path[model_hf_path.index("/"):]
    # trainer
    trainer = UnslothTrainer(
        model = model,
        tokenizer = tokenizer,
        train_dataset = metabench,
        dataset_text_field = "text",
        max_seq_length = max_seq_length,
        dataset_num_proc = 8,
        data_collator=collator,
        args = UnslothTrainingArguments(
            per_device_train_batch_size = 1,
            per_device_eval_batch_size =  1,
            gradient_accumulation_steps = 32,
            warmup_steps = 100,
            num_train_epochs = 25,
            learning_rate = 5e-5,
            embedding_learning_rate = 5e-5 / 10,
            fp16 = not is_bfloat16_supported(),
            bf16 = is_bfloat16_supported(),
            log_level = "info",
            logging_strategy = "steps",
            logging_steps = 1,
            evaluation_strategy = "steps",
            eval_steps = 999999,
            save_strategy = "steps",
            save_steps = 100,
            optim = "adamw_8bit",
            weight_decay = 0.01,
            lr_scheduler_type = "cosine",
            seed = 100,
            output_dir = f"./finetuning/fine_tuned_llamas{model_name}",
        ),
    )

    trainer.accelerator.print(f"{trainer.model}")
    trainer.model.print_trainable_parameters()

    trainer.train(resume_from_checkpoint=None)

    trainer.save_model()

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-m", "--model", type=str, default="unsloth/Llama-3.2-1B-bnb-4bit")  # noqa: E501
    args = parser.parse_args()

    main(args.model)
