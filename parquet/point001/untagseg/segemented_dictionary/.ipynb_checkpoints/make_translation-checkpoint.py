from transformers import M2M100ForConditionalGeneration, M2M100Tokenizer

model = M2M100ForConditionalGeneration.from_pretrained("facebook/m2m100_418M")
tokenizer = M2M100Tokenizer.from_pretrained("facebook/m2m100_418M")

tokenizer.src_lang = "zh"

def translate(text):
    encoded_zh = tokenizer(text, return_tensors="pt")
    generated_tokens = model.generate(**encoded_zh, forced_bos_token_id=tokenizer.get_lang_id("en"))
    english = tokenizer.batch_decode(generated_tokens, skip_special_tokens=True)
    return english[0]
    

import dask.dataframe as dd
df = dd.read_parquet('./data')
df['translated'] = df.text.apply(translate,meta=('text',str))
df.to_parquet('./translated/data/')