from sched import scheduler
import dask
import dask.dataframe as dd
import spacy

from tag_regex import remove_tags

dask.config.set(scheduler='processes')
lg = spacy.load('zh_core_web_lg')

def segment(text):
    tokens = lg(text)
    return ' '.join([tok.text for tok in tokens])
        
def main():
    df = dd.read_parquet('./data/')
    df['text'] = df.text.apply(remove_tags,meta=('text',str))
    df = dd.read_parquet('./data')
    df['text'] = df.text.apply(segment,meta=('text',str))
    df.to_parquet('./untagseg/data')

if __name__ == '__main__':
    main()
