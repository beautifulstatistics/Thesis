import re

pattern = f'/* *@[A-Za-z0-9]+[:{chr(65306)} ]+'
comp = re.compile(pattern)

def remove_tags(text):
    return comp.sub('',text)

def extract_tags(text):
    return ' '.join(comp.findall(text))