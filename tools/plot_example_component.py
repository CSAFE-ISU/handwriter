import pandas as pd

f = '/Users/stephanie/Documents/version_control/handwriter/temp.csv'
df = pd.read_csv(f)
df = df[['from', 'to']]
rows = list(df.itertuples(index=False, name=None))


g = dict()


for pair in rows:
    if pair[0] in g:
        g[pair[0]].append(pair[1])
    else:
        g[pair[0]] = [pair[1]]
