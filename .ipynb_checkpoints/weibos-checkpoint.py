import pandas as pd

df = pd.DataFrame()
for i in range(1,53):
    fn = f'./Weibo/week{i}.csv'
    df_new = pd.read_csv(fn,encoding_errors='replace')
    df_new['permission_denied'] = ~df_new.permission_denied.isna()
    df_new['image'] = (df_new.image == 1).astype(bool)
    df_new = df_new.drop(['mid','retweeted_status_mid','uid','geo','created_at','retweeted_uid','source','deleted_last_seen'],axis=1)
    df = pd.concat([df,df_new])
    print(fn)

df.to_csv('weibos.csv')
