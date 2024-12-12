import streamlit as st
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

df = pd.read_excel("../R/ecm_result6.3.2.xlsx")

df = df[~df['시도'].isin(['서울특별시','경기도','인천광역시'])]
df['Q'] = np.where(
    df['통합 필드'].isin(["부산", "대구", "나주", "울산", "원주", "음성", "진천", "전주", "완주", "김천", "진주", "제주", "세종"]),
    1, 
    0   
)

qzero = df.loc[df["Q"]==0, ['grdp','year']]
qone = df.loc[df["Q"]==1, ['grdp','year']]
qzero, qone = qzero.dropna(), qone.dropna()

qzerosum = qzero.groupby(['year']).agg(['mean'])
qonesum = qone.groupby(['year']).agg(['mean'])

# 그래프 생성
plt.figure(figsize=(10, 6))
plt.plot(qzerosum.index, qzerosum['grdp'], label="Q0", marker='o')
plt.plot(qonesum.index, qonesum['grdp'], label="Q1", marker='s')
plt.xlabel("Year")
plt.ylabel("GRDP")
plt.title("GRDP Trends for Q0 and Q1")
plt.legend()
plt.grid()

st.pyplot(plt)