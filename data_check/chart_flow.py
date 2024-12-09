import streamlit as st
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

df = pd.read_excel("../results/ecm_result6.3.2.xlsx")

regions = df['시군구'].unique()
values_list = df.columns.to_list()
values_list = values_list[5:]


r = st.selectbox(
    "CHOOSE",
    regions,
)
values = st.selectbox(
    "Choose value",
    values_list
)
r_vals = df.loc[df['시군구']==r, ['year',values]]
r_vals = r_vals.dropna() 

st.line_chart(r_vals,x='year',y=values)


# compare = st.selectbox(
#     "CHOOSE",
#     regions,
#     key='compare'
# )
c_values = st.selectbox(
    "Choose value",
    values_list,
    key='compare2'
)
c_vals = df.loc[df['시군구']==r, ['year',c_values]]
c_vals = c_vals.dropna() 

st.line_chart(c_vals, x='year',y=c_values)