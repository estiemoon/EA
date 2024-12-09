import streamlit as st
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

df = pd.read_excel("../R/ecm_result6.3.2.xlsx")
x_headers = df.columns.to_list()[5:]

x = st.selectbox(
    "CHOOSE",
    x_headers,
)
x_vals = df[x]
x_vals = x_vals.dropna() 

st.write("You selected:", x)
b = st.slider("bins", 0, 100, 40, 1)

min_v = st.slider("min_x", int(min(x_vals)), 20, int(min(x_vals)), 1)
max_v = st.slider("max_x", 0, int(max(x_vals)), int(max(x_vals)), 1)


fig, ax = plt.subplots()
ax.hist(x_vals, bins=b, range=(min_v,max_v))

st.pyplot(fig)