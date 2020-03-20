#!/usr/bin/env python
# coding: utf-8

# In[1]:


import requests, json, csv, datetime, filecmp, os, sys, glob
from collections import OrderedDict


# In[2]:


url = "https://services1.arcgis.com/CY1LXxl9zlJeBuRZ/ArcGIS/rest/services/Florida_COVID19_Case_Line_Data/FeatureServer/0/query?f=json&where=1%3D1&returnGeometry=false&outFields=*"
response = requests.get(url).content
j = requests.get(url).json()


# In[3]:


cases = [obj["attributes"] for obj in j["features"]]
headers = cases[0].keys()
data = []
for case in cases:
    data.append(OrderedDict(case))


# In[4]:


tmpf = "temp.csv"
with open(tmpf, "w", newline="") as outfile:
    writer = csv.writer(outfile)
    writer.writerow(list(cases[0].keys()))
    for row in cases:
        writer.writerow(list(row.values()))


# In[5]:


rawFiles = sorted([f for f in os.listdir("output/raw/")])
latestFile = rawFiles[-1]


# In[6]:


tempAndLatestFileSameSize = os.path.getsize("output/raw/"+latestFile) == os.path.getsize(tmpf)
if tempAndLatestFileSameSize:
    print("Downloaded file is not an update. Removing '"+tmpf+"''")
    os.remove(tmpf)
    exit(1)


# In[7]:


os.rename(tmpf,sys.argv[1])


# In[14]:


# https://services1.arcgis.com/CY1LXxl9zlJeBuRZ/ArcGIS/rest/services

