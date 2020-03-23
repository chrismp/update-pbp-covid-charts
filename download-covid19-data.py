#!/usr/bin/env python
# coding: utf-8

# In[1]:


import requests, json, csv, datetime, filecmp, os, sys, glob
from collections import OrderedDict


# In[2]:


url = sys.argv[1]
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

rawDir = sys.argv[2]+'/'
rawFiles = sorted([f for f in os.listdir(rawDir)])
if len(rawFiles)==0:
	os.rename(tmpf,sys.argv[3])
	exit(0)

latestFile = rawFiles[-1]
print(latestFile)
print(os.path.getsize(rawDir+latestFile))
print(os.path.getsize(tmpf))


# In[6]:


tempAndLatestFileSameSize = os.path.getsize(rawDir+latestFile) == os.path.getsize(tmpf)
if tempAndLatestFileSameSize:
    print("Downloaded file is not an update. Removing '"+tmpf+"''")
    os.remove(tmpf)
    exit(1)


# In[7]:


os.rename(tmpf,sys.argv[3])


# In[14]:


# https://services1.arcgis.com/CY1LXxl9zlJeBuRZ/ArcGIS/rest/services

