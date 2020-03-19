#!/usr/bin/env python
# coding: utf-8

# In[21]:


import requests, json, csv, datetime, sys
from collections import OrderedDict


# In[22]:


url = "https://services1.arcgis.com/CY1LXxl9zlJeBuRZ/ArcGIS/rest/services/Detailed_COVID_19_Case_Data/FeatureServer/0/query?f=json&where=1%3D1&returnGeometry=false&outFields=*"
j = requests.get(url).json()


# In[36]:


cases = [obj["attributes"] for obj in j["features"]]
headers = cases[0].keys()
data = []
for case in cases:
    data.append(OrderedDict(case))


# In[37]:


with open(sys.argv[1], "w", newline="") as outfile:
    writer = csv.writer(outfile)
    writer.writerow(list(cases[0].keys()))
    for row in cases:
        writer.writerow(list(row.values()))


# In[14]:


# https://services1.arcgis.com/CY1LXxl9zlJeBuRZ/ArcGIS/rest/services

