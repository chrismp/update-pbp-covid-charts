#!/usr/bin/env python
# coding: utf-8

# In[37]:


import requests, json, csv, datetime, sys
from collections import OrderedDict


# In[38]:


url = "https://services1.arcgis.com/CY1LXxl9zlJeBuRZ/ArcGIS/rest/services/Florida_COVID19_Cases_by_County/FeatureServer/0/query?f=json&where=1%3D1&returnGeometry=false&outFields=*"
content = requests.get(url).content
data = json.loads(content)
timestamp = datetime.datetime.now().strftime("%Y-%m-%d_%H%M")


# In[39]:


masterlist = []
for i, county in enumerate(data['features']):
    countyname = data['features'][i]['attributes']['County']
    countypeople = data['features'][i]['attributes']['Cases']
    for person in countypeople.split("||"):
        line = OrderedDict()
        line['county'] = countyname
        line['gender'] = person.split(",")[0].split(":")[1].strip()
        line['age'] = person.split(",")[1].split(":")[1].strip()
        line['travel'] = person.split(",")[2].split("?")[1].strip()
        masterlist.append(line)


# In[40]:

filename = sys.argv[1] + "/FL-" + timestamp + ".csv"
with open(filename, "w", newline="") as outfile:
    writer = csv.writer(outfile)
    writer.writerow(list(masterlist[0].keys()))
    for row in masterlist:
        writer.writerow(list(row.values()))


# In[41]:


# https://services1.arcgis.com/CY1LXxl9zlJeBuRZ/ArcGIS/rest/services

