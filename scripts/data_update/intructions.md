# How to Generate New Data  

Files that need to be ran:
1. Process_voucher_area_data.R
2. Create_full_dataset.R


## Process
1. Process_voucher_area_data.R  
- download counties
- filter the ones you need, intersect with school disctrit polygons. 

- until line 170 -> defining geomtery.

**here are the NJ school discticts:**
https://services1.arcgis.com/Ua5sjt3LWTPigjyD/arcgis/rest/services/School_Districts_Current/FeatureServer/0/query?geometry={"xmin":-74.22499455246528,"ymin":40.47202439692057,"xmax":-73.85351933273871,"ymax":40.83667117059111}&geometryType=esriGeometryEnvelope&spatialRel=esriSpatialRelIntersects&outFields=*&returnGeometry=true&f=json&outSR=4326