
<!-- README.md is generated from README.Rmd. Please edit the .Rmd file then knit it -->

# Trend analysis

Latest report:
[WG-SAM-2022/08](https://meetings.ccamlr.org/en/wg-sam-2022/08)

## Annual updates

There are several things to update annually. Some things might just need
to be reviewed, others might need updating.

1.  Review/update the shapefile of buffered Research Blocks.

In the subfolder *\~/Data*, the shapefile “BufferedRBs” contains the RBs
after buffering (+5km). It is generated using *BufferRBs.R* which also
produces a plot (“BufferedRBs.png”) to look at the buffers, which get a
bit special when RBs have shared boundaries (shown in blue/orange on the
plot - those in green do not share boundaries with other RBs). If a new
RB has been created, or an existing RB has been modified or removed, you
will need to update the shapefile.

2.  Review/update the shapefile of Reference Areas (HIMI and RSR_Open).

In the subfolder *\~/Data*, the shapefiles “RefAreas” and “RefAreasLL”
contain the reference areas. These are generated using
*RefArea_Shp_Maker.R*. “RefAreasLL” is the unprojected version, used to
compute fishable area within the script *FishableArea.R* (see
[WG-FSA-2021/06](https://www.ccamlr.org/en/wg-fsa-2021/06) for details).

3.  Review/update Fishable areas.

In the subfolder *\~/Data*, the csv file “FishableAreaYYYY.csv” contains
the fishable area of RBs and Reference Areas. It is generated using
*FishableArea.R*. This needs to be updated when those areas are updated
and/or when the GEBCO bathymetry is updated. If fishable areas have been
updated, the effect of the update must be analyzed.

4.  Review/update Map and Area Thumbnails.

In the subfolder *\~/Data*, the images *Map_TrendAnalysis.png*,
*Area48.png*, *Area58.png* and *Area88.png* are generated using
*Map_and_AreaThumbnails.R*. These images are used in the report and
trend plots, and may be updated in that script, if needed.

5.  Update past catch limits.

Every year new catch limits are agrred upon. Record the previous catch
limits in the file *CLs.csv* by adding a column every year. If a new RB
is created, add a row for it.

6.  Review/update vulnerable biomass estimates

Once stock assessments in the Reference Areas have been agreed by the
Commission, contact the authors (might as well do it during FSA) and ask
them for these values. They go in the script *00_RunAnalysis.R*, under
“\#Set biomass and CV for Reference Areas”. Record these values also in
*01_LoadData.R* for posterity.
