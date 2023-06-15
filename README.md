<div align="justify">

## **General information**

**Title:** The contribution of 2D and 3D geometric morphometrics to
lithic taxonomies: testing discrete categories of backed flakes from
recurrent centripetal core reduction.  
**Authors:**

- Guillermo Bustos-Pérez $^{(1,2,3,7)}$  
- Brad Gravina $^{(4,5)}$  
- Michel Brenet $^{(5, 6)}$  
- Francesca Romagnoli $^{(1,7)}$

$^{(1)}$ Departamento de Prehistoria y Arqueología, Universidad Autónoma
de Madrid, Campus de Cantoblanco, 28049, Madrid, Spain  
$^{(2)}$ Institut Català de Paleoecologia Humana i Evolució Social
(IPHES-CERCA), Zona Educacional 4, Campus Sescelades URV (Edifici W3),
43007 Tarragona, Spain  
$^{(3)}$ Universitat Rovira i Virgili, Departament d’Història i Història
de l’Art, Avinguda de Catalunya 35, 43002 Tarragona, Spain  
$^{(4)}$ Musée national de Préhistoire, MC, 1 rue du Musée, 24260 Les
Eyzies de Tayac, France  
$^{(5)}$ Univ. Bordeaux, CNRS, MC, PACEA, UMR-5199, FF-33600 Pessac,
France  
$^{(6)}$ INRAP Grand Sud-Ouest, Centre mixte de recherches
archéologiques, Domaine de Campagne, 242460 Campagne, France  
$^{(7)}$ Corresponding authors at: <guillermo.bustos@uam.es> \|
<francesca.romagnoli@uam.es>  
**ORCID**: 0000-0002-1089-818X

**Abstract**  
Paleolithic lithic assemblages are usually dominated by flakes, which
display a high degree of morphological variability. When analyzing
Paleolithic lithic assemblages, it is common to classify flakes into
categories based on their morphological and technological features,
which are linked to the position of the flake in a reduction sequence
and how removals are organized in a given production method. For the
analysis of Middle Paleolithic lithic assemblages, two categories of
flakes are commonly used: core edge flakes and pseudo-Levallois points.
A third type, core edge flakes with a limited back, is also commonly
found in the archaeological literature, providing an alternative
category with a definition that does not match the two previous types
but shares many of their morphological and technological features. The
present study addresses whether these three flakes constitute discrete
categories based on their morphological and technological attributes.
Geometric morphometrics are employed on an experimental set composed of
the three categories of flakes to quantify morphological variation.
Machine learning models and principal components biplots are used to
test the discreteness of the categories. The results indicate that
geometric morphometrics succeed in capturing the morphological and
technological features that characterize each type of product.
Pseudo-Levallois points have the highest discreteness of the three
technological products, and while some degree of mixture exists between
core edge flakes and core edge flakes with a limited back, they are also
highly distinguishable. We conclude that the three categories are
discrete and can be employed in technological lists of products for the
analysis of lithic assemblages and that geometric morphometrics is
useful for testing for the validity of categories.

**Key words:** lithic analysis; lithic technology; geometric
morphometrics; machine learning; Middle Paleolithic; Levallois;
discoidal

**Funding**  
This research has been supported by the project SI1/PJI/2019-00488
funded by Comunidad Autónoma de Madrid and Universidad Autónoma de
Madrid.

## **Access information**

**License**  
The following data and code is publish under a **Non Commercial 4.0
International (CC BY-NC 4.0) license**. For more details, see the
[**License**](License.md) file.  
![](Report/Figures/License.png)

**Link to related datasets**

3D meshes and coordinates are freely available as a Zenodo repository
at:  
Guillermo Bustos-Pérez, Brad Gravina, Michel Brenet, & Francesca
Romagnoli. (2022). Research compendium: Combining quantitative
approaches to differentiate between backed products from discoidal and
Levallois reduction sequences. In Journal of Archaological Science:
Reports (1.0). Zenodo. <https://doi.org/10.5281/zenodo.7085139>

## **Methodological information**

All flakes were scanned with an Academia 20 structured light surface
scanner (Creaform 3D) at a 0.2 mm resolution. Flakes were scanned in two
parts, automatically aligned (or manually aligned in the case automatic
alignment failure), and exported in STL format. Cloudcompare 2.11.3
(<https://www.danielgm.net/cc/>) free software was employed to perform
additional cleaning, mesh sampling, surface reconstruction and
transformation into PLY files. Finally, all files were decimated to a
quality of 50,000.

3D landmarks of each artifact were placed using Viewbox Version 4.1.0.12
(<http://www.dhal.com/viewbox.htm>). Placement of landmarks was done
using a flake template (available at
<https://doi.org/10.5281/zenodo.7085139>) and landmarks were relaxed to
minimize bending energy. Resulting point coordinates were exported into
.xlsx files.

2D Geometric morphometrics was performed using screenshots of the upper
view of each flake orientated along the technological axis and using
free software MeshLab (<https://www.meshlab.net/>). One thin-plate
spline (tps) was generated using tpsUtil v.1.82, and the tpsDig v.2.32
(<https://www.sbmorphometrics.org/index.html>).

## **Structure of the repository**

The present repository has been organized following the structure of a
research compendium. The folder [Report](Report) contains the following
files:

- The report containing the complete workflow and text is available as a
  [Github markdown](Report/Backed-Flakes-Categories.md) or as an
  [RMarkdown](Report/Backed-Flakes-Categories.Rmd).

- [Data](Report/Data) folder containing the following files:

  - [Attributes data](Report/Data/Attributes%20data.csv): a .csv file
    containing all data of atribute analysis of the sample and their
    categories.  
  - [2D-Upper-view](Report/Data/Attributes%20data.TPS:) a TPS file
    containing the 2D coordinates of the perimeter of the upper view of
    each specimen. Each specimen has a landmarks per pixel, and thud
    having different number of landmarks. To resample each specimen into
    having the same number of landmarks, the [21 Muggle 2D
    data](Report/Scripts/21%20Muggle%202D%20data.R): R script is
    employed.  
  - [GM csvs](Report/Data/GM%20csvs) folder containing all .csv’s with
    the 3D coordinates of all specimens.  
  - [2D Data](Report/Data/2D%20data.RData): an .RData file containing a
    *nosymproc* object. This object is the result of performing
    procrusties alignment to the 2D coordinates using package Morpho.
    The *nosymproc* object contains rewritten (and original)
    coordinates, results from PCA, and interpretation of PC.
    - This file is obtained sourcing the [21 Muggle 2D
      data](Report/Scripts/21%20Muggle%202D%20data.R) R script.  
  - [3D Data](Report/Data/3D%20data.RData): an .RData file containing a
    *nosymproc* object. This object is the result of performing
    procrusties alignment to the 3D coordinates using package Morpho.
    The *nosymproc* object contains rewritten (and original)
    coordinates, results from PCA, and interpretation of PC.
    - This file is obtained sourcing the [22 Muggle 3D
      data](Report/Scripts/22%20Muggle%203D%20data.R) R script.  
  - [2D Results Up and Down
    sampling](Report/Data/2D%20Results%20Up%20and%20Down%20sampling.RData):
    an .RData file containing the results of each model resampling using
    the 2D data.
    - This data is obtained sourcing the [31 Models on 2D
      data](Report/Scripts/31%20Models%20on%202D%20data.R) R script.  
  - [3D Results Up and Down
    sampling](Report/Data/3D%20Results%20Up%20and%20Down%20sampling.RData):
    an .RData file containing the results of each model resampling using
    the 3D data.
    - This data is obtained sourcing the [32 Models on 2D
      data](Report/Scripts/32%20Models%20on%203D%20data.R) R script.  
  - [Best model 2D varimp and
    cm](Report/Data/Best%20model%202D%20varimp%20and%20cm.RData):
    results of variable importance and predictions after 30 cycles of
    K-fold cross validation on the model that performed best on the 2D
    data.
    - This data is obtained sourcing the [33 Loop over best model of 2D
      data](Report/Scripts/33%20Loop%20over%20best%20model%20of%202D%20data.R)
      R script.  
  - [Best model 3D varimp and
    cm](Report/Data/Best%20model%203D%20varimp%20and%20cm.RData):
    results of variable importance and predictions after 30 cycles of
    K-fold cross validation on the model that performed best on the 3D
    data.
    - This data is obtained sourcing the [34 Loop over best model of 3D
      data](Report/Scripts/34%20Loop%20over%20best%20model%20of%203D%20data.R)
      R script.

- [Figures](Report/Figures) folder containing figures included in the
  manuscript.  

- [Scripts](Report/Scripts) folder containing scripts employed to muggle
  the original data, or train the models.  

- [References](Report/References.bib): a .bib file containing references
  cited in the text.

</div>
