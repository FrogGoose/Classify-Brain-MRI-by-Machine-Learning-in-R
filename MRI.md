Brain MRI Segmentation
================
MA
May 2, 2018

This project attempts to show how to use R and different machine leanring algorithms to classify Brain MRI.

First, let us take a look at the original MRI.

``` r
library(grid)
library(png)
Original.MRI=readPNG("C:/Users/mym/Documents/Git-R/Brain MRI/Classify-Brain-MRI-by-Machine-Learning-in-R/110_2_orig.png")
grid.raster(Original.MRI)
```

![](MRI_files/figure-markdown_github/unnamed-chunk-1-1.png)

And take a look at the associated segmented MRI:

``` r
Segmented.MRI=readPNG("C:/Users/mym/Documents/Git-R/Brain MRI/Classify-Brain-MRI-by-Machine-Learning-in-R/110_2_seg.png")
grid.raster(Segmented.MRI)
```

![](MRI_files/figure-markdown_github/unnamed-chunk-2-1.png)

Our task is to segment from the original MRI grey matter, white matter and csf as showed in blue, green and red in the segmented MRI.

Before we go any further, we need to know how the MRI stored in computer in terms of data. Namely, what the Original.MRI look like? In fact, Original.MRI is an array with 3 dimensions, or we can say it consists of 3 layers of indicating Red, green and blue respectively and each layer is a 2 dinension matrix indicating the location of each pixel. In general, the value in the array is between 0 and 1 or 0 and 255.

In order to understand the structure of MRI data, let take advantage of segmented MRI to display grey matter, white matter and csf from the original MRI respectively.

``` r
Segmented.MRI.Integer=Segmented.MRI*255 #original value is between 0 and 1. This converts them into integers
#copy the original MRI 3 times
Original.MRI.white.matter=Original.MRI
Original.MRI.grey.matter=Original.MRI
Original.MRI.csf=Original.MRI
#use the segmented MRI to display those three matters respectively from the original MRI.
for (i in (1:dim(Original.MRI)[1])){
  for (j in (1:dim(Original.MRI)[2])){
    Original.MRI.white.matter[i,j,]=ifelse(all(Segmented.MRI.Integer[i,j,1:3]==c(0,255,0)),c(1,1,1),c(0,0,0))
    Original.MRI.grey.matter[i,j,]=ifelse(all(Segmented.MRI.Integer[i,j,1:3]==c(0,0,255)),c(1,1,1),c(0,0,0))
    Original.MRI.csf[i,j,]=ifelse(all(Segmented.MRI.Integer[i,j,1:3]==c(255,0,0)),c(1,1,1),c(0,0,0))
  }
}
#display white matter from the original MRI
grid.raster(Original.MRI.white.matter)
```

![](MRI_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
#display grey matter from the original MRI
grid.raster(Original.MRI.grey.matter)
```

![](MRI_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
#display csf from the original MRI
grid.raster(Original.MRI.csf)
```

![](MRI_files/figure-markdown_github/unnamed-chunk-5-1.png)

Now get the data to be classified ready.
