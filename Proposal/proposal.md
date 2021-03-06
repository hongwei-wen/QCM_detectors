##Background & Goal
Alchohols have different structures, and are used frequently in hygiene products and 
cosmetics. Since some of them are harmful for people, successful detection of different 
alchohol types is important and necessary.  

In this study, five different types of alcohol are detected by five quartz crystal 
microbalance (QCM) sensors with different structures. 
The main goal of the study is to use measurement of the QCM sensors 
to construct a data-driven model to classify five types of alchohol. 
Moreover, we compare the accuracy of five QCM sensors and determine the best one. 


##Data Description
###Introduction of data source:
Five different QCM gas sensors are used, and five different gas measurements
(1-octanol, 1-propanol, 2-butanol, 2- propanol and 1-isobutanol) are conducted in each of
these sensors. A QCM is an electromechanical oscillator that contains a thin slice of 
quartz crystal with chemical receptive material placed on its surface.
The measurements of frequency reduction in the oscillation are our samples.
There are two different channels in these QCM sensors. One of these channel includes 
molecularly imprinted polymers (MIP), and the other includes nanoparticles (NP). 
Diverse QCM sensor structures are obtained using different MIP and NP ratios.
For each type of alchohol gas, we mix it and air with different ratio, 
ranging from 0.799/0.201 to 0.400/0.6000. 

###Data description
For each QCM detector, we have ten measurements of frequency reduction and 
and a five-dimension one-hot vector to show the type of achohol. The number of observations
is 25 for each QCM detector. Now we briefly show several data from QCM10 dataset.

```R
>>head(QCM10)
      `0.799_0.201` `0.799_0.201_1` `0.700_0.300` `0.700_0.300_1` `0.600_0.400` `0.600_0.400_1`
          <dbl>           <dbl>         <dbl>           <dbl>         <dbl>           <dbl>
1         -12.0           -11.0         -19.1           -17.3         -33.1           -28.4
2         -12.2           -11.3         -22.3           -20.0         -39.8           -33.6
3         -12.6           -11.7         -26.7           -23.3         -46.5           -38.7
4         -13.8           -12.8         -30.6           -26.2         -52.3           -43.0
5         -15.7           -13.9         -34.5           -28.6         -57.4           -46.3
6         -58.4           -38.7         -83.6           -57.3        -110.            -76.8

 
      `0.501_0.499` `0.501_0.499_1` `0.400_0.600` `0.400_0.600_1` `1-Octanol` `1-Propanol`
          <dbl>           <dbl>         <dbl>           <dbl>         <dbl>        <dbl>
1         -48.8           -40.8         -62.5           -50.8           1            0
2         -56.9           -46.8         -73.3           -59.0           1            0
3         -66.0           -53.5         -84.5           -67.2           1            0
4         -73.8           -59.2         -94.4           -74.4           1            0
5         -80.4           -63.5        -103.            -80.2           1            0
6        -134.            -96.1        -171.           -124.            0            1

        `2-Butanol` `2-propanol` `1-isobutanol`
           <dbl>        <dbl>          <dbl>
1           0            0              0
2           0            0              0
3           0            0              0
4           0            0              0
5           0            0              0
6           0            0              0
```


##Explorative data analysis
###Data preprocessing
Convert five dataset into the form of dataframe and transform the one-hot vector into label.
Also some other work such as renaming columns and variable.

```R
library(dplyr)
library(readr)
library(stringr)
first_category_name = list.files("Desktop/textbook/QCM_Sensor_Alcohol_Dataset") 
dir = paste("./Desktop/textbook/QCM_Sensor_Alcohol_Dataset/",first_category_name,sep="")
for(i in 1:length(dir)){
  index = str_sub(first_category_name[i],1,-5)
  data_index = data.frame(read_csv(dir[i]))
  print(names(data_index))
  names(data_index) = c("ch1p0.8", "ch2p0.8", "ch1p0.7","ch2p0.7","ch1p0.6","ch2p0.6","ch1p0.5","ch2p0.5","ch1p0.4","ch2p0.4","Oct1","Pro1","But2","pro2","iso1")
  data_index <- mutate(data_index,labels = Oct1*1+Pro1*2+But2*3+pro2*4+iso1*5)
  assign(index, data_index)
  #print(head(get(index)))
}
```

###Data exploring
Based on measurement of QCM10, we summarize the average frequency reduction of channel 1
for five types of alchohol with different gas ratio.
```R
>> select(QCM10,-(Oct1:iso1)) %>% group_by(labels) %>% summarize(m0.8=mean(ch1p0.8),
m0.7=mean(ch1p0.7), m0.6=mean(ch1p0.6), m0.5=mean(ch1p0.5), m0.4=mean(ch1p0.4))
  labels  m0.8  m0.7   m0.6   m0.5   m0.4
   <dbl> <dbl> <dbl>  <dbl>  <dbl>  <dbl>
1      1 -13.2 -26.6  -45.8  -65.2  -83.5
2      2 -59.9 -84.5 -108.  -128.  -157. 
3      3 -61.3 -93.8 -131.  -163.  -201. 
4      4 -44.5 -66.4  -82.1  -93.8 -107. 
5      5 -54.0 -77.8 -113.  -156.  -204. 
```
A obvious decreasing trend can be observed as the ratio varies. 

Under the condition of same detector, we summarize the average frequency reduction for both
two channel. 
```R
> select(QCM10,-(Oct1:iso1)) %>% group_by(labels) %>% summarize(m1p0.8=mean(ch1p0.8), 
m2p0.8=mean(ch2p0.8), m1p0.6=mean(ch1p0.6), m2p0.6=mean(ch2p0.6), m1p0.4=mean(ch1p0.4), 
m2p0.4=mean(ch2p0.4))
# A tibble: 5 x 7
  labels m1p0.8 m2p0.8 m1p0.6 m2p0.6 m1p0.4 m2p0.4
   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
1      1  -13.2  -12.2  -45.8  -38.0  -83.5  -66.3
2      2  -59.9  -40.1 -108.   -75.9 -157.  -116. 
3      3  -61.3  -62.5 -131.  -106.  -201.  -150. 
4      4  -44.5  -35.2  -82.1  -62.1 -107.   -80.8
5      5  -54.0  -42.4 -113.   -87.0 -204.  -158. 
```
It seems that the measurement of two channel under the same ratio is similar. 

Next we compare the result of different QCM detector under the same channel and same alchohol.
```R
> qcm10ch1 = select(QCM10,-(Oct1:iso1)) %>% group_by(labels) %>% summarize(m0.8=mean(ch1p0.8), m0.6=mean(ch1p0.6), m0.4=mean(ch1p0.4))
> qcm3ch1 = select(QCM3,-(Oct1:iso1)) %>% group_by(labels) %>% summarize(m0.8=mean(ch1p0.8), m0.6=mean(ch1p0.6), m0.4=mean(ch1p0.4))
> mutate(data.frame(qcm10ch1), qcm3m0.8 = data.frame(qcm3ch1)[,2], qcm3m0.6 = data.frame(qcm3ch1)[,3], qcm3m0.4 = data.frame(qcm3ch1)[,4])
  labels    m0.8     m0.6     m0.4 qcm3m0.8 qcm3m0.6 qcm3m0.4
1      1 -13.246  -45.834  -83.538  -12.520  -35.324  -72.060
2      2 -59.898 -108.002 -157.390  -77.352 -169.522 -234.402
3      3 -61.330 -131.434 -201.210  -91.412 -180.750 -263.004
4      4 -44.482  -82.072 -106.902  -67.316 -119.478 -174.800
5      5 -54.020 -112.906 -203.842  -66.224 -152.242 -267.434
```
The trends between QCM10 and QCM3 detectors are similar. Distinct meausurement between 
different types of alchohol gives detectors chance to classify correctly.

