
# 📕️ Development of automatic bird acoustic analysis systems

## 📑️ About

The main goal of this project is to develop an automatic system that identifies and classifies the acoustic signals of the nocturnal bird species that nest in Catalonia. Additionally, the secondary goal is to create a classifier that can distinguish between the presence and absence of bird acoustic signals in order to enhance the first classifier's prediction.

In order to meet these objectives, a classifier system has been implemented based on training data obtained from the 14 species of nocturnal birds as well as other background sounds. This tool has been build with R --a language and environment for statistical computing and graphics R -- and it will allow the scientific research community in bioacoustics and ornithologists to analyze large amounts of data from the sounds they collect during the nights, as several research groups at the museum (especially the chiropteran one) record many gigabytes of audio files.

This project has been developed by [@esther-amores](https://www.github.com/esther-amores) in collaboration with the Granollers Museum of Natural Sciences as a result of the Final Data Science Master’s Project at the University of Girona.


## 🛠️ Setup

This project has been developed to execute in Ubuntu 20.04. 
Follow these steps in order to prepare your environment.

### 📊️ Install R and RStudio

```bash
sudo apt-get update
sudo apt-get upgrade
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/'
sudo apt-get install r-base r-base-dev
```
    
### 📊️ Install software to make R packages work correctly

```bash
sudo apt-get install fftw3 fftw3-dev pkg-config
sudo apt-get install cmake
sudo apt-get install libxml2-dev libssl-dev libcurl4-openssl-dev
sudo apt-get install -y libavfilter-dev
sudo apt-get install ffmpeg
```

### 🐍️ Install Python 3

```bash
sudo apt-get update
sudo apt-get install python3-dev python3-pip
sudo pip3 install --upgrade pip
```

### 🐍️ Install Python requirements

```bash
pip install -r requirements.txt
sudo pip3 install librosa
```

### 📼️ Install extras to reproduce .mp4 files

```bash
sudo apt install ubuntu-restricted-extras
```
