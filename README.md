
# 📕️ Development of automatic bird acoustic analysis systems

A brief description of what this project does and who it's for


## 📑️ About

This project has been developed by [@esther-amores](https://www.github.com/esther-amores) in collaboration with the Granollers Museum of Natural Sciences as a result of the Final Data Science Master’s Project at the University of Girona.


## 🛠️ Setup

This project has been developed to execute in Ubuntu 20.04. (copiar el que ja tinc al word)
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

### 🐍️ Install tensorflow 

```bash
sudo apt-get install build-essential cmake git unzip \pkg-config libopenblas-dev liblapack-dev
```

### 📼️ Install extras to reproduce .mp4 files

```bash
sudo apt install ubuntu-restricted-extras
```

### Clone the repository 
Run first line just if you don't have Git installed.

```bash
sudo apt install git
git clone (name)
cd (name)
```
