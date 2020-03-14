# mas

<!-- badges: start -->

<!-- badges: end -->

<!-- The goal of mas is to ... -->

The goal of this project to track all code and documentation necessary for my master thesis in data science at Zurich University of Applied Sciences (ZHAW).

## Setup / Data
This project uses stock price data from yahoo finance. This dataset contains several million rows of data and is too large to be included in this repo. Therefore the data must be fetched once using the function download "download_stocks()". This script will download the required data and copy it into the folder “data”. This takes about 35 minutes to complete. After the data is available on your system other scripts will run as expected.
