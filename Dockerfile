FROM rocker/r-ver:4.1.3

RUN apt-get update 
RUN apt-get install -y libcurl4-openssl-dev libxml2-dev libssl-dev libpng-dev

RUN apt-get install -y wget git tar

# Set timezone
ENV TZ=America/New_York
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# System requirements for R packages
RUN apt-get install -y postgresql-server-dev-all

# Install needed R packages
RUN install2.r --skipinstalled --ncpus -1 \
tidyverse \
slackr \
janitor \
kableExtra \
aws.s3 \
googlesheets4 \
config \
palmerpenguins \
pander \
    && rm -rf /tmp/downloaded_packages \
    && strip /usr/local/lib/R/site-library/*/libs/*.so
  
copy . .


# Set ENV defaults that are overwritten by job ENV 
ENV message = "Hi"


CMD ["Rscript", "do_something_in_R.R"]

