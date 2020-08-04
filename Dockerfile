FROM rocker/rstudio:3.5

env NO_DISPLAY=1

# common devel dependencies
RUN DEBIAN_FRONTEND=noninteractive \
    apt-get update -yqq && \
    apt-get install -yqq \
      build-essential \
      cargo \
      cloc \
      curl \
      default-jdk \
      flex \
      libavfilter-dev \
      libbz2-dev \
      libcairo2-dev \
      libfftw3-dev \
      libgdal-dev \
      libglpk-dev \
      libglu1-mesa-dev \
      libgmp3-dev \
      libgsl-dev \
      libhiredis-dev \
      libjpeg-dev \
      liblzma-dev \
      libmpfr-dev \
      libmagick++-dev \
      libmariadbclient-dev \
      libpcre2-dev \
      libpcre3-dev \
      libpoppler-cpp-dev \
      libpq-dev \
      libreadline-dev \
      librsvg2-dev \
      libsodium-dev \
      libudunits2-dev \
      libwebp-dev \
      libxml2-dev \
      rsync \
      tree \
      unixodbc-dev \
      vim \
      wget \
      x11-utils \
      xorg-dev \
      xvfb \
      zlib1g-dev

WORKDIR /R

# bootstrap scripts
RUN wget "https://raw.githubusercontent.com/PRL-PRG/runr/typer-oopsla20/inst/install-r.sh" && \
    wget "https://raw.githubusercontent.com/PRL-PRG/runr/typer-oopsla20/inst/install-cran-packages.sh" && \
    chmod +x *.sh

# set up environments
ADD envir /
ADD envir-dyntrace /

# R
RUN . /envir && ./install-r.sh

# R dyntrace
RUN . /envir-dyntrace && ./install-r.sh

# CRAN mirror
ADD CRAN-packages-urls.txt /tmp
RUN . /envir && \
    mkdir -p "$PACKAGES_ZIP_DIR" && \
    cd "$PACKAGES_ZIP_DIR" && \
    cat /tmp/CRAN-packages-urls.txt | xargs -P 4 -L 1 curl -O && \
    R --slave -e 'tools::write_PACKAGES(".", type="source", verbose=T)'

RUN . /envir && \
    mkdir -p $PACKAGES_SRC_DIR && \
    ls -1 $PACKAGES_ZIP_DIR/*.tar.gz | xargs -L 1 tar -C "$PACKAGES_SRC_DIR" -xzf

# R packages
ADD packages-base.txt .
ADD packages-corpus.txt .
RUN . /envir && \
    cat packages-base.txt packages-corpus.txt > packages.txt && \
    ./install-cran-packages.sh -m "$CRAN_LOCAL_MIRROR" -f packages.txt

## The following are dependencies that are too outdated in debian

# GNU parallel
RUN mkdir parallel && \
    cd parallel && \
    curl http://ftp.gnu.org/gnu/parallel/parallel-latest.tar.bz2 | tar -xjf- --strip 1 && \
    ./configure && \
    make install && \
    mkdir ~/.parallel && \
    touch cat ~/.parallel/will-cite

# GNU m4
RUN mkdir m4 && \
    cd m4 && \
    curl https://ftp.gnu.org/gnu/m4/m4-latest.tar.bz2 | tar -xjf- --strip 1 && \
    ./configure && \
    make install

# GNU bison
RUN mkdir bison && \
    cd bison && \
    curl https://ftp.gnu.org/gnu/bison/bison-3.5.tar.gz | tar -xzf- --strip 1 && \
    ./configure && \
    make install

# cloc
RUN curl https://github.com/AlDanial/cloc/releases/download/1.86/cloc-1.86.pl -o /usr/bin/cloc && \
    chmod +x /usr/bin/cloc

# paper tooling
RUN . /envir && \
    git clone https://github.com/PRL-PRG/injectr --branch typer-oopsla20 && \
    R CMD INSTALL injectr

RUN . /envir && \
    git clone https://github.com/PRL-PRG/tastr --branch typer-oopsla20 && \
    make -C tastr CXX=g++

RUN . /envir && \
    git clone https://github.com/PRL-PRG/contractr --branch typer-oopsla20 && \
    R CMD INSTALL contractr

RUN . /envir-dyntrace && \
    git clone https://github.com/PRL-PRG/propagatr --branch typer-oopsla20 && \
    R_HOME="$R_BASE_DIR" R CMD INSTALL propagatr

RUN . /envir && \
    git clone https://github.com/PRL-PRG/runr --branch typer-oopsla20 && \
    R CMD INSTALL runr

WORKDIR /

# configure entrypoint
ADD entrypoint.sh /
RUN chmod +x /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
