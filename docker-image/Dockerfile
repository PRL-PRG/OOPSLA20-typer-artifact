FROM rocker/rstudio:3.5

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
      texlive \
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
    wget "https://raw.githubusercontent.com/PRL-PRG/runr/typer-oopsla20/inst/create-cran-snapshot.R" && \
    chmod +x *.sh *.R

env R_VERSION=3.5.0 \
    R_PROJECT_BASE_DIR=/R

env R_DIR=/$R_PROJECT_BASE_DIR/R-$R_VERSION \
    RDT_DIR=/$R_PROJECT_BASE_DIR/R-dyntrace \
    R_LIBS=/$R_PROJECT_BASE_DIR/library/$R_VERSION \
    PACKAGES_SRC_DIR=/$R_PROJECT_BASE_DIR/CRAN/extracted \
    PACKAGES_ZIP_DIR=/$R_PROJECT_BASE_DIR/CRAN/src/contrib \
    CRAN_LOCAL_MIRROR=file:///$R_PROJECT_BASE_DIR/CRAN \
    R_KEEP_PKG_SOURCE=1 \
    R_ENABLE_JIT=0 \
    R_COMPILE_PKGS=0 \
    R_DISABLE_BYTECODE=1 \
    OMP_NUM_THREADS=1

# R
RUN ./install-r.sh -d $R_DIR -v $R_VERSION

# R dyntrace
RUN ./install-r.sh -d $RDT_DIR -s https://github.com/PRL-PRG/R-dyntrace/archive/r-$R_VERSION.tar.gz

env PATH=$R_DIR/bin:$PATH

# CRAN mirror
ADD CRAN-packages.txt /tmp
RUN ./create-cran-snapshot.R "$PACKAGES_ZIP_DIR" /tmp/CRAN-packages.txt

RUN mkdir -p "$PACKAGES_SRC_DIR" && \
    ls -1 "$PACKAGES_ZIP_DIR"/*.tar.gz | xargs -L 1 tar -C "$PACKAGES_SRC_DIR" -xzf

# R packages
ADD CRAN-packages.txt .
RUN ls -1 "$PACKAGES_SRC_DIR" | ./install-cran-packages.sh -l $R_LIBS -m "$CRAN_LOCAL_MIRROR" -f -

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
RUN curl -L https://github.com/AlDanial/cloc/releases/download/1.86/cloc-1.86.pl -o /usr/bin/cloc && \
    chmod +x /usr/bin/cloc

# paper tooling
RUN git clone https://github.com/PRL-PRG/injectr --branch typer-oopsla20 && \
    R CMD INSTALL injectr

RUN git clone https://github.com/PRL-PRG/tastr --branch typer-oopsla20 && \
    make -C tastr CXX=g++

ARG CONTRACTR_VERSION=unknown
RUN git clone https://github.com/PRL-PRG/contractr --branch typer-oopsla20 && \
    R CMD INSTALL contractr

RUN git clone https://github.com/PRL-PRG/propagatr --branch typer-oopsla20 && \
    $RDT_DIR/bin/R CMD INSTALL propagatr

ARG RUNR_VERSION=unknown
RUN git clone https://github.com/PRL-PRG/runr --branch typer-oopsla20 && \
    R CMD INSTALL runr

WORKDIR /

env IN_DOCKER=1

# configure entrypoint
ADD entrypoint.sh /
RUN chmod +x /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
