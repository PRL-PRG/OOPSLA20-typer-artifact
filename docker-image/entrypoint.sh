#!/bin/bash
set -e

# unfortunatelly, the following does not work with GNU parallel --workdir
# if [ "$1" == "/init" ]; then
#     # rstudio takes care of the permissions
#     exec "$@"
# fi
#
# # if both not set we do not need to do anything
# if [ -z "${USERID}" -a -z "${GROUPID}" ]; then
#     echo "WARN: Not using USERID and GROUPID."
#     echo "WARN: This is not recommended as it likely lead to file permissions problems";
# else
#     groupmod -g ${USERID} rstudio
#     usermod -u ${USERID} -g ${GROUPID} rstudio
# fi
#
# sudo -u rstudio \
#      PATH=$PATH \
#      CRAN_LOCAL_MIRROR=$CRAN_LOCAL_MIRROR \
#      GROUPID=$GROUPID \
#      IN_DOCKER=$IN_DOCKER \
#      OMP_NUM_THREADS=$OMP_NUM_THREADS \
#      PACKAGES_SRC_DIR=$PACKAGES_SRC_DIR \
#      PACKAGES_ZIP_DIR=$PACKAGES_ZIP_DIR \
#      RDT_DIR=$RDT_DIR \
#      R_COMPILE_PKGS=$R_COMPILE_PKGS \
#      R_DIR=$R_DIR \
#      R_DISABLE_BYTECODE=$R_DISABLE_BYTECODE \
#      R_ENABLE_JIT=$R_ENABLE_JIT \
#      R_KEEP_PKG_SOURCE=$R_KEEP_PKG_SOURCE \
#      R_LIBS=$R_LIBS \
#      R_PROJECT_BASE_DIR=$R_PROJECT_BASE_DIR \
#      R_VERSION=$R_VERSION \
#      USERID=$USERID \

exec "$@"
