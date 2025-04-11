########################################################
#        Renku install section - do not edit           #

FROM renku/renkulab-r:4.3.1-0.25.0 as builder

# RENKU_VERSION determines the version of the renku CLI
# that will be used in this image. To find the latest version,
# visit https://pypi.org/project/renku/#history.
ARG RENKU_VERSION={{ __renku_version__ | default("2.7.0") }}

# Install renku from pypi or from github if a dev version
RUN python3 -m venv .renku/venv && \
    . .renku/venv/bin/activate && \
    pip install --upgrade pip && \
    if [ -n "$RENKU_VERSION" ] ; then \
        currentversion=$(renku --version || echo "none") ; \
        if [ "$RENKU_VERSION" != "$currentversion" ] ; then \
            pip uninstall renku -y || true ; \
            gitversion=$(echo "$RENKU_VERSION" | sed -n "s/^[[:digit:]]\+\.[[:digit:]]\+\.[[:digit:]]\+\(rc[[:digit:]]\+\)*\(\.dev[[:digit:]]\+\)*\(+g\([a-f0-9]\+\)\)*\(+dirty\)*$/\4/p") ; \
            if [ -n "$gitversion" ] ; then \
                pip install --no-cache-dir --force "git+https://github.com/SwissDataScienceCenter/renku-python.git@$gitversion" ; \
            else \
                pip install --no-cache-dir --force renku==${RENKU_VERSION} ; \
            fi \
        fi \
    fi
#             End Renku install section                #
########################################################

FROM renku/renkulab-r:4.3.1-0.25.0

# This will fix installing of units, and also prevent similar issues for sf, xml2, httr, and others.
USER root
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libxml2-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*


# install the R dependencies
COPY install.R ./
RUN R -f install.R

COPY . .

COPY --from=builder ${HOME}/.renku/venv ${HOME}/.renku/venv
