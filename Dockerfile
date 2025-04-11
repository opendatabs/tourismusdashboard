########################################################
#        Renku install section - do not edit           #

FROM renku/renkulab-r:4.3.1-0.25.0 as builder

ARG RENKU_VERSION=2.9.4

# Install renku from pypi or from github if a dev version
RUN if [ -n "$RENKU_VERSION" ] ; then \
        source .renku/venv/bin/activate ; \
        currentversion=$(renku --version) ; \
        if [ "$RENKU_VERSION" != "$currentversion" ] ; then \
            pip uninstall renku -y ; \
            gitversion=$(echo "$RENKU_VERSION" | sed -n "s/^[[:digit:]]\+\.[[:digit:]]\+\.[[:digit:]]\+\(rc[[:digit:]]\+\)*\(\.dev[[:digit:]]\+\)*\(+g\([a-f0-9]\+\)\)*\(+dirty\)*$/\4/p") ; \
            if [ -n "$gitversion" ] ; then \
                pip install --no-cache-dir --force "git+https://github.com/SwissDataScienceCenter/renku-python.git@$gitversion" ;\
            else \
                pip install --no-cache-dir --force renku==${RENKU_VERSION} ;\
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

## Uses packages as at 01/04/2025
RUN echo "r <- getOption('repos'); \
	  r['CRAN'] <- 'https://packagemanager.rstudio.com/cran/__linux__/focal/2025-04-01'; \
	  options(repos = r);" > ~/.Rprofile

COPY install.R ./
RUN R -f install.R

COPY --from=builder ${HOME}/.renku/venv ${HOME}/.renku/venv
