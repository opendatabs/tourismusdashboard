########################################################
#        Renku install section                         #

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

WORKDIR ${HOME}

USER root
# Install dependencies without conflict
RUN apt-get update && apt-get install -y \
    curl gnupg unixodbc unixodbc-dev apt-transport-https software-properties-common

# Add Microsoft SQL Server ODBC repo (Ubuntu 20.04)
RUN curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add - && \
    curl https://packages.microsoft.com/config/ubuntu/20.04/prod.list > /etc/apt/sources.list.d/mssql-release.list && \
    apt-get update

# Download .deb packages first
RUN apt-get download odbcinst1debian2 odbcinst

# Install odbcinst with --force-overwrite to avoid file conflict
RUN dpkg -i --force-overwrite odbcinst*.deb || apt-get -f install -y

# Now safely install msodbcsql18
RUN ACCEPT_EULA=Y DEBIAN_FRONTEND=noninteractive apt-get install -y msodbcsql18 && \
    apt-get clean && rm -rf /var/lib/apt/lists/*
 
# This will fix installing of units, and also prevent similar issues for sf, xml2, httr, and others.
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libxml2-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

USER ${NB_USER}

## Uses packages as at 01/04/2025
RUN echo "r <- getOption('repos'); \
	  r['CRAN'] <- 'https://packagemanager.rstudio.com/cran/__linux__/focal/2025-04-01'; \
	  options(repos = r);" > ~/.Rprofile

COPY install.R ${HOME}/
RUN R -f ${HOME}/install.R

COPY . ${HOME}/

COPY --from=builder ${HOME}/.renku/venv ${HOME}/.renku/venv
