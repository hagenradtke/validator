# largely based on rocker r-base image

FROM ubuntu:bionic-20181204

MAINTAINER "Hagen Radtke <hagen.radtke@io-warnemuende>"
# Add user to 'staff' group, granting them write privileges to /usr/local/lib/R/site.library
RUN useradd docker \
	&& mkdir /home/docker \
	&& chown docker:docker /home/docker \
	&& addgroup docker staff

RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		ed \
		less \
		locales \
		vim-tiny \
		wget \
		ca-certificates \
		fonts-texgyre \
        gnupg2 \
		libnetcdf-dev \
        dirmngr \
		libxml2-dev \
		libudunits2-dev \
		libcurl4-gnutls-dev \
        software-properties-common \
        apt-transport-https \
		libssl-dev \
&& rm -rf /var/lib/apt/lists/*
# Configure default locale, see https://github.com/rocker-org/rocker/issues/19

RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
	&& locale-gen en_US.utf8 \
	&& /usr/sbin/update-locale LANG=en_US.UTF-8

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8

#RUN add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/'
ENV DEBIAN_FRONTEND=noninteractive  

# Now install R and littler, and create a link for littler in /usr/local/bin
# Also set a default CRAN repo, and make sure littler knows about it too
## Now install R and littler, and create a link for littler in /usr/local/bin
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/'

RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		littler \
        r-cran-littler \
		r-base \
		r-base-dev \
        r-recommended \
		gdebi-core \
		libgit2-dev \
		libgeos-dev \
		libssh2-1-dev \
		libcairo2-dev \
	&& ln -s /usr/lib/R/site-library/littler/examples/install.r /usr/local/bin/install.r \
	&& ln -s /usr/lib/R/site-library/littler/examples/install2.r /usr/local/bin/install2.r \
	&& ln -s /usr/lib/R/site-library/littler/examples/installGithub.r /usr/local/bin/installGithub.r \
	&& ln -s /usr/lib/R/site-library/littler/examples/testInstalled.r /usr/local/bin/testInstalled.r \
	&& install.r docopt \
	&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
	&& rm -rf /var/lib/apt/lists/*

RUN apt-key adv --recv-keys --keyserver hkp://keyserver.ubuntu.com:80 0xF1656F24C74CD1D8
RUN add-apt-repository 'deb [arch=amd64,arm64,ppc64el] http://mirror.wtnet.de/mariadb/repo/10.3/ubuntu bionic main'
RUN apt-get update && apt-get install -fy --no-install-recommends mariadb-server mariadb-client libmysqlclient-dev 

# Download and install shiny server
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    . /etc/environment

RUN R -e "install.packages('shiny',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/
RUN chown shiny:shiny /var/lib/shiny-server
RUN R -e "install.packages('RMariaDB',dependencies=TRUE, repos='http://cran.rstudio.com/')"

# Copy configuration files into the Docker image
RUN mkdir $HOME/app 
COPY app $HOME/app/.
COPY my.cnf /etc/.


# Copy R files
RUN service mysql start
RUN /etc/init.d/./mysql start \
	&& mysql -uroot -e "CREATE DATABASE measurements;"\
	&& mysql -uroot measurements < /app/mysql_example/measurements.sql \
	&& mysql -uroot -e "CREATE USER 'validator'@'localhost' IDENTIFIED BY 'test';USE measurements; GRANT SELECT ON * TO 'validator'@'localhost';" 

RUN R -e "install.packages('ggplot2',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('RCurl',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('RNetCDF',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotrix',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('mgcv',dependencies=TRUE, repos='http://cran.rstudio.com/')"

# Make the ShinyApp available at port 8888
EXPOSE 3838

COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod +x /usr/bin/shiny-server.sh
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
CMD chmod -R 777 /app/* && /etc/init.d/./mysql start && /usr/bin/./shiny-server.sh
# Copy further configuration files into the Docker image
#COPY shiny-server.sh /usr/bin/shiny-server.sh
##COPY run_app.R $HOME/.
#CMD  /etc/init.d/./mysql start && Rscript /run_app.R