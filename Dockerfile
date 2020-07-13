FROM virtualstaticvoid/heroku-docker-r:shiny
ENV PORT=8080
CMD ["/usr/bin/R", "--no-save", "--gui-none", "-f /app/run.R"]

## Emacs, make this -*- mode: sh; -*-

## Start from xenial
FROM rocker/r-apt:xenial

## This handle reaches Carl and Dirk
MAINTAINER "Carl Boettiger and Dirk Eddelbuettel" rocker-maintainers@eddelbuettel.com

## Update and install rstan
RUN apt-get update && apt-get install -y --no-install-recommends r-cran-rstan

## Make R the default
CMD ["R"]