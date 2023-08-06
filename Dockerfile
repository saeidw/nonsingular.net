FROM ubuntu:22.04 AS build

RUN apt-get update && apt-get -yy install ca-certificates curl
RUN curl -L 'https://github.com/gohugoio/hugo/releases/download/v0.116.1/hugo_extended_0.116.1_linux-amd64.tar.gz' -o hugo.tar.gz
RUN tar zxvf hugo.tar.gz hugo && mv hugo /usr/local/bin

COPY . /src
WORKDIR /src
RUN hugo

FROM nginx:1.25.1

COPY --from=build /src/public /usr/share/nginx/html

