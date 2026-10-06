# syntax=docker/dockerfile:1
#
# The image behind https://notebooks.buddhilw.com/: nginx serving the static
# build. The build context is the built site (history-of-math/target/hom);
# the nginx config comes in through a named context:
#
#   docker build -f deploy/Dockerfile --build-context conf=deploy history-of-math/target/hom

FROM nginxinc/nginx-unprivileged:1.29-alpine

COPY --from=conf nginx.conf /etc/nginx/conf.d/default.conf
COPY . /usr/share/nginx/html

USER 101

EXPOSE 8080

HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD wget -q -O /dev/null http://127.0.0.1:8080/healthz || exit 1
