FROM jackfirth/racket:7.3-full

COPY *.rkt /app/
WORKDIR /app

ENTRYPOINT ["racket", "server.rkt"]