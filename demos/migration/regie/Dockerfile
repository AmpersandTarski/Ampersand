# The director of the migration demonstrator: a page with three buttons, and the hands that work Docker.
# Build it from the directory of the demonstrator: docker build --build-arg DEMO=kurk -f regie/Dockerfile .
FROM docker:27-cli
RUN apk add --no-cache python3
ARG DEMO
COPY ${DEMO}/deploy/system.json ${DEMO}/demo.json /demo/
COPY ${DEMO}/deploy/db-init/01-grant.sql /demo/grants.sql
COPY regie/regie.py regie/page.html /regie/
ENV DEMO=${DEMO} PYTHONUNBUFFERED=1
EXPOSE 8000
ENTRYPOINT ["python3", "/regie/regie.py"]
