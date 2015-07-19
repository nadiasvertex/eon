FROM python:3.5
MAINTAINER Christopher Nelson <nadiasvertex@gmail.com>
LABEL version=0.1
LABEL description="eon data server"

RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app

COPY requirements.txt /usr/src/app/
RUN pip install --no-cache-dir -r requirements.txt

COPY . /usr/src/app
CMD [ "python3", "-m", "eon" ]