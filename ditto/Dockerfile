FROM python:3.7-alpine

RUN apk add --update build-base libffi-dev

WORKDIR /app
COPY ./requirements.txt ./requirements.txt
RUN python -m pip install -r requirements.txt

COPY ./ditto ./ditto

ENTRYPOINT ["python", "-m"]
CMD ["ditto"]