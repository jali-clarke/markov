from flask import Flask, jsonify, request
import logging

from ditto.dispatcher import dispatch

logging.basicConfig(level=logging.INFO)
app = Flask(__name__)

@app.route("/", methods=["POST"])
def root():
    request_payload = request.json
    response_payload = dispatch(app.logger, request_payload)
    return jsonify(response_payload), 200
