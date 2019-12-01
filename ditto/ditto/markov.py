import os
import requests

class Markov:
    def __init__(self):
        self._markov_api_url = os.environ["MARKOV_URL"]

    def train(self, channel_id, user_id, messages):
        key = self._key(channel_id, user_id)
        all_markovs_route = f"{self._markov_api_url}/markovMaps"
        markov_route = f"{all_markovs_route}/{key}"

        requests.delete(markov_route).raise_for_status()
        requests.post(all_markovs_route, json={"name": key}).raise_for_status()
        requests.put(markov_route, json={"messages": messages}).raise_for_status()

    def message(self, channel_id, user_id):
        key = self._key(channel_id, user_id)
        message_route = f"{self._markov_api_url}/markovMaps/{key}/message"
        
        response = requests.get(message_route)
        response.raise_for_status()

        return response.json()["message"]

    def _key(self, channel_id, user_id):
        return f"{channel_id}_{user_id}"