import os
import slack

class SlackClient:
    def __init__(self):
        token = os.environ["OAUTH"]
        self._client = slack.WebClient(token=token)

    def post_message(self, channel_id, text):
        self._client.chat_postMessage(channel=channel_id, text=text)

    def scrape_message(self, channel_id, user_id):
        pass