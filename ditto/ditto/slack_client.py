import os
import slack

class SlackClient:
    def __init__(self):
        token = os.environ["OAUTH"]
        self._client = slack.WebClient(token=token)

    def post_message(self, channel_id, text):
        self._client.chat_postMessage(channel=channel_id, text=text)

    def scrape_messages(self, channel_id, user_id):
        yield from self._scrape_messages(channel_id, user_id, "0")

    def _scrape_messages(self, channel_id, user_id, start_ts):
        try:
            response = self._client.channels_history(channel=channel_id, count="1000", oldest=start_ts)
        except:
            response = self._client.groups_history(channel=channel_id, count="1000", oldest=start_ts)

        messages_chunk = response["messages"]

        if len(messages_chunk) == 0:
            return

        for message_body in messages_chunk:
            if message_body.get("user") == user_id and message_body["type"] == "message":
                yield message_body["text"]

        if response["has_more"]:
            new_start_ts = messages_chunk[-1]["ts"]
            yield from self._scrape_messages(channel_id, user_id, new_start_ts)
