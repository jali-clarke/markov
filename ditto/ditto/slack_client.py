import os
import slack

class SlackClient:
    def __init__(self):
        token = os.environ["OAUTH"]
        bot_token = os.environ["BOT_USER_OAUTH"]
        bot_client = slack.WebClient(token=bot_token)

        self._client = slack.WebClient(token=token)
        self._self_user_id = bot_client.auth_test()["user_id"]

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
            if _message_is_valid(user_id, self._self_user_id, message_body):
                yield message_body["text"]

        if response["has_more"]:
            new_start_ts = messages_chunk[-1]["ts"]
            yield from self._scrape_messages(channel_id, user_id, new_start_ts)

def _message_is_valid(user_id, ditto_user_id, message_body):
    if message_body["type"] != "message":
        return False

    is_correct_user = message_body.get("user") == user_id
    is_not_for_ditto = not message_body["text"].lstrip().startswith(f"<@{ditto_user_id}>")

    return is_correct_user and is_not_for_ditto