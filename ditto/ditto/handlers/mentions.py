import re
from requests import HTTPError

from ditto.asynchronous import asynchronously
from ditto.markov import Markov

_markov = Markov()

def _command_regex(command):
    return re.compile(f"^\s*<@.+>\s+{command}\s+<@(.+)>\s*$")

_learn_regex = _command_regex("learn")
_imitate_regex = _command_regex("imitate")

@asynchronously
def respond_to_mention(client, event):
    if event.get("username") == "ditto":
        return {}

    text = event["text"]
    channel_id = event["channel"]

    learn_match = _learn_regex.match(text)
    if learn_match:
        user_id = learn_match.groups()[0]
        _learn_from_user(client, channel_id, user_id)
        return {}

    imitate_match = _imitate_regex.match(text)
    if imitate_match:
        user_id = imitate_match.groups()[0]
        _imitate_user(client, channel_id, user_id)
        return {}

    client.post_message(channel_id, f"ditto! (i don't understand this: {text})")
    return {}

def _learn_from_user(client, channel_id, user_id):
    try:
        client.post_message(channel_id, f"ditto! (learning from user <@{user_id}>)")
        messages_generator = client.scrape_messages(channel_id, user_id)
        _markov.train(channel_id, user_id, list(messages_generator))
        client.post_message(channel_id, f"ditto! (done learning from user <@{user_id}>)")
    except HTTPError:
        client.post_message(channel_id, f"ditto! (could not learn from user <@{user_id}>)")

def _imitate_user(client, channel_id, user_id):
    try:
        message = _markov.message(channel_id, user_id)
        client.post_message(channel_id, f"(as <@{user_id}>) {message}")
    except HTTPError:
        client.post_message(channel_id, f"ditto! (i haven't learned from <@{user_id}> yet)")