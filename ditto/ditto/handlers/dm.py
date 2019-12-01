from ditto.asynchronous import asynchronously

@asynchronously
def respond_to_dm(client, event):
    if event.get("username") == "ditto":
        return {}

    text = event["text"]
    message = f"ditto! ({text})"
    channel = event["channel"]
    client.post_message(channel, message)
    return {}