import pprint

from ditto.slack_client import SlackClient

from ditto.handlers.challenge import handle_challenge
from ditto.handlers.dm import respond_to_dm

_client = SlackClient()

def _pretty(value):
    return pprint.pformat(value, indent=2)

def dispatch(logger, event_payload):
    logger.info("REQUEST -----")
    logger.info(_pretty(event_payload))
    event_type = event_payload.get("type")

    if event_type == "url_verification":
        logger.info("HANDLING VERIFICATION CHALLENGE")
        return handle_challenge(event_payload)
    if event_type == "event_callback":
        logger.info("HANDLING EVENT")
        event_internal = event_payload["event"]
        event_subtype = event_internal.get("type")

        if event_subtype == "message":
            logger.info("RESPONDING TO DM")
            return respond_to_dm(_client, event_internal)
        else:
            logger.info("UNKNOWN EVENT SUBTYPE")
            return {}
    else:
        logger.info("UNKNOWN REQUEST TYPE")
        return {}