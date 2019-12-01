import threading

def asynchronously(callback):
    def _wrapped(*args, **kwargs):
        thread = threading.Thread(target=callback, args=args, kwargs=kwargs, daemon=True)
        thread.start()

        return {}

    return _wrapped