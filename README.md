# Markov

A previously-simple project demonstrating an implementation of Markov chains.  A Markov chain is a probabilistic state machine; if you tell it where you are, it will tell you where you might go.

This is a general idea which can be used (for example) for predictive text.  This is what is used in Reddit's r/subredditsimulator.  Currently exists as a web server - not much more than an in-memory database.

## Planned work
* add k8s manifest
* slack bot client to markov server
* decouple storage (currently uses an in-memory database)
* microservices!