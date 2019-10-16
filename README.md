# Markov

A previously-simple project demonstrating an implementation of Markov chains.  A Markov chain is a probabilistic state machine; if you tell it where you are, it will tell you where you might go.

This is a general idea which can be used (for example) for predictive text.  This is what is used in Reddit's r/subredditsimulator.

## Requirements
* `libssl-dev` or similar (depending on platform)
* a `cassandra` database with `markov_names` and `markov_data` column families

## Planned work
* slack bot client to markov server
* twitter bot client to markov server
* very simple frontend web app client to markov server
