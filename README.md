sentibot
=====
### Developed by Bastien Clément, Adriano Ruberto, Loïc Serafin, Henrik Akesson

## A Slackbot that keeps track of your emotions.


An OTP application developed in Erlang with rebar3

Required
-----
[Erlang 19](https://www.erlang.org/)
[Rebar3](https://github.com/erlang/rebar3)


Build instructions
-----
Simply go in the project directory and run the command 

    $ rebar3 compile

Launch instructions
-----
To run the program, use the command

    $ rebar3 shell --apps=sentibot

Commands
-----
After being invited in a channel, the bot will recognize users emotions when they say a phrase of the type

    > I am [emotion]

The bot will then respond the user's pseudonym followed be the corresponding emoji. 
The bot will also list all emotions of users within a channel after saying:

    > whasup
