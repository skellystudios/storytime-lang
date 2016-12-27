# Storytime
Interpreted programming language in haskell that can be disguised to look like normal english 

First, an example:

    A republican hears the democrat say nothing and the democrat hears the republican say wise things. Campaigning is where someone says promises they can't keep and the democrat hears them and they listen to the republican and the republican hears the democrat. I always campaign.

StoryTime is a language whose aim is to be easily writable by a human programmer, but also easily mistook for valid semantic English. The idea came from the concept of steganography (security by obscurity, often hiding a secret inside something innocuous, not to be confused with stenography, which is the art of being able to write something so quickly it’s instantly unreadable).

I actually had three criteria in mind while making this:

1. It should be possible to construct programs by hand
2. The program should look and read like valid English
3. The program should surprise and delight (shock and awe) even those who know it’s a program, by being non-obvious how the program’s function came from its structure
4. That last rule was pretty useful in making decisions about how to structure the language, because it ruled out doing things in the way other natural language-esque esolangs do, e.g. ~English.

Storytime is currently an interpreted language built in Haskell with Parsec for parsing. 

Check out my blog on it here: http://skelly.io/technology/2016/02/12/storytime-lang.html
