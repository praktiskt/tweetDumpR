# tweetDumpR
An R-package for collecting a lot of tweets really fast. Without limits.

## General

This package was born out of frustration from working with the official Twitter API limits. This package bypasses these limits completely by using not the official API, but rather the Twitter frontend API to generate and parse JSONs.

While there are no limits of requests per time-window, the Twitter frontend API seem to stop sending data at page 40, or about 800 tweets.

## Installation

Using devtools, simply run the following: 

`devtools::install_github("magnusfurugard/tweetDumpR")`

## Usage

There are three functions in this package.

`get_user_metadata(username = "hadleywickham")`
* Get user metadata, including join date, screen name, number of followers, number of tweets and much more.

`get_tweets_fast(username = "hadleywickham", pages = 5)`

* Gets the first 5 pages of tweets for @hadleywickham (5 pages equals about 100 tweets). 
* Does not parse all retrieved data, making it faster than a full parse.

`get_tweets_full(username = "hadleywickham", pages = 5)`

* Gets the first 5 pages of tweets for @hadleywickham (5 pages equals about 100 tweets).
* Parses as much data as possible, including retweet-history for retrieved tweets.

## Credits

The method of scraping the Twitter frontend API was found in this Python module: https://github.com/kennethreitz/twitter-scraper. 
