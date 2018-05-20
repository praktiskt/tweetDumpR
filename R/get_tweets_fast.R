#' @title Twitter-data retrieval (fast)
#' @description This function grabs as much data as it can as quickly as it can. This is the fast version of the function which parses the entire tweet page in one go. We are able to do this by ignoring uncertain bits of a tweet. This means skipping things like hashtag extraction, image url extraction and retweet-history.
#' @export
#' @param username The name of the twitter follower you wish to lookup.
#' @param pages The number of tweet pages you want to retrieve. About 20 tweets per page.
#' @return Returns a tibble containing tweet-data for the specified used.
#' @examples
#' get_tweets_fast(username = 'hadleywickham', pages = 5)
get_tweets_fast = function(username, pages = 1) {

  suppressMessages({
    require(httr)
    require(dplyr)
    require(magrittr)
    require(tidyr)
    require(tibble)
    require(rvest)
    require(xml2)
  })

  org_pages = pages

  # To store the results.
  dataset = list()

  # Url to target.
  url = stringr::str_interp('https://twitter.com/i/profiles/show/${username}/timeline/tweets?include_available_features=1&include_entities=1&include_new_items_bar=true&lang=en')

  # Initial tweet collection (latest tweets)
  get = GET(
    url = url,
    add_headers(
      "Accept" = 'application/json, text/javascript, */*; q=0.01',
      "Referer" = stringr::str_interp('https://twitter.com/${username}'),
      "User-Agent" = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/603.3.8 (KHTML, like Gecko) Version/10.1.2 Safari/603.3.8',
      "X-Twitter-Active-User" = 'yes',
      "X-Requested-With" = 'XMLHttpRequest'
    )
  ) %>%
    extract2("content") %>%
    rawToChar() %>%
    jsonlite::fromJSON()

  # Extract list of tweets (about 20)
  tweets = get %>%
    extract2("items_html") %>%
    read_html()

  # While we have things to process.
  while (T) {

    # If we're done, break loop and return.
    if(pages == 0) break

    tweet = rvest::html_nodes(tweets, ".js-stream-tweet")

    tweet_text = tweet %>%
      rvest::html_nodes(".js-tweet-text") %>%
      rvest::html_text()

    tweet_id = tweet %>%
      rvest::html_nodes(".tweet-timestamp") %>%
      rvest::html_attr("data-conversation-id") %>%
      as.numeric()

    tweet_timestamp = tweet %>%
      rvest::html_nodes("._timestamp") %>%
      rvest::html_attr("data-time") %>%
      as.numeric() %>%
      as.POSIXct(origin = "1970-01-01")

    tweet_interactions = tweet %>%
      rvest::html_nodes(".ProfileTweet-actionCountForAria") %>%
      rvest::html_text() %>%
      as.tibble() %>%
      mutate(type = rep(c("replies", "retweets", "likes"), length(tweet_id)),
             value = stringr::str_replace_all(value, "\\D", "") %>%
               as.numeric(),
             rn = row_number()) %>%
      spread(type, value)

    tweet_likes = tweet_interactions$likes %>%
      na.omit() %>%
      as.vector()

    tweet_retweets = tweet_interactions$retweets %>%
      na.omit() %>%
      as.vector()

    tweet_replies = tweet_interactions$replies %>%
      na.omit() %>%
      as.vector()

    # Store parsed tweets in this list.
    dataset[[pages]] = tibble(
      id = tweet_id,
      timestamp = tweet_timestamp,
      text = tweet_text,
      likes = tweet_likes,
      retweets = tweet_retweets,
      replies = tweet_replies
    ) %>%
      mutate(
        timestamp = as.POSIXct(timestamp, origin = "1970-01-01")
      )

    # Find out where to start (minimum tweet id for this iteration)
    min_id = get$min_position

    # Get next tweets (start from min of previous page).
    get = GET(
      url = url,
      add_headers(
        "Accept" = 'application/json, text/javascript, */*; q=0.01',
        "Referer" = stringr::str_interp('https://twitter.com/${username}'),
        "User-Agent" = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/603.3.8 (KHTML, like Gecko) Version/10.1.2 Safari/603.3.8',
        "X-Twitter-Active-User" = 'yes',
        "X-Requested-With" = 'XMLHttpRequest'
      ),
      query = list(max_position = min_id)
    ) %>%
      extract2("content") %>%
      rawToChar() %>%
      jsonlite::fromJSON()

    # Check if there's more to get. 
    if(get$has_more_items){
     tweets = get %>%
        extract2("items_html") %>%
       read_html()
    } else {
      warning(paste("Stopped before last page. Unable to retrieve more tweets for user", username, "from page", org_pages-pages+1))
      break
    }

    paste0("Finished page ", org_pages-pages+1, ". Proceeding to next.") %>%
      print()

    pages = pages - 1

  } #while

  # Bind dataset-list and return as tibble.
  return(dataset %>%
           data.table::rbindlist() %>%
           as.tibble())

}
